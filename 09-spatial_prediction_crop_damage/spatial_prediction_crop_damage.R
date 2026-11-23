## ----------------------------------------------------------------------------------------
if (!require("terra")) install.packages("terra")
if (!require("openxlsx")) install.packages("openxlsx")
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyr")) install.packages("tidyr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("stringr")) install.packages("stringr")
if (!require("ranger")) install.packages("ranger")
if (!require("pdp")) install.packages("pdp")

library(terra)
library(openxlsx)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(ranger)
library(pdp)


## ----------------------------------------------------------------------------------------
d_eth = as.data.frame(read.xlsx("Data/Kebede et al. 2019a.xlsx", sheet = "Data"))


## ----------------------------------------------------------------------------------------
fix_coord = function(x, int_digits) {
  n = nchar(as.character(as.integer(x)))
  x / 10^(n - int_digits)
}

d_eth = d_eth %>%
  mutate(
    Lat = fix_coord(Lat, int_digits = 1),
    Lon = fix_coord(Lon, int_digits = 2))


## ----------------------------------------------------------------------------------------
unique_plots = d_eth %>%
  distinct(PlotID, Lat, Lon)

nrow(unique_plots)


## ----------------------------------------------------------------------------------------
ethiopia_pts = data.frame(
  point_id = as.character(unique_plots$PlotID),
  lat = unique_plots$Lat,
  lon = unique_plots$Lon)

ethiopia_pts2 = terra::vect(
  ethiopia_pts,
  geom = c("lon", "lat"),
  crs = "EPSG:4326")

plot(ethiopia_pts2, main = "Unique maize plot locations")


## ----------------------------------------------------------------------------------------
## 1. Landscape metrics from land-use raster
landscape_metrics = function(landuse_map, points, class_ids, buffers,
                              id_col = "id", crs_proj,
                              dist_only_ids = integer(0)) {

  lu_proj = terra::project(landuse_map, crs_proj, method = "near")
  pts_proj = terra::project(points, crs_proj)
  pt_xy = terra::crds(pts_proj)
  ids = as.data.frame(pts_proj)[[id_col]]

  # Distance to nearest land-use class
  all_dist_ids = c(class_ids, dist_only_ids)
  dist_df = data.frame(id = ids)

  for (cl in all_dist_ids) {
    cl_r = lu_proj == cl
    cl_r[cl_r == 0] = NA
    dist_r = terra::distance(cl_r)
    dist_df[[paste0("dist_class", cl)]] =
      terra::extract(dist_r, pt_xy)[, 1]
  }

  # Buffer-based metrics
  buffer_rows = vector("list", length(buffers))

  for (j in seq_along(buffers)) {
    b = buffers[j]
    message(format(Sys.time(), "%H:%M:%S"), "  Buffer ", b, " m started")

    buf = terra::buffer(pts_proj, width = b)
    pt_rows = vector("list", nrow(pts_proj))

    for (i in seq_len(nrow(pts_proj))) {
      if (i %% 50 == 0) message("Point ", i, " of ", nrow(pts_proj))

      lu_crop = terra::crop(lu_proj, buf[i, ], mask = TRUE)
      vals = terra::values(lu_crop)
      vals = vals[!is.na(vals)]
      total_n = length(vals)

      row = data.frame(
        id = ids[i],
        buffer = b,
        flag_empty_buffer = total_n == 0)

      if (total_n == 0) {
        pt_rows[[i]] = row
        next
      }

      for (cl in class_ids) {
        row[[paste0("prop_class", cl)]] = sum(vals == cl) / total_n

        bin = lu_crop == cl
        bin[bin == 0] = NA

        if (sum(!is.na(terra::values(bin))) == 0) {
          row[[paste0("npatch_class", cl)]] = 0
        } else {
          patches_r = terra::patches(bin, directions = 8)
          pvals = terra::values(patches_r)
          row[[paste0("npatch_class", cl)]] =
            length(unique(pvals[!is.na(pvals)]))
        }
      }

      p = table(vals) / total_n
      p = p[p > 0]
      row$shannon = -sum(p * log(p))

      pt_rows[[i]] = row
    }

    buffer_rows[[j]] = do.call(rbind, pt_rows)
    message(format(Sys.time(), "%H:%M:%S"), "  Buffer ", b, " m completed")
  }

  buffer_metrics = do.call(rbind, buffer_rows)
  merge(buffer_metrics, dist_df, by = "id")
}

## 2. Metrics from vector layers
vector_layer_metrics = function(points_proj, buffers, id_col = "point_id",
                                 polygon_layers = list(),
                                 line_layers = list()) {

  ids = as.data.frame(points_proj)[[id_col]]

  nearest_dist = function(pts, layer) {
    vapply(
      seq_len(nrow(pts)),
      function(i) min(terra::distance(pts[i, ], layer)),
      numeric(1)
    )
  }

  dist_df = data.frame(id = ids)

  for (nm in names(polygon_layers)) {
    dist_df[[paste0("dist_", nm)]] =
      nearest_dist(points_proj, polygon_layers[[nm]])
  }

  for (nm in names(line_layers)) {
    dist_df[[paste0("dist_", nm)]] =
      nearest_dist(points_proj, line_layers[[nm]])
  }

  buffer_rows = vector("list", length(buffers))

  for (j in seq_along(buffers)) {
    b = buffers[j]
    buf = terra::buffer(points_proj, width = b)
    buf_area = terra::expanse(buf)
    pt_rows = vector("list", nrow(points_proj))

    for (i in seq_len(nrow(points_proj))) {
      row = data.frame(id = ids[i], buffer = b)

      for (nm in names(polygon_layers)) {
        inter = tryCatch(
          terra::intersect(buf[i, ], polygon_layers[[nm]]),
          error = function(e) NULL
        )

        row[[paste0("prop_", nm)]] =
          if (is.null(inter) || nrow(inter) == 0) {
            0
          } else {
            sum(terra::expanse(inter)) / buf_area[i]
          }
      }

      for (nm in names(line_layers)) {
        inter = tryCatch(
          terra::intersect(buf[i, ], line_layers[[nm]]),
          error = function(e) NULL
        )

        row[[paste0(nm, "_length_m")]] =
          if (is.null(inter) || nrow(inter) == 0) {
            0
          } else {
            sum(terra::perim(inter))
          }
      }

      pt_rows[[i]] = row
    }

    buffer_rows[[j]] = do.call(rbind, pt_rows)
  }

  buffer_metrics = do.call(rbind, buffer_rows)
  merge(buffer_metrics, dist_df, by = "id")
}

## 3. Generic landscape metrics pipeline
run_hwc_pipeline = function(points, id_col, detail_cols,
                             lulc, lulc_classes, crs_proj, buffers_m,
                             dist_only_classes = integer(0),
                             polygon_layer_paths = list(),
                             line_layer_paths = list(),
                             skip_crop = character(0),
                             margin_deg = 0.05,
                             output_path,
                             fill_na_km2 = NULL,
                             fill_na_value = NULL) {

  lu = lulc

  # Optional filling of large internal no-data areas
  if (!is.null(fill_na_km2) && !is.null(fill_na_value)) {
    lu = lu[[1]]
    pa = terra::patches(terra::ifel(is.na(lu), 1, NA), directions = 8)
    pm = terra::as.matrix(pa, wide = TRUE)

    edge_ids = unique(c(
      pm[1, ], pm[nrow(pm), ], pm[, 1], pm[, ncol(pm)]))

    area_by_patch = terra::zonal(
      terra::cellSize(lu, unit = "km"), pa, fun = "sum")

    big_ids = area_by_patch[area_by_patch[[2]] >= fill_na_km2, 1]
    fill_ids = setdiff(big_ids, edge_ids[!is.na(edge_ids)])

    message("Pipeline: filled ", length(fill_ids),
            " no-data area(s) with class ", fill_na_value)

    if (length(fill_ids) > 0) {
      lu = terra::cover(
        lu,
        terra::classify(pa, cbind(fill_ids, fill_na_value), others = NA))
    }
  }

  pts = terra::vect(points, geom = c("lon", "lat"), crs = "EPSG:4326")

  # Raster-based metrics
  raster_metrics = landscape_metrics(
    landuse_map = lu,
    points = pts,
    class_ids = lulc_classes,
    buffers = buffers_m,
    id_col = id_col,
    crs_proj = crs_proj,
    dist_only_ids = dist_only_classes)

  class_name_map = setNames(
    names(c(lulc_classes, dist_only_classes)),
    paste0("class", c(lulc_classes, dist_only_classes)))

  nms = names(raster_metrics)
  for (tag in names(class_name_map)) {
    nms = gsub(tag, class_name_map[[tag]], nms)
  }
  names(raster_metrics) = nms

  # Optional vector-based metrics
  have_vector_layers =
    length(polygon_layer_paths) > 0 ||
    length(line_layer_paths) > 0

  if (have_vector_layers) {
    pts_proj = terra::project(pts, crs_proj)

    study_ext_wgs = terra::ext(pts) + margin_deg
    study_ext_vect = terra::vect(study_ext_wgs, crs = "EPSG:4326")

    crop_to_study = function(layer_path) {
      layer = terra::vect(layer_path)
      ext_native = terra::project(study_ext_vect, terra::crs(layer))
      cropped = terra::crop(layer, ext_native)

      if (nrow(cropped) == 0) {
        stop("No features found in ", layer_path,
             ". Increase margin_deg or skip cropping this layer.")
      }

      cropped
    }

    polygon_layers = setNames(
      lapply(names(polygon_layer_paths), function(nm) {
        p = polygon_layer_paths[[nm]]
        lyr = if (nm %in% skip_crop) terra::vect(p) else crop_to_study(p)
        terra::makeValid(terra::project(lyr, crs_proj))
      }),
      names(polygon_layer_paths))

    line_layers = setNames(
      lapply(names(line_layer_paths), function(nm) {
        p = line_layer_paths[[nm]]
        lyr = if (nm %in% skip_crop) terra::vect(p) else crop_to_study(p)
        terra::project(lyr, crs_proj)
      }),
      names(line_layer_paths))

    vector_metrics = vector_layer_metrics(
      points_proj = pts_proj,
      buffers = buffers_m,
      id_col = id_col,
      polygon_layers = polygon_layers,
      line_layers = line_layers)

    long_metrics = merge(raster_metrics, vector_metrics,
                          by = c("id", "buffer"))
  } else {
    long_metrics = raster_metrics
  }

  # Reshape metrics to wide format
  dist_cols = grep("^dist_", names(long_metrics), value = TRUE)
  dist_wide = unique(long_metrics[, c("id", dist_cols)])
  names(dist_wide)[-1] = paste0(names(dist_wide)[-1], "_m")

  buf_cols = setdiff(
    names(long_metrics),
    c("id", "buffer", dist_cols, "flag_empty_buffer"))

  wide_list = lapply(buffers_m, function(b) {
    sub = long_metrics[long_metrics$buffer == b, c("id", buf_cols)]
    names(sub)[-1] = paste0(names(sub)[-1], "_", b, "m")
    sub
  })

  buf_wide = Reduce(
    function(x, y) merge(x, y, by = "id"),
    wide_list)

  wide = merge(dist_wide, buf_wide, by = "id")
  names(wide)[names(wide) == "id"] = id_col

  names(wide) = gsub(
    "^npatch_(.+)_([0-9]+m)$",
    "n_\\1_patches_\\2",
    names(wide))

  if (length(detail_cols) > 0) {
    wide = merge(points[, c(id_col, detail_cols)], wide, by = id_col)
  }

  write.csv(wide, output_path, row.names = FALSE)
  wide
}


## ----------------------------------------------------------------------------------------
lulc = terra::rast("Data/Spatial/Raw/ethiopia_dynamicworld_2015_updated.tiff")

ethiopia_pts2 = terra::project(ethiopia_pts2, terra::crs(lulc))

plot(
  lulc,
  main = "Dynamic World land cover (2015)",
  col = c(
    "steelblue", "darkgreen", "palegreen3", "turquoise3",
    "gold", "olivedrab3", "red3", "tan", "white"))

terra::points(ethiopia_pts2, pch = 20, cex = 2, col = "black")


## ----------------------------------------------------------------------------------------
dynamicworld_classes = c(
  water = 0,
  trees = 1,
  grass = 2,
  flooded_vegetation = 3,
  crops = 4,
  shrub_and_scrub = 5,
  bare = 7)


## ----------------------------------------------------------------------------------------
crs_ethiopia = "EPSG:32637"

buffers_ethiopia = c(
  100, 500, 1000, 1500, 2000, 5000)


## ----------------------------------------------------------------------------------------
ethiopia_wide = run_hwc_pipeline(
  points = ethiopia_pts,
  id_col = "point_id",
  detail_cols = character(0),
  lulc = lulc,
  lulc_classes = dynamicworld_classes,
  dist_only_classes = integer(0),
  crs_proj = crs_ethiopia,
  buffers_m = buffers_ethiopia,
  polygon_layer_paths = list(),
  line_layer_paths = list(),
  output_path = "Data/ethiopia_landscape_metrics_wide.csv"
)

head(ethiopia_wide)


## ----------------------------------------------------------------------------------------
metrics = ethiopia_wide

buffer_levels = c(
  "100m", "500m", "1000m", "1500m", "2000m", "5000m")

metrics_long = metrics %>%
  pivot_longer(
    cols = -point_id,
    names_to = "raw_metric",
    values_to = "value") %>%
  mutate(
    buffer = str_extract(
      raw_metric,
      "(100|500|1000|1500|2000|5000)m$"),
    buffer = if_else(
      is.na(buffer),
      "Distance (m)",
      buffer),
    buffer = factor(
      buffer,
      levels = c(buffer_levels, "Distance (m)")),
    metric_type = case_when(
      str_starts(raw_metric, "prop_") ~ "Proportion",
      str_starts(raw_metric, "n_") &
        str_detect(raw_metric, "_patches_") ~ "Patch Count",
      str_starts(raw_metric, "shannon") ~ "Shannon Diversity",
      str_starts(raw_metric, "dist_") ~ "Distance to Class (m)",
      TRUE ~ "Other"),
    landcover = raw_metric %>%
      str_remove("^(prop_|n_|dist_)") %>%
      str_remove("_patches(_(100|500|1000|1500|2000|5000)m)?$") %>%
      str_remove("_m$") %>%
      str_remove("_(100|500|1000|1500|2000|5000)m$") %>%
      if_else(metric_type == "Shannon Diversity", "All classes", .))


## ----------------------------------------------------------------------------------------
metrics_long %>%
  filter(metric_type %in% c("Proportion", "Patch Count")) %>%
  ggplot(aes(x = landcover, y = value, fill = buffer)) +
  geom_boxplot(outlier.size = 0.8, alpha = 0.85) +
  facet_wrap(~ metric_type, scales = "free_y", ncol = 1) +
  labs(
    x = "Land-cover class",
    y = "Metric value",
    fill = "Buffer distance") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1))


## ----------------------------------------------------------------------------------------
metrics_long %>%
  filter(metric_type == "Distance to Class (m)") %>%
  ggplot(
    aes(
      x = reorder(landcover, value, median, na.rm = TRUE),
      y = value)) +
  geom_boxplot(
    fill = "#4E79A7",
    outlier.size = 0.8,
    alpha = 0.85) +
  labs(
    x = "Land-cover class",
    y = "Distance (m)") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1))


## ----------------------------------------------------------------------------------------
metrics_long %>%
  filter(metric_type == "Shannon Diversity") %>%
  ggplot(aes(x = buffer, y = value)) +
  geom_boxplot(
    fill = "#4E79A7",
    outlier.size = 0.8,
    alpha = 0.85) +
  labs(
    x = "Buffer distance",
    y = "Shannon diversity index") +
  theme_bw()


## ----------------------------------------------------------------------------------------
d_eth$PlotID = as.character(d_eth$PlotID)

d_eth_metrics = d_eth %>%
  left_join(
    ethiopia_wide,
    by = c("PlotID" = "point_id"))


## ----------------------------------------------------------------------------------------
d_eth_metrics %>%
  filter(LengthTunnelling >= 100) %>%
  select(PlotID, Year, LengthTunnelling)


## ----------------------------------------------------------------------------------------
response_variable = "LengthTunnelling"

field_predictors = c(
  "PlantingDate",
  "PlantingDensity",
  "CroppingSystem",
  "MaizeVariety",
  "DAP",
  "Urea",
  "NInput",
  "MC_0_30",
  "MN_0_30",
  "MP_0_30",
  "pH_top_soil")

# Check that all requested field-level variables are present in the workbook.
missing_field_predictors = setdiff(field_predictors, names(d_eth))
if (length(missing_field_predictors) > 0) {
  stop("Missing field-level variables: ",
       paste(missing_field_predictors, collapse = ", "))
}

landscape_predictors = setdiff(names(ethiopia_wide), "point_id")
candidate_predictors = c("Year", field_predictors, landscape_predictors)

model_data = d_eth_metrics %>%
  filter(
    !is.na(.data[[response_variable]]),
    .data[[response_variable]] < 100) %>%
  select(
    PlotID,
    all_of(response_variable),
    all_of(candidate_predictors))

# Convert categorical variables to factors for ranger.
model_data = model_data %>%
  mutate(
    across(where(is.character), as.factor),
    Year = as.factor(Year))

# Remove predictors with no variation or entirely missing values.
candidate_predictors = candidate_predictors[
  vapply(
    model_data[, candidate_predictors, drop = FALSE],
    function(x) length(unique(x[!is.na(x)])) > 1,
    logical(1))]

model_data = model_data %>%
  select(
    PlotID,
    all_of(response_variable),
    all_of(candidate_predictors))


## ----------------------------------------------------------------------------------------
model_data_complete = model_data %>%
  filter(
    if_all(
      all_of(c(response_variable, candidate_predictors)),
      ~ !is.na(.x)))


## ----------------------------------------------------------------------------------------
# Select numerical predictors only.
numeric_predictors = candidate_predictors[
  vapply(
    model_data_complete[, candidate_predictors, drop = FALSE],
    is.numeric,
    logical(1))]

# Calculate Spearman correlation matrix
cor_matrix = stats::cor(
  model_data_complete[, numeric_predictors, drop = FALSE],
  method = "spearman",
  use = "complete.obs")

# Identify pairs of predictors with strong correlations
correlation_threshold = 0.7

# Identify highly correlated predictors
correlated_predictors_to_remove = caret::findCorrelation(
  cor_matrix,
  cutoff = correlation_threshold,
  names = TRUE,
  exact = TRUE)

# Update candidate predictors
candidate_predictors = setdiff(
  candidate_predictors,
  correlated_predictors_to_remove)

# Update model dataset
model_data_complete = model_data_complete %>%
  dplyr::select(-dplyr::all_of(correlated_predictors_to_remove))

# Display removed predictors
correlated_predictors_to_remove


## ----------------------------------------------------------------------------------------
set.seed(123)

plot_ids = unique(model_data_complete$PlotID)
model_formula = reformulate(
  candidate_predictors,
  response = response_variable)

cv_predictions = data.frame()

for (i in seq_along(plot_ids)) {

  test_plot = plot_ids[i]
  train_data = model_data_complete %>%
    filter(PlotID != test_plot)
  test_data = model_data_complete %>%
    filter(PlotID == test_plot)
  rf_cv = ranger(
    formula = model_formula,
    data = train_data %>% select(-PlotID),
    num.trees = 1000,
    mtry = max(1, floor(sqrt(length(candidate_predictors)))),
    min.node.size = 5,
    importance = "none",
    seed = 123)

  pred = predict(
    rf_cv,
    data = test_data %>% select(-PlotID, -all_of(response_variable)))$predictions

  cv_predictions = rbind(
    cv_predictions,
    data.frame(
      PlotID = test_data$PlotID,
      observed = test_data[[response_variable]],
      predicted = pred))
}


## ----------------------------------------------------------------------------------------
RMSE = sqrt(mean(
  (cv_predictions$observed - cv_predictions$predicted)^2))

MAE = mean(abs(
  cv_predictions$observed - cv_predictions$predicted))

R2 = 1 -
  sum((cv_predictions$observed - cv_predictions$predicted)^2) /
  sum((cv_predictions$observed - mean(cv_predictions$observed))^2)

data.frame(
  n_observations = nrow(cv_predictions),
  n_plots = n_distinct(cv_predictions$PlotID),
  RMSE = RMSE,
  MAE = MAE,
  R2 = R2)


## ----------------------------------------------------------------------------------------
ggplot(
  cv_predictions,
  aes(x = observed, y = predicted)) +
  geom_point(alpha = 0.7) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed") +
  labs(
    x = "Observed length of tunnelling (cm)",
    y = "Predicted length of tunnelling (cm)") +
  theme_bw()


## ----------------------------------------------------------------------------------------
set.seed(123)

rf_final = ranger(
  formula = model_formula,
  data = model_data_complete %>% select(-PlotID),
  num.trees = 1000,
  mtry = max(1, floor(sqrt(length(candidate_predictors)))),
  min.node.size = 5,
  importance = "permutation",
  seed = 123)

importance = data.frame(
  variable = names(rf_final$variable.importance),
  importance = as.numeric(rf_final$variable.importance)) %>%
  arrange(desc(importance))

head(importance, 20)


## ----------------------------------------------------------------------------------------
importance %>%
  slice_max(
    order_by = importance,
    n = 20) %>%
  ggplot(
    aes(
      x = reorder(variable, importance),
      y = importance)) +
  geom_col(fill = "#4E79A7") +
  coord_flip() +
  labs(
    x = NULL,
    y = "Permutation importance") +
  theme_bw()


## ----------------------------------------------------------------------------------------
model_predictors = model_data_complete %>%
  dplyr::select(-PlotID, -all_of(response_variable))

# Year
year_grid = sort(unique(model_predictors$Year))

pdp_year = data.frame(
  Year = year_grid,
  yhat = sapply(year_grid, function(x) {
    newdata = model_predictors
    newdata$Year = x
    mean(predict(rf_final, data = newdata)$predictions)
  }))

ggplot(pdp_year, aes(x = Year, y = yhat)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    x = "Year",
    y = "Predicted length of tunnelling (cm)") +
  theme_bw()

# Number of crop patches within 5 km
crop_patches_grid = seq(
  min(model_predictors$n_crops_patches_5000m, na.rm = TRUE),
  max(model_predictors$n_crops_patches_5000m, na.rm = TRUE),
  length.out = 50)

pdp_crop_patches = data.frame(
  n_crops_patches_5000m = crop_patches_grid,
  yhat = sapply(crop_patches_grid, function(x) {
    newdata = model_predictors
    newdata$n_crops_patches_5000m = x
    mean(predict(rf_final, data = newdata)$predictions)
  }))

ggplot(pdp_crop_patches, aes(x = n_crops_patches_5000m, y = yhat)) +
  geom_line(linewidth = 1) +
  labs(
    x = "Number of crop patches within 5 km",
    y = "Predicted length of tunnelling (cm)") +
  theme_bw()

