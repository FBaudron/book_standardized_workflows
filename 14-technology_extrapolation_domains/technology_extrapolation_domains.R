## ----message=FALSE, warning=FALSE--------------------------------------------------------
if (!require('terra')) install.packages("terra")
if (!require('geodata')) install.packages("geodata")
if (!require('sf')) install.packages("sf")
if (!require('dplyr')) install.packages("dplyr")
if (!require('ggplot2')) install.packages("ggplot2")
if (!require('ggspatial')) install.packages("ggspatial")
if (!require('tidyterra')) install.packages("tidyterra")

library(terra)
library(geodata)
library(sf)
library(dplyr)
library(ggplot2)
library(ggspatial)
library(tidyterra)

path = "Data"


## ----fig.width=12, fig.height=9----------------------------------------------------------
ssa = vect("Data/Spatial/Processed/ssa.shp")

cropland_fraction = rast("Data/Spatial/Processed/cropland.tif")
cropmask = rast("Data/Spatial/Processed/cropmask.tif")

sand = rast("Data/Spatial/Processed/sand.tif")
soil_n = rast("Data/Spatial/Processed/soil_n.tif")
awc = rast("Data/Spatial/Processed/pawc.tif")
mean_temperature = rast("Data/Spatial/Processed/tavg.tif")
annual_precipitation = rast("Data/Spatial/Processed/prec.tif")
tlu = rast("Data/Spatial/Processed/tlu.tif")
rural_population = rast("Data/Spatial/Processed/rural_population.tif")
value_food_crops = rast("Data/Spatial/Processed/value_food_crops.tif")
value_nonfood_crops = rast("Data/Spatial/Processed/value_nonfood_crops.tif")
travel_time = rast("Data/Spatial/Processed/travel_time.tif")

indicator_stack = c(
  sand,
  soil_n,
  awc,
  mean_temperature,
  annual_precipitation,
  tlu,
  rural_population,
  value_food_crops,
  value_nonfood_crops,
  travel_time)

names(indicator_stack) = c(
  "sand",
  "soil_n",
  "awc",
  "mean_temperature",
  "annual_precipitation",
  "tlu",
  "rural_population",
  "value_food_crops",
  "value_nonfood_crops",
  "travel_time")

plot(indicator_stack)


## ----fig.width=12, fig.height=9----------------------------------------------------------
terra::hist(
  indicator_stack,
  main = "Indicator distributions")


## ----------------------------------------------------------------------------------------
indicator_transformed = indicator_stack

vars_log = c(
  "soil_n",
  "tlu",
  "rural_population",
  "value_food_crops",
  "value_nonfood_crops",
  "travel_time")

for (v in vars_log) {
  indicator_transformed[[v]] = log1p(
    indicator_transformed[[v]])
}


## ----fig.width=12, fig.height=7----------------------------------------------------------
terra::hist(
  indicator_transformed,
  main = "Indicator distributions after transformation")


## ----------------------------------------------------------------------------------------
indicator_means = global(indicator_transformed, "mean", na.rm = TRUE)
indicator_sds   = global(indicator_transformed, "sd", na.rm = TRUE)

indicator_means = as.numeric(indicator_means[, 1])
indicator_sds   = as.numeric(indicator_sds[, 1])

indicator_z = (indicator_transformed - indicator_means) / indicator_sds

names(indicator_z) = paste0(names(indicator_transformed), "_z")


## ----fig.width=9, fig.height=7-----------------------------------------------------------
reference_sites <- vect("Data/ll_mfl.gpkg")

reference_sites <- reference_sites[
  reference_sites$district %in% c(
    "Senegal",
    "Ethiopia",
    "Kenya1",
    "Kenya2",
    "Zimbabwe",
    "Ivory Coast"
  ),
]

plot(ssa, main = "CGIAR MFL LL in sub-Saharan Africa")
plot(reference_sites, add = TRUE, col = "blue")


## ----reference-site-profiles-------------------------------------------------------------
site_names <- unique(reference_sites$district)
site_names <- site_names[!is.na(site_names)]

site_profiles <- data.frame(
  site = site_names,
  matrix(
    NA_real_,
    nrow = length(site_names),
    ncol = nlyr(indicator_z)))

names(site_profiles)[-1] <- names(indicator_z)

for (i in seq_along(site_names)) {
  site_polygon <- reference_sites[
    reference_sites$district == site_names[i], ]
  # Dissolve polygons belonging to the same reference site
  site_polygon <- terra::aggregate(
    site_polygon,
    by = "district",
    dissolve = TRUE)
  # Calculate the mean of each indicator within the site
  site_values <- terra::extract(
    indicator_z,
    site_polygon,
    fun = mean,
    na.rm = TRUE)
  site_profiles[i, -1] <- as.numeric(
    site_values[1, -1])
}

# Transpose site profiles: indicators in rows, reference sites in columns
site_profiles_t <- as.data.frame(t(site_profiles[, -1]))

names(site_profiles_t) <- site_profiles$site

site_profiles_t <- data.frame(
  Indicator = rownames(site_profiles_t),
  site_profiles_t,
  row.names = NULL,
  check.names = FALSE)

site_profiles_t %>%
  knitr::kable(
    digits = 2,
    booktabs = TRUE,
    caption = "Mean standardized indicator values across reference sites") %>%
  kableExtra::kable_styling(
    latex_options = "striped",
    font_size = 10,
    position = "center") %>%
  kableExtra::column_spec(1, bold = TRUE)


## ----------------------------------------------------------------------------------------
lambda <- 0.4

transferability_stack <- rast()

for (i in seq_along(site_names)) {
  site_profile <- as.numeric(site_profiles[i, -1])
  distance_squared <- (indicator_z - site_profile)^2
  distance_to_site <- sqrt(sum(distance_squared))
  transferability <- exp(-lambda * distance_to_site)
  names(transferability) <- site_names[i]
  transferability_stack <- c(
    transferability_stack,
    transferability)
}


## ----fig.width=10, fig.height=8----------------------------------------------------------
plot(
  transferability_stack,
  range = c(0, 1),
  main = names(transferability_stack))


## ----------------------------------------------------------------------------------------
maximum_cropland_fraction <- global(
  cropland_fraction,
  "max",
  na.rm = TRUE)[1, 1]

cropland_divisor <- ifelse(maximum_cropland_fraction > 1, 100, 1)

pixel_area_ha <- cellSize(cropmask, unit = "ha")

cropland_area_ha <- pixel_area_ha *
  (cropland_fraction / cropland_divisor)

cropland_area_ha <- mask(cropland_area_ha, cropmask)


## ----------------------------------------------------------------------------------------
transferability_thresholds <- seq(1, 0, by = -0.05)

total_cropland_ha <- global(
  cropland_area_ha,
  "sum",
  na.rm = TRUE)[1, 1]

total_rural_population <- global(
  rural_population,
  "sum",
  na.rm = TRUE)[1, 1]


## ----------------------------------------------------------------------------------------
cumulative_reach <- data.frame()

for (i in seq_along(site_names)) {
  site_transferability <- transferability_stack[[i]]
  cumulative_cropland <- numeric(length(transferability_thresholds))
  cumulative_population <- numeric(length(transferability_thresholds))
  for (j in seq_along(transferability_thresholds)) {
    threshold_mask <- site_transferability >=
      transferability_thresholds[j]
    cumulative_cropland[j] <- global(
      cropland_area_ha * threshold_mask,
      "sum",
      na.rm = TRUE
    )[1, 1]
    cumulative_population[j] <- global(
      rural_population * threshold_mask,
      "sum",
      na.rm = TRUE
    )[1, 1]
  }

  site_reach <- data.frame(
    reference_site = site_names[i],
    transferability_threshold = transferability_thresholds,
    cumulative_cropland_ha = cumulative_cropland,
    share_cropland_pct = 100 * cumulative_cropland / total_cropland_ha,
    cumulative_rural_population = cumulative_population,
    share_rural_population_pct =
      100 * cumulative_population / total_rural_population)

  cumulative_reach <- rbind(cumulative_reach, site_reach)
}



## ----fig.width=6, fig.height=5-----------------------------------------------------------
ggplot(
  cumulative_reach,
  aes(
    x = transferability_threshold,
    y = share_cropland_pct,
    colour = reference_site)) +
  geom_line(linewidth = 1) +
  scale_x_reverse(breaks = seq(0, 1, by = 0.1)) +
  labs(
    x = "Transferability threshold",
    y = "Cumulative cropland share (%)",
    colour = "Reference site") +
  theme_bw()


## ----fig.width=6, fig.height=5-----------------------------------------------------------
ggplot(
  cumulative_reach,
  aes(
    x = transferability_threshold,
    y = share_rural_population_pct,
    colour = reference_site)) +
  geom_line(linewidth = 1) +
  scale_x_reverse(breaks = seq(0, 1, by = 0.1)) +
  labs(
    x = "Transferability threshold",
    y = "Cumulative rural population share (%)",
    colour = "Reference site") +
  theme_bw()


## ----fig.width=9, fig.height=7-----------------------------------------------------------
maximum_transferability <- app(
  transferability_stack,
  fun = max,
  na.rm = TRUE)

# Visual stretch: 1st–99th percentiles
stretch_limits <- quantile(
  terra::values(maximum_transferability),
  probs = c(0.05, 0.95),
  na.rm = TRUE)

ggplot() +
  geom_spatraster(
    data = maximum_transferability) +
  geom_spatvector(
    data = ssa,
    fill = NA,
    color = "black",
    linewidth = 0.3) +
  scale_fill_viridis_c(
    option = "inferno",
    limits = stretch_limits,
    oob = scales::squish,
    name = "Transferability",
    na.value = "transparent") +
  coord_sf() +
  labs(title = "Maximum transferability across reference sites") +
  theme_bw() +
  theme(
    plot.title = element_text(
      size = 14,
      face = "bold"),
    legend.position = "right",
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    axis.title = element_blank(),
    panel.background = element_rect(
      fill = "white",
      color = NA),
    plot.background = element_rect(
      fill = "white",
      color = NA)) +
  annotation_scale(
    location = "br",
    width_hint = 0.3) +
  annotation_north_arrow(
    location = "tl",
    which_north = "true",
    style = north_arrow_fancy_orienteering())


## ----combined-reach----------------------------------------------------------------------
selected_threshold <- 0.6

combined_reach_mask <- ifel(
  maximum_transferability >= selected_threshold,
  1, NA)

combined_cropland <- mask(cropland_area_ha, combined_reach_mask)
combined_population <- mask(rural_population, combined_reach_mask)

combined_cropland_ha <- global(
  combined_cropland,
  "sum",
  na.rm = TRUE)[1, 1]

combined_rural_population <- global(
  combined_population,
  "sum",
  na.rm = TRUE)[1, 1]

combined_reach_summary <- data.frame(
  transferability_threshold = selected_threshold,
  unique_cropland_ha = combined_cropland_ha,
  share_cropland_pct =
    100 * combined_cropland_ha / total_cropland_ha,
  unique_rural_population = combined_rural_population,
  share_rural_population_pct =
    100 * combined_rural_population / total_rural_population)

# Transpose summary table for a vertical layout
combined_reach_summary_t <- data.frame(
  Indicator = names(combined_reach_summary),
  Value = as.numeric(combined_reach_summary[1, ]))

combined_reach_summary_t$Indicator <- c(
  "Transferability threshold",
  "Unique cropland area (ha)",
  "Share of total cropland (%)",
  "Rural population reached",
  "Share of total rural population (%)")

combined_reach_summary_t %>%
  knitr::kable(
    digits = 2,
    booktabs = TRUE,
    col.names = c("Indicator", "Value"),
    caption = "Cropland and rural population within the combined reach area") %>%
  kableExtra::kable_styling(
    latex_options = "striped",
    font_size = 10,
    position = "center") %>%
  kableExtra::column_spec(1, bold = TRUE)

