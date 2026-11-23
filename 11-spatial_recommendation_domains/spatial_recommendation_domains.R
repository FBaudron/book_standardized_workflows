## ----message=FALSE, warning=FALSE--------------------------------------------------------
if (!require('terra')) install.packages("terra")
if (!require('geodata')) install.packages("geodata")
if (!require('sf')) install.packages("sf")
if (!require('dplyr')) install.packages("dplyr")
if (!require('tidyr')) install.packages("tidyr")
if (!require('ggplot2')) install.packages("ggplot2")
if (!require('ade4')) install.packages("ade4")
if (!require('factoextra')) install.packages("factoextra")
if (!require('randomForest')) install.packages("randomForest")
if (!require('ggspatial')) install.packages("ggspatial")
if (!require('tidyterra')) install.packages("tidyterra")
if (!require('knitr')) install.packages("knitr")
if (!require('kableExtra')) install.packages("kableExtra")
if (!require('MASS')) install.packages("MASS")
if (!require('cluster')) install.packages("cluster")

library(terra)
library(geodata)
library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ade4)
library(factoextra)
library(randomForest)
library(ggspatial)
library(tidyterra)
library(knitr)
library(kableExtra)
library(MASS)
library(cluster)

path = "Data"
set.seed(123)


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


## ----------------------------------------------------------------------------------------
set.seed(123)

pca_sample = spatSample(
  indicator_z,
  size = 10000,
  method = "random",
  na.rm = TRUE,
  as.df = TRUE,
  xy = TRUE)

pca_sample = na.omit(pca_sample)

nrow(pca_sample)


## ----------------------------------------------------------------------------------------
pca = dudi.pca(
  pca_sample[, -c(1:2)],
  scannf = FALSE,
  nf = 4)

eigenvalues = data.frame(
  eigenvalue = pca$eig,
  axis = paste0("Axis", seq_along(pca$eig)))

initial_eigenvalues %>%
  knitr::kable(
    digits = 3,
    booktabs = TRUE,
    col.names = c("Eigenvalue", "Principal component"),
    caption = "Eigenvalues of the initial PCA") %>%
  kableExtra::kable_styling(
    latex_options = "striped",
    font_size = 9,
    position = "center")


## ----------------------------------------------------------------------------------------
fviz_pca_var(
  pca,
  col.var = "contrib") +
  scale_color_gradient2(
    low = "blue",
    mid = "purple",
    high = "red",
    midpoint = 9) +
  theme_minimal()

fviz_pca_ind(pca_initial)


## ----fig.width=9, fig.height=7-----------------------------------------------------------
pc_raster = terra::predict(
  indicator_z,
  pca)

names(pc_raster) = c("pc1", "pc2", "pc3", "pc4")

plot(pc_raster)


## ----------------------------------------------------------------------------------------
pc_scores = pca$li

hclust_model = hclust(
  dist(pc_scores),
  method = "ward.D2")

plot(
  hclust_model,
  hang = -1,
  labels = FALSE,
  main = "Ward hierarchical clustering",
  xlab = "",
  sub = "")


## ----------------------------------------------------------------------------------------
k_values = 3:10
sil_width = numeric(length(k_values))

for (i in seq_along(k_values)) {
  k = k_values[i]
  cluster_membership = cutree(
    hclust_model,
    k = k)
  sil = silhouette(
    cluster_membership,
    dist(pc_scores))
  sil_width[i] = mean(sil[, "sil_width"])
}

best_k = k_values[which.max(sil_width)]

plot(
  k_values,
  sil_width,
  type = "b",
  pch = 19,
  xlab = "Number of domains",
  ylab = "Average silhouette width")


## ----------------------------------------------------------------------------------------
k = 5

domains = cutree(
  hclust_model,
  k = k)

table(domains)


## ----------------------------------------------------------------------------------------
rf_training = data.frame(
  pc1 = pc_scores[, 1],
  pc2 = pc_scores[, 2],
  pc3 = pc_scores[, 3],
  pc4 = pc_scores[, 4],
  cluster = as.factor(domains))

set.seed(123)

rf_model = randomForest(
  cluster ~ pc1 + pc2 + pc3 + pc4,
  data = rf_training,
  importance = TRUE)

print(rf_model)


## ----fig.width=9, fig.height=7-----------------------------------------------------------
domain_raster = terra::predict(
  pc_raster,
  rf_model,
  type = "response")

names(domain_raster) = "domain"
domain_raster = mask(domain_raster, cropmask)

# Convert domain values to a categorical raster
domain_raster = as.factor(domain_raster)

ggplot() +
  geom_spatraster(
    data = domain_raster) +
  geom_spatvector(
    data = ssa,
    fill = NA,
    color = "black",
    linewidth = 0.3) +
  scale_fill_viridis_d(
    option = "inferno",
    begin = 0.2,
    end = 0.95,
    name = "Domain",
    na.value = "transparent",
    na.translate = FALSE) +
  coord_sf() +
  labs(title = "Recommendation domains across Sub-Saharan Africa") +
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


## ----------------------------------------------------------------------------------------
set.seed(123)

importance_stack = c(domain_raster, indicator_stack)
names(importance_stack)[1] = "domain"

importance_sample = spatSample(
  importance_stack,
  size = 10000,
  method = "random",
  na.rm = TRUE,
  as.df = TRUE)

importance_sample = na.omit(importance_sample)
importance_sample$domain = as.factor(importance_sample$domain)

rf_indicator_importance = randomForest(
  domain ~ .,
  data = importance_sample,
  importance = TRUE)

# Extract MeanDecreaseGini variable importance
importance_gini = randomForest::importance(rf_indicator_importance)[, "MeanDecreaseGini"]

# Plot variable importance
varImpPlot(
  rf_indicator_importance,
  type = 2,
  main = "Variable importance (MeanDecreaseGini)")


## ----------------------------------------------------------------------------------------
domain_means = zonal(
  indicator_stack,
  domain_raster,
  fun = "mean",
  na.rm = TRUE)

domain_sds = zonal(
  indicator_stack,
  domain_raster,
  fun = "sd",
  na.rm = TRUE)

kableExtra::landscape(
  domain_means %>%
    knitr::kable(
      digits = 2,
      booktabs = TRUE,
      caption = "Mean indicator values by spatial domain") %>%
    kableExtra::kable_styling(
      latex_options = "striped",
      font_size = 10,
      position = "center") %>%
    kableExtra::column_spec(1, bold = TRUE))

kableExtra::landscape(
  domain_sds %>%
    knitr::kable(
      digits = 2,
      booktabs = TRUE,
      caption = "Standard deviations of indicators by spatial domain") %>%
    kableExtra::kable_styling(
      latex_options = "striped",
      font_size = 10,
      position = "center") %>%
    kableExtra::column_spec(1, bold = TRUE))


## ----fig.width=9, fig.height=7-----------------------------------------------------------
set.seed(123)

# Sample cells from the domain and indicator rasters
domain_indicator_stack = c(domain_raster, indicator_stack)

domain_indicator_sample = spatSample(
  domain_indicator_stack,
  size = 10000,
  method = "random",
  na.rm = TRUE,
  as.df = TRUE)

domain_indicator_sample$domain = factor(
  domain_indicator_sample$domain,
  levels = 1:k)

# Reshape data into long format
domain_indicator_long = domain_indicator_sample |>
  pivot_longer(
    cols = all_of(names(indicator_stack)),
    names_to = "indicator",
    values_to = "value")

# Indicators displayed on a log-transformed scale
log_indicators = c(
  "rural_population",
  "soil_n",
  "tlu",
  "travel_time",
  "value_food_crops",
  "value_nonfood_crops")

domain_indicator_long = domain_indicator_long |>
  mutate(
    value_plot = ifelse(
      indicator %in% log_indicators,
      log1p(value),
      value))

# Plot distributions by recommendation domain
ggplot(
  domain_indicator_long,
  aes(x = domain, y = value_plot, fill = domain)) +
  geom_boxplot(
    outlier.size = 0.2,
    show.legend = FALSE) +
  scale_fill_viridis_d(
    option = "inferno",
    begin = 0.2,
    end = 0.95,
    name = "Domain") +
  facet_wrap(
    ~ indicator,
    scales = "free_y",
    ncol = 3) +
  labs(
    x = "Recommendation domain",
    y = "Indicator value (log-transformed for selected indicators)") +
  theme_bw()


## ----------------------------------------------------------------------------------------
# Geographical area of each recommendation domain
pixel_area_km2 = cellSize(
  domain_raster,
  unit = "km")

# Cropland area: convert hectares per pixel to km²
cropland_area_km2 = cropland_fraction / 100

# Sum geographical and cropland areas by domain
area_by_domain = zonal(
  c(pixel_area_km2, cropland_area_km2),
  domain_raster,
  fun = "sum",
  na.rm = TRUE)

names(area_by_domain) = c(
  "domain",
  "geographical_area_km2",
  "cropland_area_km2")

area_by_domain %>%
  knitr::kable(
    digits = 0,
    booktabs = TRUE,
    caption = "Geographical and cropland areas by recommendation domain") %>%
  kableExtra::kable_styling(
    latex_options = "striped",
    font_size = 10,
    position = "center") %>%
  kableExtra::column_spec(1, bold = TRUE)


## ----------------------------------------------------------------------------------------
population_by_domain = zonal(
  rural_population,
  domain_raster,
  fun = "sum",
  na.rm = TRUE)

names(population_by_domain) = c(
  "domain",
  "rural_population")

population_by_domain %>%
  knitr::kable(
    digits = 0,
    booktabs = TRUE,
    caption = "Rural population by recommendation domain") %>%
  kableExtra::kable_styling(
    latex_options = "striped",
    font_size = 10,
    position = "center") %>%
  kableExtra::column_spec(1, bold = TRUE)

