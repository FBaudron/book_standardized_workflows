## ----------------------------------------------------------------------------------------
if (!require('terra')) install.packages("terra")
if (!require('geodata')) install.packages("geodata")

library(terra)
library(geodata)

path = "Data"


## ----------------------------------------------------------------------------------------

zwe_all = geodata::gadm("ZWE", level = 2, path = path)
zwe = zwe_all[zwe_all$NAME_2 == "Mbire", ]

plot(zwe, main = "Mbire District, Zimbabwe")


## ----------------------------------------------------------------------------------------
maize_share  = 0.75
legume_share = 0.25

RUE                 = 2.35    # kg DM / ha / mm (Rufino 2011)
wet_months          = 6
winter_months       = 12 - wet_months
grazing_utilisation = 1.0

cattle_weight       = 250     # kg per head
cattle_daily_intake = 0.03    # fraction of body weight per day

collection_rate  = 0.4
manure_t_per_TLU = 1.022
N_content_manure = 0.014
P_content_manure = 0.002

NDFA_rate = 0.62
HI_gdnuts  = 0.40

# PLACEHOLDERS: replace with locally appropriate values
residue_to_grain_ratio = 1.5
residue_degradation    = 1
residue_feed_share     = 1

max_iterations = 50
tolerance      = 0.01


## ----------------------------------------------------------------------------------------
prec = geodata::worldclim_country("ZWE", var = "prec", path = path)

R = mask(crop(sum(prec), zwe), zwe)
names(R) = "rain_mm"

plot(R, main = "Annual rainfall (mm)")
plot(zwe, add = T)


## ----------------------------------------------------------------------------------------
grass_frac = geodata::landcover("grassland", path = path)

grass_frac = crop(grass_frac, zwe)
grass_frac = mask(grass_frac, zwe)

names(grass_frac) = "grassland_share"

plot(grass_frac, main = "Grassland share")
plot(zwe, add = T)


## ----------------------------------------------------------------------------------------
crop_frac  = geodata::landcover("cropland",  path = path)

crop_frac = crop(crop_frac, zwe)
crop_frac = mask(crop_frac, zwe)

names(crop_frac)  = "cropland_share"

plot(crop_frac, main = "Cropland share")
plot(zwe, add = T)


## ----------------------------------------------------------------------------------------
cell_ha = cellSize(R, unit = "ha")
zero_in = R * 0

Agrassland = cover(cell_ha * grass_frac, zero_in)
Acropland  = cover(cell_ha * crop_frac, zero_in)

names(Agrassland) = "grassland_ha"
names(Acropland)  = "cropland_ha"


## ----------------------------------------------------------------------------------------
daily_need_kg = cattle_daily_intake * cattle_weight
days_wet      = (365 / 12) * wet_months

wet_feed_per_TLU = daily_need_kg * days_wet

biomass_ha = RUE * R
plot(biomass_ha, main = "Grassland biomass (kg DM / ha)")
lines(zwe)

biomass = biomass_ha * Agrassland * grazing_utilisation

max_grazing_CC_TLU = biomass / wet_feed_per_TLU
names(max_grazing_CC_TLU) = "grazing_CC_TLU"

max_grazing_CC_TLU_ha = max_grazing_CC_TLU / cell_ha
names(max_grazing_CC_TLU_ha) = "grazing_CC_TLU_per_ha"


## ----------------------------------------------------------------------------------------
q_brk = unique(
  quantile(values(max_grazing_CC_TLU_ha), na.rm = TRUE))

print(q_brk)

if (length(q_brk) > 2) {
  plot(
    max_grazing_CC_TLU_ha,
    breaks = q_brk,
    main = "Grazing carrying capacity (TLU / ha)")
} else {
  plot(
    max_grazing_CC_TLU_ha,
    main = "Grazing carrying capacity (TLU / ha)")
}

lines(zwe, col = "black")


## ----------------------------------------------------------------------------------------
winter_feed_per_TLU_tons = (daily_need_kg / 1000) *
  (365 / 12) * winter_months

maize_ha  = Acropland * maize_share
legume_ha = Acropland * legume_share

names(maize_ha)  = "maize_ha"
names(legume_ha) = "legume_ha"


## ----------------------------------------------------------------------------------------
# Aboveground biomass and grain yield (kg/ha)
above = 2907
grain = 688

roots    = 0.3 * above
residues = above + roots

N_residues = residues * 0.02
N_grains   = grain * 0.038

N_uptake   = N_residues + N_grains
NDFA       = N_uptake * NDFA_rate
N_exported = NDFA * HI_gdnuts
N_fixed    = NDFA - N_exported

# N available per ha of maize, scaled by the legume:maize area ratio
N_legumes_per_ha_maize = N_fixed * (legume_share / maize_share)

total_gdnut_grain_prod = grain * legume_ha
total_N_legumes        = N_legumes_per_ha_maize * maize_ha

names(total_gdnut_grain_prod) = "total_gdnut_grain_kg"
names(total_N_legumes)        = "total_N_legumes_kg"


## ----------------------------------------------------------------------------------------
data_sandy_p_ge_30 = data.frame(
  x = c(0, 30, 60, 90, 120),
  y = c(1.19, 2.7, 2.97, 3.24, 3.73))

data_sandy_p_lt_30 = data.frame(
  x = c(0, 30, 60, 90, 120),
  y = c(0.645, 1.79, 1.823, 2.193, 2.016))

cf_ge = coef(lm(y ~ x + I(x^2), data = data_sandy_p_ge_30))
cf_lt = coef(lm(y ~ x + I(x^2), data = data_sandy_p_lt_30))

predict_yield = function(N_kg_ha, P_kg_ha) {
  N_c = terra::clamp(N_kg_ha, 0, 120)
  y_ge = cf_ge[1] + cf_ge[2] * N_c + cf_ge[3] * N_c^2
  y_lt = cf_lt[1] + cf_lt[2] * N_c + cf_lt[3] * N_c^2
  ifel(P_kg_ha >= 30, y_ge, y_lt)
}


## ----------------------------------------------------------------------------------------
CC_grazing = max_grazing_CC_TLU

has_maize  = maize_ha > 0
maize_safe = ifel(has_maize, maize_ha, NA)

n_inside = global(!is.na(R), "sum")[1, 1]

current_TLU           = CC_grazing
convergence_iteration = R * NA
max_change            = rep(NA_real_, max_iterations)
actual_iterations     = max_iterations


## ----------------------------------------------------------------------------------------
for (iter in seq_len(max_iterations)) {
  N_manure_per_ha = (
    current_TLU *
      manure_t_per_TLU *
      collection_rate *
      N_content_manure *
      1000) / maize_safe
  P_manure_per_ha = (
    current_TLU *
      manure_t_per_TLU *
      collection_rate *
      P_content_manure *
      1000) / maize_safe
  N_total_per_ha = N_manure_per_ha + N_legumes_per_ha_maize
  P_total_per_ha = P_manure_per_ha
  sandy_yield      = predict_yield(N_total_per_ha, P_total_per_ha)
  total_maize_prod = maize_ha * sandy_yield
  total_residues = total_maize_prod *
    residue_to_grain_ratio *
    residue_degradation *
    residue_feed_share
  CC_residues = total_residues / winter_feed_per_TLU_tons
  CC_residues = ifel(has_maize, CC_residues, 0)
  # Cell-wise minimum of grazing- and residue-based carrying capacity
  new_TLU = ifel(
    CC_grazing < CC_residues,
    CC_grazing,
    CC_residues)
  change = abs(new_TLU - current_TLU)
  max_change[iter] = global(change, "max", na.rm = TRUE)[1, 1]
  if (iter > 1) {
    newly_converged = is.na(convergence_iteration) &
      (change < tolerance)
    convergence_iteration = ifel(
      newly_converged,
      iter,
      convergence_iteration)
  }
  current_TLU = new_TLU
  n_conv = global(!is.na(convergence_iteration), "sum")[1, 1]
  if (n_conv == n_inside) {
    actual_iterations = iter
    break
  }
}


## ----------------------------------------------------------------------------------------
final_TLU = current_TLU
names(final_TLU) = "final_TLU"

TLU_per_ha = final_TLU / cell_ha
names(TLU_per_ha) = "TLU_per_ha"

limiting_factor = ifel(CC_residues < CC_grazing, 2, 1)
names(limiting_factor) = "limiting_factor"

names(convergence_iteration) = "convergence_iteration"

plot(limiting_factor, main = "Limiting factor")
plot(zwe, add = T)


## ----------------------------------------------------------------------------------------
# Calculate the 98th percentile for the display scale
grazing_CC_max = quantile(
  values(max_grazing_CC_TLU_ha, mat = FALSE),
  probs = 0.98,
  na.rm = TRUE)

# Cap values above the 98th percentile for visualisation
max_grazing_CC_TLU_ha_plot = terra::clamp(
  max_grazing_CC_TLU_ha,
  lower = 0,
  upper = grazing_CC_max)

plot(
  max_grazing_CC_TLU_ha_plot,
  main = "Grazing-based carrying capacity (TLU/ha)",
  col = hcl.colors(100, "YlGnBu", rev = TRUE))
plot(zwe, add = T)


## ----------------------------------------------------------------------------------------
plot(
  TLU_per_ha,
  main = "Final stocking density (TLU / ha)",
  col = hcl.colors(100, "YlGnBu", rev = TRUE))
plot(zwe, add = T)


## ----------------------------------------------------------------------------------------
plot(
  as.factor(limiting_factor),
  main = "Limiting factor",
  col = c("#2C7BB6", "#D7191C"),
  levels = c("Grazing land", "Crop residues"),
  plg = list(
    title = "Limiting resource"))
plot(zwe, add = T)


## ----------------------------------------------------------------------------------------
# Calculate the 98th percentile for the display scale
N_manure_max = quantile(
  values(N_manure_per_ha, mat = FALSE),
  probs = 0.98,
  na.rm = TRUE)

# Cap values above the 98th percentile for visualisation
N_manure_per_ha_plot = terra::clamp(
  N_manure_per_ha,
  lower = 0,
  upper = N_manure_max)

plot(
  N_manure_per_ha_plot,
  main = ("N from manure (kg/ha) at final stocking density, kg/ha"),
  col = hcl.colors(100, "YlGnBu", rev = TRUE))
plot(zwe, add = TRUE, col = NA, border = "black")


## ----------------------------------------------------------------------------------------
glw = rast("Data/GLW4-2020.D-DA.CTL.tif")

# GLW cells are approximately 10 km; retain cells that partly
# overlap the district
glw = mask(
  crop(glw, zwe, snap = "out"),
  zwe,
  touches = TRUE)

# Convert cattle heads to TLU
glw = glw * 0.7

# Modelled livestock density (TLU / km2)
mine_km2 = final_TLU / (cell_ha / 100)

# Aggregate modelled density to the GLW grid
mine_glw = resample(mine_km2, glw, method = "average")

compare = c(mine_glw, glw)
names(compare) = c("model_TLU_km2", "GLW_cattle_km2")

plot(compare)


## ----fig.width=9, fig.height=7-----------------------------------------------------------
d = as.data.frame(compare, na.rm = TRUE)

cor(
  d$model_TLU_km2,
  d$GLW_cattle_km2,
  method = "spearman")

lim = range(
  c(d$model_TLU_km2, d$GLW_cattle_km2),
  na.rm = TRUE)

plot(
  d$model_TLU_km2,
  d$GLW_cattle_km2,
  xlim = lim,
  ylim = lim,
  xlab = "Modelled livestock density (TLU/km2)",
  ylab = "GLW cattle density (TLU/km2)")
abline(0, 1)
abline(
  lm(GLW_cattle_km2 ~ model_TLU_km2, data = d),
  col = "blue",
  lty = 1)

