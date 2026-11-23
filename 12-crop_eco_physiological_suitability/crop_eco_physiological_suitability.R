## ----message=FALSE, warning=FALSE--------------------------------------------------------
if (!require('terra')) install.packages("terra")
if (!require('geodata')) install.packages("geodata")
if (!require('ggplot2')) install.packages("ggplot2")
if (!require('tidyterra')) install.packages("tidyterra")
if (!require('ggspatial')) install.packages("ggspatial")
if (!require('dplyr')) install.packages("dplyr")
if (!require('viridis')) install.packages("viridis")

library(terra)
library(geodata)
library(ggplot2)
library(tidyterra)
library(ggspatial)
library(dplyr)
library(viridis)

path = "Data"


## ----------------------------------------------------------------------------------------

ssa = vect("Data/Spatial/Processed/ssa.shp")

cropmask = rast("Data/Spatial/Processed/cropmask.tif")

prec_annual = rast("Data/Spatial/Processed/prec.tif")

ph = rast("Data/Spatial/Processed/ph.tif")


## ----------------------------------------------------------------------------------------

tavg = worldclim_global(var = "tavg", res = 5, path = path)

names(tavg) = c(
  "Jan", "Feb", "Mar", "Apr",
  "May", "Jun", "Jul", "Aug",
  "Sep", "Oct", "Nov", "Dec")

tavg = crop(tavg, cropmask)
tavg = mask(tavg, cropmask)



## ----------------------------------------------------------------------------------------
maize_main = crop_calendar_sacks(crop = "Maize (main season)", path = path)
maize_main = crop(maize_main, cropmask)
maize_main = mask(maize_main, cropmask)

plot(maize_main[[2]], main = "Maize planting, main growing season (mid-point)")
plot(ssa, add = T)


## ----------------------------------------------------------------------------------------
plot(maize_main[[5]], main = "Maize harvesting, main growing season (mid-point)")
plot(ssa, add = T)


## ----------------------------------------------------------------------------------------
maize_second = crop_calendar_sacks(crop = "Maize (2nd season)", path = path)
maize_second = crop(maize_second, cropmask)
maize_second = mask(maize_second, cropmask)

plot(maize_second[[2]], main = "Maize planting, second growing season (mid-point)")
plot(ssa, add = T)


## ----------------------------------------------------------------------------------------
plot(maize_second[[5]], main = "Maize harvesting, second growing season (mid-point)")
plot(ssa, add = T)


## ----------------------------------------------------------------------------------------
sorghum_main = crop_calendar_sacks(crop = "Sorghum (main season)", path = path)
sorghum_main = crop(sorghum_main, cropmask)
sorghum_main = mask(sorghum_main, cropmask)

plot(sorghum_main[[2]], main = "Sorghum planting, main growing season (mid-point)")
plot(ssa, add = T)


## ----------------------------------------------------------------------------------------
plot(sorghum_main[[5]], main = "Sorghum harvesting, main growing season (mid-point)")
plot(ssa, add = T)


## ----------------------------------------------------------------------------------------
sorghum_second = crop_calendar_sacks(crop = "Sorghum (2nd season)", path = path)
sorghum_second = crop(sorghum_second, cropmask)
sorghum_second = mask(sorghum_second, cropmask)

plot(sorghum_second[[2]], main = "Sorghum planting, second growing season (mid-point)")
plot(ssa, add = T)


## ----------------------------------------------------------------------------------------
plot(sorghum_second[[5]], main = "Sorghum harvesting, second growing season (mid-point)")
plot(ssa, add = T)


## ----------------------------------------------------------------------------------------
maize_main_plant = maize_main[["plant"]]
maize_main_harvest = maize_main[["harvest"]]

maize_main_valid = ifel(
  !is.na(maize_main_plant) &
    !is.na(maize_main_harvest),
  1,NA)

# Identify months whose midpoint falls within the growing period.
# The second condition accounts for growing seasons crossing December-January.
maize_main_tjan = ifel(
  (maize_main_plant <= 15 &
     maize_main_harvest >= 15) |
    (maize_main_harvest < maize_main_plant &
       (maize_main_plant <= 15 |
          maize_main_harvest >= 15)),
  tavg[[1]],
  NA)

maize_main_tfeb = ifel(
  (maize_main_plant <= 45 &
     maize_main_harvest >= 45) |
    (maize_main_harvest < maize_main_plant &
       (maize_main_plant <= 45 |
          maize_main_harvest >= 45)),
  tavg[[2]],
  NA)

maize_main_tmar = ifel(
  (maize_main_plant <= 74 &
     maize_main_harvest >= 74) |
    (maize_main_harvest < maize_main_plant &
       (maize_main_plant <= 74 |
          maize_main_harvest >= 74)),
  tavg[[3]],
  NA)

maize_main_tapr = ifel(
  (maize_main_plant <= 105 &
     maize_main_harvest >= 105) |
    (maize_main_harvest < maize_main_plant &
       (maize_main_plant <= 105 |
          maize_main_harvest >= 105)),
  tavg[[4]],
  NA)

maize_main_tmay = ifel(
  (maize_main_plant <= 135 &
     maize_main_harvest >= 135) |
    (maize_main_harvest < maize_main_plant &
       (maize_main_plant <= 135 |
          maize_main_harvest >= 135)),
  tavg[[5]],
  NA)

maize_main_tjun = ifel(
  (maize_main_plant <= 166 &
     maize_main_harvest >= 166) |
    (maize_main_harvest < maize_main_plant &
       (maize_main_plant <= 166 |
          maize_main_harvest >= 166)),
  tavg[[6]],
  NA)

maize_main_tjul = ifel(
  (maize_main_plant <= 196 &
     maize_main_harvest >= 196) |
    (maize_main_harvest < maize_main_plant &
       (maize_main_plant <= 196 |
          maize_main_harvest >= 196)),
  tavg[[7]],
  NA)

maize_main_taug = ifel(
  (maize_main_plant <= 227 &
     maize_main_harvest >= 227) |
    (maize_main_harvest < maize_main_plant &
       (maize_main_plant <= 227 |
          maize_main_harvest >= 227)),
  tavg[[8]],
  NA)

maize_main_tsep = ifel(
  (maize_main_plant <= 258 &
     maize_main_harvest >= 258) |
    (maize_main_harvest < maize_main_plant &
       (maize_main_plant <= 258 |
          maize_main_harvest >= 258)),
  tavg[[9]],
  NA)

maize_main_toct = ifel(
  (maize_main_plant <= 288 &
     maize_main_harvest >= 288) |
    (maize_main_harvest < maize_main_plant &
       (maize_main_plant <= 288 |
          maize_main_harvest >= 288)),
  tavg[[10]],
  NA)

maize_main_tnov = ifel(
  (maize_main_plant <= 319 &
     maize_main_harvest >= 319) |
    (maize_main_harvest < maize_main_plant &
       (maize_main_plant <= 319 |
          maize_main_harvest >= 319)),
  tavg[[11]],
  NA)

maize_main_tdec = ifel(
  (maize_main_plant <= 349 &
     maize_main_harvest >= 349) |
    (maize_main_harvest < maize_main_plant &
       (maize_main_plant <= 349 |
          maize_main_harvest >= 349)),
  tavg[[12]],
  NA)


# Mean Tmean during maize main growing season
# Because `na.rm = TRUE` is used, months outside the growing period are ignored
maize_main_tmean = app(
  c(
    maize_main_tjan,
    maize_main_tfeb,
    maize_main_tmar,
    maize_main_tapr,
    maize_main_tmay,
    maize_main_tjun,
    maize_main_tjul,
    maize_main_taug,
    maize_main_tsep,
    maize_main_toct,
    maize_main_tnov,
    maize_main_tdec),
  mean,
  na.rm = TRUE)

maize_main_tmean = mask(maize_main_tmean, maize_main_valid)

names(maize_main_tmean) = "maize_main_tmean"

plot(maize_main_tmean, main = "Mean teamperature during the main growing season of maize")
plot(ssa, add = T)


## ----maize-second-temperature------------------------------------------------------------

maize_second_plant = maize_second[["plant"]]
maize_second_harvest = maize_second[["harvest"]]

maize_second_valid = ifel(
  !is.na(maize_second_plant) &
    !is.na(maize_second_harvest),
  1,NA)

maize_second_tjan = ifel(
  (maize_second_plant <= 15 &
     maize_second_harvest >= 15) |
    (maize_second_harvest < maize_second_plant &
       (maize_second_plant <= 15 |
          maize_second_harvest >= 15)),
  tavg[[1]],
  NA)

maize_second_tfeb = ifel(
  (maize_second_plant <= 45 &
     maize_second_harvest >= 45) |
    (maize_second_harvest < maize_second_plant &
       (maize_second_plant <= 45 |
          maize_second_harvest >= 45)),
  tavg[[2]],
  NA)

maize_second_tmar = ifel(
  (maize_second_plant <= 74 &
     maize_second_harvest >= 74) |
    (maize_second_harvest < maize_second_plant &
       (maize_second_plant <= 74 |
          maize_second_harvest >= 74)),
  tavg[[3]],
  NA)

maize_second_tapr = ifel(
  (maize_second_plant <= 105 &
     maize_second_harvest >= 105) |
    (maize_second_harvest < maize_second_plant &
       (maize_second_plant <= 105 |
          maize_second_harvest >= 105)),
  tavg[[4]],
  NA)

maize_second_tmay = ifel(
  (maize_second_plant <= 135 &
     maize_second_harvest >= 135) |
    (maize_second_harvest < maize_second_plant &
       (maize_second_plant <= 135 |
          maize_second_harvest >= 135)),
  tavg[[5]],
  NA)

maize_second_tjun = ifel(
  (maize_second_plant <= 166 &
     maize_second_harvest >= 166) |
    (maize_second_harvest < maize_second_plant &
       (maize_second_plant <= 166 |
          maize_second_harvest >= 166)),
  tavg[[6]],
  NA)

maize_second_tjul = ifel(
  (maize_second_plant <= 196 &
     maize_second_harvest >= 196) |
    (maize_second_harvest < maize_second_plant &
       (maize_second_plant <= 196 |
          maize_second_harvest >= 196)),
  tavg[[7]],
  NA)

maize_second_taug = ifel(
  (maize_second_plant <= 227 &
     maize_second_harvest >= 227) |
    (maize_second_harvest < maize_second_plant &
       (maize_second_plant <= 227 |
          maize_second_harvest >= 227)),
  tavg[[8]],
  NA)

maize_second_tsep = ifel(
  (maize_second_plant <= 258 &
     maize_second_harvest >= 258) |
    (maize_second_harvest < maize_second_plant &
       (maize_second_plant <= 258 |
          maize_second_harvest >= 258)),
  tavg[[9]],
  NA)

maize_second_toct = ifel(
  (maize_second_plant <= 288 &
     maize_second_harvest >= 288) |
    (maize_second_harvest < maize_second_plant &
       (maize_second_plant <= 288 |
          maize_second_harvest >= 288)),
  tavg[[10]],
  NA)

maize_second_tnov = ifel(
  (maize_second_plant <= 319 &
     maize_second_harvest >= 319) |
    (maize_second_harvest < maize_second_plant &
       (maize_second_plant <= 319 |
          maize_second_harvest >= 319)),
  tavg[[11]],
  NA)

maize_second_tdec = ifel(
  (maize_second_plant <= 349 &
     maize_second_harvest >= 349) |
    (maize_second_harvest < maize_second_plant &
       (maize_second_plant <= 349 |
          maize_second_harvest >= 349)),
  tavg[[12]],
  NA)


maize_second_tmean = app(
  c(
    maize_second_tjan,
    maize_second_tfeb,
    maize_second_tmar,
    maize_second_tapr,
    maize_second_tmay,
    maize_second_tjun,
    maize_second_tjul,
    maize_second_taug,
    maize_second_tsep,
    maize_second_toct,
    maize_second_tnov,
    maize_second_tdec),
  mean,
  na.rm = TRUE)

maize_second_tmean = mask(maize_second_tmean, maize_second_valid)

plot(maize_second_tmean, main = "Mean teamperature during the second growing season of maize")
plot(ssa, add = T)


## ----------------------------------------------------------------------------------------

sorghum_main_plant = sorghum_main[["plant"]]
sorghum_main_harvest = sorghum_main[["harvest"]]

sorghum_main_valid = ifel(
  !is.na(sorghum_main_plant) &
    !is.na(sorghum_main_harvest),
  1, NA)

sorghum_main_tjan = ifel(
  (sorghum_main_plant <= 15 &
     sorghum_main_harvest >= 15) |
    (sorghum_main_harvest < sorghum_main_plant &
       (sorghum_main_plant <= 15 |
          sorghum_main_harvest >= 15)),
  tavg[[1]],
  NA)

sorghum_main_tfeb = ifel(
  (sorghum_main_plant <= 45 &
     sorghum_main_harvest >= 45) |
    (sorghum_main_harvest < sorghum_main_plant &
       (sorghum_main_plant <= 45 |
          sorghum_main_harvest >= 45)),
  tavg[[2]],
  NA)

sorghum_main_tmar = ifel(
  (sorghum_main_plant <= 74 &
     sorghum_main_harvest >= 74) |
    (sorghum_main_harvest < sorghum_main_plant &
       (sorghum_main_plant <= 74 |
          sorghum_main_harvest >= 74)),
  tavg[[3]],
  NA)

sorghum_main_tapr = ifel(
  (sorghum_main_plant <= 105 &
     sorghum_main_harvest >= 105) |
    (sorghum_main_harvest < sorghum_main_plant &
       (sorghum_main_plant <= 105 |
          sorghum_main_harvest >= 105)),
  tavg[[4]],
  NA)

sorghum_main_tmay = ifel(
  (sorghum_main_plant <= 135 &
     sorghum_main_harvest >= 135) |
    (sorghum_main_harvest < sorghum_main_plant &
       (sorghum_main_plant <= 135 |
          sorghum_main_harvest >= 135)),
  tavg[[5]],
  NA)

sorghum_main_tjun = ifel(
  (sorghum_main_plant <= 166 &
     sorghum_main_harvest >= 166) |
    (sorghum_main_harvest < sorghum_main_plant &
       (sorghum_main_plant <= 166 |
          sorghum_main_harvest >= 166)),
  tavg[[6]],
  NA)

sorghum_main_tjul = ifel(
  (sorghum_main_plant <= 196 &
     sorghum_main_harvest >= 196) |
    (sorghum_main_harvest < sorghum_main_plant &
       (sorghum_main_plant <= 196 |
          sorghum_main_harvest >= 196)),
  tavg[[7]],
  NA)

sorghum_main_taug = ifel(
  (sorghum_main_plant <= 227 &
     sorghum_main_harvest >= 227) |
    (sorghum_main_harvest < sorghum_main_plant &
       (sorghum_main_plant <= 227 |
          sorghum_main_harvest >= 227)),
  tavg[[8]],
  NA)

sorghum_main_tsep = ifel(
  (sorghum_main_plant <= 258 &
     sorghum_main_harvest >= 258) |
    (sorghum_main_harvest < sorghum_main_plant &
       (sorghum_main_plant <= 258 |
          sorghum_main_harvest >= 258)),
  tavg[[9]],
  NA)

sorghum_main_toct = ifel(
  (sorghum_main_plant <= 288 &
     sorghum_main_harvest >= 288) |
    (sorghum_main_harvest < sorghum_main_plant &
       (sorghum_main_plant <= 288 |
          sorghum_main_harvest >= 288)),
  tavg[[10]],
  NA)

sorghum_main_tnov = ifel(
  (sorghum_main_plant <= 319 &
     sorghum_main_harvest >= 319) |
    (sorghum_main_harvest < sorghum_main_plant &
       (sorghum_main_plant <= 319 |
          sorghum_main_harvest >= 319)),
  tavg[[11]],
  NA)

sorghum_main_tdec = ifel(
  (sorghum_main_plant <= 349 &
     sorghum_main_harvest >= 349) |
    (sorghum_main_harvest < sorghum_main_plant &
       (sorghum_main_plant <= 349 |
          sorghum_main_harvest >= 349)),
  tavg[[12]],
  NA)

sorghum_main_tmean = app(
  c(
    sorghum_main_tjan,
    sorghum_main_tfeb,
    sorghum_main_tmar,
    sorghum_main_tapr,
    sorghum_main_tmay,
    sorghum_main_tjun,
    sorghum_main_tjul,
    sorghum_main_taug,
    sorghum_main_tsep,
    sorghum_main_toct,
    sorghum_main_tnov,
    sorghum_main_tdec),
  mean,
  na.rm = TRUE)

sorghum_main_tmean = mask(sorghum_main_tmean,sorghum_main_valid)

plot(sorghum_main_tmean, main = "Mean teamperature during the main growing season of sorghum")
plot(ssa, add = T)


## ----------------------------------------------------------------------------------------
sorghum_second_plant = sorghum_second[["plant"]]
sorghum_second_harvest = sorghum_second[["harvest"]]

sorghum_second_valid = ifel(
  !is.na(sorghum_second_plant) &
    !is.na(sorghum_second_harvest),
  1, NA)

sorghum_second_tjan = ifel(
  (sorghum_second_plant <= 15 &
     sorghum_second_harvest >= 15) |
    (sorghum_second_harvest < sorghum_second_plant &
       (sorghum_second_plant <= 15 |
          sorghum_second_harvest >= 15)),
  tavg[[1]],
  NA)

sorghum_second_tfeb = ifel(
  (sorghum_second_plant <= 45 &
     sorghum_second_harvest >= 45) |
    (sorghum_second_harvest < sorghum_second_plant &
       (sorghum_second_plant <= 45 |
          sorghum_second_harvest >= 45)),
  tavg[[2]],
  NA)

sorghum_second_tmar = ifel(
  (sorghum_second_plant <= 74 &
     sorghum_second_harvest >= 74) |
    (sorghum_second_harvest < sorghum_second_plant &
       (sorghum_second_plant <= 74 |
          sorghum_second_harvest >= 74)),
  tavg[[3]],
  NA)

sorghum_second_tapr = ifel(
  (sorghum_second_plant <= 105 &
     sorghum_second_harvest >= 105) |
    (sorghum_second_harvest < sorghum_second_plant &
       (sorghum_second_plant <= 105 |
          sorghum_second_harvest >= 105)),
  tavg[[4]],
  NA)

sorghum_second_tmay = ifel(
  (sorghum_second_plant <= 135 &
     sorghum_second_harvest >= 135) |
    (sorghum_second_harvest < sorghum_second_plant &
       (sorghum_second_plant <= 135 |
          sorghum_second_harvest >= 135)),
  tavg[[5]],
  NA)

sorghum_second_tjun = ifel(
  (sorghum_second_plant <= 166 &
     sorghum_second_harvest >= 166) |
    (sorghum_second_harvest < sorghum_second_plant &
       (sorghum_second_plant <= 166 |
          sorghum_second_harvest >= 166)),
  tavg[[6]],
  NA)

sorghum_second_tjul = ifel(
  (sorghum_second_plant <= 196 &
     sorghum_second_harvest >= 196) |
    (sorghum_second_harvest < sorghum_second_plant &
       (sorghum_second_plant <= 196 |
          sorghum_second_harvest >= 196)),
  tavg[[7]],
  NA)

sorghum_second_taug = ifel(
  (sorghum_second_plant <= 227 &
     sorghum_second_harvest >= 227) |
    (sorghum_second_harvest < sorghum_second_plant &
       (sorghum_second_plant <= 227 |
          sorghum_second_harvest >= 227)),
  tavg[[8]],
  NA)

sorghum_second_tsep = ifel(
  (sorghum_second_plant <= 258 &
     sorghum_second_harvest >= 258) |
    (sorghum_second_harvest < sorghum_second_plant &
       (sorghum_second_plant <= 258 |
          sorghum_second_harvest >= 258)),
  tavg[[9]],
  NA)

sorghum_second_toct = ifel(
  (sorghum_second_plant <= 288 &
     sorghum_second_harvest >= 288) |
    (sorghum_second_harvest < sorghum_second_plant &
       (sorghum_second_plant <= 288 |
          sorghum_second_harvest >= 288)),
  tavg[[10]],
  NA)

sorghum_second_tnov = ifel(
  (sorghum_second_plant <= 319 &
     sorghum_second_harvest >= 319) |
    (sorghum_second_harvest < sorghum_second_plant &
       (sorghum_second_plant <= 319 |
          sorghum_second_harvest >= 319)),
  tavg[[11]],
  NA)

sorghum_second_tdec = ifel(
  (sorghum_second_plant <= 349 &
     sorghum_second_harvest >= 349) |
    (sorghum_second_harvest < sorghum_second_plant &
       (sorghum_second_plant <= 349 |
          sorghum_second_harvest >= 349)),
  tavg[[12]],
  NA)

sorghum_second_tmean = app(
  c(
    sorghum_second_tjan,
    sorghum_second_tfeb,
    sorghum_second_tmar,
    sorghum_second_tapr,
    sorghum_second_tmay,
    sorghum_second_tjun,
    sorghum_second_tjul,
    sorghum_second_taug,
    sorghum_second_tsep,
    sorghum_second_toct,
    sorghum_second_tnov,
    sorghum_second_tdec),
  mean,
  na.rm = TRUE)

sorghum_second_tmean = mask(
  sorghum_second_tmean,
  sorghum_second_valid)

names(sorghum_second_tmean) = "sorghum_second_tmean"

plot(sorghum_second_tmean, main = "Mean teamperature during the second growing season of sorghum")
plot(ssa, add = T)


## ----------------------------------------------------------------------------------------
maize_main_temp_fuzzy = ifel(
  maize_main_tmean <= 10,
  0,
  ifel(
    maize_main_tmean < 18,
    (maize_main_tmean - 10) / (18 - 10),
    ifel(
      maize_main_tmean <= 33,
      1,
      ifel(
        maize_main_tmean < 47,
        (47 - maize_main_tmean) / (47 - 33),
        0))))


maize_second_temp_fuzzy = ifel(
  maize_second_tmean <= 10,
  0,
  ifel(
    maize_second_tmean < 18,
    (maize_second_tmean - 10) / (18 - 10),
    ifel(
      maize_second_tmean <= 33,
      1,
      ifel(
        maize_second_tmean < 47,
        (47 - maize_second_tmean) / (47 - 33),
        0))))


sorghum_main_temp_fuzzy = ifel(
  sorghum_main_tmean <= 8,
  0,
  ifel(
    sorghum_main_tmean < 22,
    (sorghum_main_tmean - 8) / (22 - 8),
    ifel(
      sorghum_main_tmean <= 35,
      1,
      ifel(
        sorghum_main_tmean < 40,
        (40 - sorghum_main_tmean) / (40 - 35),
        0))))

sorghum_second_temp_fuzzy = ifel(
  sorghum_second_tmean <= 8,
  0,
  ifel(
    sorghum_second_tmean < 22,
    (sorghum_second_tmean - 8) / (22 - 8),
    ifel(
      sorghum_second_tmean <= 35,
      1,
      ifel(
        sorghum_second_tmean < 40,
        (40 - sorghum_second_tmean) / (40 - 35),
        0))))


# Plot trapezoidal temperature suitability functions
temp = seq(0, 50, by = 0.1)
par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))

# Maize
maize_fuzzy = pmax(0, pmin(
  1,
  (temp - 10) / (18 - 10),
  (47 - temp) / (47 - 33)))
plot(temp, maize_fuzzy,
     type = "l", lwd = 2, col = "tomato",
     xlim = c(0, 50), ylim = c(0, 1),
     xlab = "Mean temperature (°C)",
     ylab = "Suitability",
     main = "Maize")
abline(v = c(10, 18, 33, 47),
       lty = c(2, 3, 3, 2), col = "grey50")

# Sorghum
sorghum_fuzzy = pmax(0, pmin(
  1,
  (temp - 8) / (22 - 8),
  (40 - temp) / (40 - 35)))
plot(temp, sorghum_fuzzy,
     type = "l", lwd = 2, col = "tomato",
     xlim = c(0, 50), ylim = c(0, 1),
     xlab = "Mean temperature (°C)",
     ylab = "Suitability",
     main = "Sorghum")
abline(v = c(8, 22, 35, 40),
       lty = c(2, 3, 3, 2), col = "grey50")

par(mfrow = c(1, 1))


## ----------------------------------------------------------------------------------------
maize_rain_fuzzy = ifel(
  prec_annual <= 400,
  0,
  ifel(
    prec_annual < 600,
    (prec_annual - 400) / (600 - 400),
    ifel(
      prec_annual <= 1200,
      1,
      ifel(
        prec_annual < 1800,
        (1800 - prec_annual) / (1800 - 1200),
        0))))


sorghum_rain_fuzzy = ifel(
  prec_annual <= 300,
  0,
  ifel(
    prec_annual < 400,
    (prec_annual - 300) / (400 - 300),
    ifel(
      prec_annual <= 600,
      1,
      ifel(
        prec_annual < 700,
        (700 - prec_annual) / (700 - 600),
        0))))


# Plot trapezoidal rainfall suitability functions
prec = seq(0, 2000, by = 1)
par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))

# Maize
maize_rain_curve = pmax(0, pmin(
  1,
  (prec - 400) / (600 - 400),
  (1800 - prec) / (1800 - 1200)))
plot(prec, maize_rain_curve,
     type = "l", lwd = 2, col = "tomato",
     xlim = c(0, 2000), ylim = c(0, 1),
     xlab = "Annual precipitation (mm)",
     ylab = "Suitability",
     main = "Maize")
abline(v = c(400, 600, 1200, 1800),
       lty = c(2, 3, 3, 2), col = "grey50")

# Sorghum
sorghum_rain_curve = pmax(0, pmin(
  1,
  (prec - 300) / (400 - 300),
  (700 - prec) / (700 - 600)))
plot(prec, sorghum_rain_curve,
     type = "l", lwd = 2, col = "tomato",
     xlim = c(0, 2000), ylim = c(0, 1),
     xlab = "Annual precipitation (mm)",
     ylab = "Suitability",
     main = "Sorghum")
abline(v = c(300, 400, 600, 700),
       lty = c(2, 3, 3, 2), col = "grey50")

par(mfrow = c(1, 1))


## ----------------------------------------------------------------------------------------
maize_ph_fuzzy = ifel(
  ph <= 4.5,
  0,
  ifel(
    ph < 5,
    (ph - 4.5) / (5 - 4.5),
    ifel(
      ph <= 7,
      1,
      ifel(
        ph < 8.5,
        (8.5 - ph) / (8.5 - 7),
        0))))


sorghum_ph_fuzzy = ifel(
  ph <= 5,
  0,
  ifel(
    ph < 5.5,
    (ph - 5) / (5.5 - 5),
    ifel(
      ph <= 7.5,
      1,
      ifel(
        ph < 8,
        (8 - ph) / (8 - 7.5),
        0))))


# Plot trapezoidal pH suitability functions
ph_values = seq(3, 10, by = 0.01)
par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))

# Maize
maize_ph_curve = pmax(0, pmin(
  1,
  (ph_values - 4.5) / (5 - 4.5),
  (8.5 - ph_values) / (8.5 - 7)))
plot(ph_values, maize_ph_curve,
     type = "l", lwd = 2, col = "tomato",
     xlim = c(3, 10), ylim = c(0, 1),
     xlab = "Soil pH",
     ylab = "Suitability",
     main = "Maize")
abline(v = c(4.5, 5, 7, 8.5),
       lty = c(2, 3, 3, 2), col = "grey50")

# Sorghum
sorghum_ph_curve = pmax(0, pmin(
  1,
  (ph_values - 5) / (5.5 - 5),
  (8 - ph_values) / (8 - 7.5)))
plot(ph_values, sorghum_ph_curve,
     type = "l", lwd = 2, col = "tomato",
     xlim = c(3, 10), ylim = c(0, 1),
     xlab = "Soil pH",
     ylab = "Suitability",
     main = "Sorghum")
abline(v = c(5, 5.5, 7.5, 8),
       lty = c(2, 3, 3, 2), col = "grey50")

par(mfrow = c(1, 1))


## ----------------------------------------------------------------------------------------
maize_main_suitability = app(
  c(
    maize_main_temp_fuzzy,
    maize_rain_fuzzy,
    maize_ph_fuzzy),
  mean,
  na.rm = FALSE)

maize_second_suitability = app(
  c(
    maize_second_temp_fuzzy,
    maize_rain_fuzzy,
    maize_ph_fuzzy),
  mean,
  na.rm = FALSE)

# Apply seasonal calendar masks
maize_main_suitability = mask(
  maize_main_suitability,
  maize_main_valid)

maize_second_suitability = mask(
  maize_second_suitability,
  maize_second_valid)

# Combine seasons: retain the maximum suitability across available seasons
maize_suitability_ssa = ifel(
  is.na(maize_main_suitability),
  maize_second_suitability,
  ifel(
    is.na(maize_second_suitability),
    maize_main_suitability,
    ifel(
      maize_main_suitability >= maize_second_suitability,
      maize_main_suitability,
      maize_second_suitability)))

names(maize_main_suitability) = "maize_main_suitability"
names(maize_second_suitability) = "maize_second_suitability"
names(maize_suitability_ssa) = "maize_suitability"


## ----fig.width=9, fig.height=7-----------------------------------------------------------
ggplot() +
  geom_spatraster(
    data = maize_suitability_ssa) +
  geom_spatvector(
    data = ssa,
    fill = NA,
    color = "black",
    linewidth = 0.3) +
  scale_fill_viridis_c(
    option = "inferno",
    limits = c(0, 1),
    name = "Suitability",
    na.value = "transparent") +
  coord_sf() +
  labs(title = "Maize eco-physiological suitability") +
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
sorghum_main_suitability = app(
  c(
    sorghum_main_temp_fuzzy,
    sorghum_rain_fuzzy,
    sorghum_ph_fuzzy),
  mean,
  na.rm = FALSE)

sorghum_second_suitability = app(
  c(
    sorghum_second_temp_fuzzy,
    sorghum_rain_fuzzy,
    sorghum_ph_fuzzy),
  mean,
  na.rm = FALSE)

# Apply seasonal calendar masks
sorghum_main_suitability = mask(
  sorghum_main_suitability,
  sorghum_main_valid)

sorghum_second_suitability = mask(
  sorghum_second_suitability,
  sorghum_second_valid)

# Combine seasons
sorghum_suitability_ssa = ifel(
  is.na(sorghum_main_suitability),
  sorghum_second_suitability,
  ifel(
    is.na(sorghum_second_suitability),
    sorghum_main_suitability,
    ifel(
      sorghum_main_suitability >= sorghum_second_suitability,
      sorghum_main_suitability,
      sorghum_second_suitability)))

names(sorghum_main_suitability) = "sorghum_main_suitability"
names(sorghum_second_suitability) = "sorghum_second_suitability"
names(sorghum_suitability_ssa) = "sorghum_suitability"


## ----fig.width=9, fig.height=7-----------------------------------------------------------
ggplot() +
  geom_spatraster(
    data = sorghum_suitability_ssa) +
  geom_spatvector(
    data = ssa,
    fill = NA,
    color = "black",
    linewidth = 0.3) +
  scale_fill_viridis_c(
    option = "inferno",
    limits = c(0, 1),
    name = "Suitability",
    na.value = "transparent") +
  coord_sf() +
  labs(title = "Sorghum eco-physiological suitability") +
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

