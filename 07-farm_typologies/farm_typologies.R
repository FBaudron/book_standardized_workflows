## ----message=FALSE, warning=FALSE--------------------------------------------------------
if (!require('openxlsx')) install.packages("openxlsx")
if (!require('cluster')) install.packages("cluster")
if (!require('gtsummary')) install.packages("gtsummary")
if (!require('kableExtra')) install.packages("kableExtra")
if (!require('splitstackshape')) install.packages("splitstackshape")
if (!require('randomForest')) install.packages("randomForest")

library(openxlsx)
library(cluster)
library(gtsummary)
library(kableExtra)
library(splitstackshape)
library(randomForest)


## ----------------------------------------------------------------------------------------
data = readRDS("small_dataset.rds")
# data = read.xlsx("small_dataset.xlsx", sheet = 1)


## ----------------------------------------------------------------------------------------
names(data)[c(1, 4:6, 27, 29, 33:36)] = c("ID", "age_hhh", "sex_hhh", "edu_hhh", "cover_crops", "integrated_pest_management", "homemade_feeds", "fodder_production", "fodder_preservation", "survival_feeding")

data$sex_hhh = as.numeric(data$sex_hhh == "Female")

data$edu_hhh = as.numeric(data$edu_hhh != "Primary level")

data$main_food_source = as.numeric(data$main_food_source == "Own production")

data$main_income_source = as.numeric(data$main_income_source == "Crop sales" | data$main_income_source == "Livestock sales")

data[c(23:36)][is.na(data[c(23:36)])] = 0


## ----fig.width=4, fig.height=4-----------------------------------------------------------
boxplot(data$age_hhh)


## ----fig.width=4, fig.height=4-----------------------------------------------------------
boxplot(data$family_size)


## ----fig.width=4, fig.height=4-----------------------------------------------------------
data = data[data$family_size < 20,]
boxplot(data$family_size)


## ----------------------------------------------------------------------------------------
# boxplot(data$cropped_area)

# boxplot(data$fallow)

# boxplot(data$prop_non_cereals)

# boxplot(data$fertilizers)

# boxplot(data$amendment)

# boxplot(data$cattle)

# boxplot(data$small_rum)
data = data[data$small_rum < 100,]
# boxplot(data$small_rum)

# boxplot(data$poultry)
data = data[data$poultry < 400,]
# boxplot(data$poultry)

# boxplot(data$food_security)

# boxplot(data$hdds)

# boxplot(data$tot_div)

# boxplot(data$cereal_prod)

# boxplot(data$offtake)
data = data[data$offtake < 6,]
# boxplot(data$offtake)

# boxplot(data$eq_value)
data = data[data$eq_value < 8000,]
# boxplot(data$eq_value)


## ----------------------------------------------------------------------------------------
typo = data


## ----fig.width=4, fig.height=4-----------------------------------------------------------
hist(typo$age_hhh)


## ----fig.width=4, fig.height=4-----------------------------------------------------------
hist(typo$family_size)


## ----fig.width=4, fig.height=4-----------------------------------------------------------
typo$family_size = log10(typo$family_size + (0.5 * min(typo$family_size[typo$family_size > 0], na.rm = TRUE)))
hist(typo$family_size)


## ----------------------------------------------------------------------------------------
# hist(typo$cropped_area)
typo$cropped_area = log10(typo$cropped_area + (0.5 * min(typo$cropped_area[typo$cropped_area > 0], na.rm = TRUE)))
# hist(typo$cropped_area)

# hist(typo$fallow)
typo$fallow = log10(typo$fallow + (0.5 * min(typo$fallow[typo$fallow > 0], na.rm = TRUE)))
# hist(typo$fallow)

# hist(typo$prop_non_cereals)

# hist(typo$fertilizers)
typo$fertilizers = log10(typo$fertilizers + (0.5 * min(typo$fertilizers[typo$fertilizers > 0], na.rm = TRUE)))
# hist(typo$fertilizers)

# hist(typo$amendment)
typo$amendment = log10(typo$amendment + (0.5 * min(typo$amendment[typo$amendment > 0], na.rm = TRUE)))
# hist(typo$amendment)

# hist(typo$cattle)
typo$cattle = log10(typo$cattle + (0.5 * min(typo$cattle[typo$cattle > 0], na.rm = TRUE)))
# hist(typo$cattle)

# hist(typo$small_rum)
typo$small_rum = log10(typo$small_rum + (0.5 * min(typo$small_rum[typo$small_rum > 0], na.rm = TRUE)))
# hist(typo$small_rum)

# hist(typo$poultry)
typo$poultry = log10(typo$poultry + (0.5 * min(typo$poultry[typo$poultry > 0], na.rm = TRUE)))
# hist(typo$poultry)

# hist(typo$food_security)

# hist(typo$hdds)

# hist(typo$tot_div)

# hist(typo$cereal_prod)
typo$cereal_prod = log10(typo$cereal_prod + (0.5 * min(typo$cereal_prod[typo$cereal_prod > 0], na.rm = TRUE)))
# hist(typo$cereal_prod)

# hist(typo$offtake)
typo$offtake = log10(typo$offtake + (0.5 * min(typo$offtake[typo$offtake > 0], na.rm = TRUE)))
# hist(typo$offtake)

# hist(typo$eq_value)
typo$eq_value = log10(typo$eq_value + (0.5 * min(typo$eq_value[typo$eq_value > 0], na.rm = TRUE)))
# hist(typo$eq_value)


## ----------------------------------------------------------------------------------------
typo = na.omit(typo)


## ----------------------------------------------------------------------------------------
typology_vars <- c(
  "age_hhh", "sex_hhh", "edu_hhh", "family_size",
  "cropped_area", "fallow", "prop_non_cereals",
  "fertilizers", "amendment", "cattle", "small_rum", "poultry",
  "food_security", "hdds", "tot_div", "cereal_prod", "offtake",
  "eq_value", "bee_hives", "ind_garden", "com_garden",
  "main_food_source", "main_income_source",
  "community_seed_banks", "small_grains", "crop_rotation",
  "intercropping", "cover_crops", "mulching",
  "integrated_pest_management", "compost_manure",
  "tree_planting_on_farm",
  "tree_retention_natural_regeneration_on_farm",
  "homemade_feeds", "fodder_production",
  "fodder_preservation", "survival_feeding")

asymmetric_binary_vars <- c(
  "bee_hives", "ind_garden", "com_garden",
  "community_seed_banks", "small_grains", "crop_rotation",
  "intercropping", "cover_crops", "mulching",
  "integrated_pest_management", "compost_manure",
  "tree_planting_on_farm",
  "tree_retention_natural_regeneration_on_farm",
  "homemade_feeds", "fodder_production",
  "fodder_preservation", "survival_feeding")

diss <- cluster::daisy(
  typo[, typology_vars],
  metric = "gower",
  type = list(asymm = asymmetric_binary_vars))


## ----------------------------------------------------------------------------------------
pco <- cmdscale(diss, eig = TRUE, k = 10)

# Inspect the eigenvalues
barplot(pco$eig[1:20])

# Retain enough axes to account for at least 50% of the variation
cum_var <- cumsum(pco$eig) / sum(pco$eig)
k_axes <- which(cum_var >= 0.5)[1]
k_axes

pco_var <- pco$points[, 1:k_axes, drop = FALSE]


## ----------------------------------------------------------------------------------------
k_candidates <- 3:6
sil_width <- numeric(length(k_candidates))
names(sil_width) <- k_candidates

for (k in k_candidates) {
  pam_fit <- cluster::pam(
    dist(pco_var),
    k = k,
    diss = TRUE
  )
  sil_width[as.character(k)] <- pam_fit$silinfo$avg.width
}

best_k <- as.integer(names(which.max(sil_width)))
best_k

plot(
  k_candidates,
  sil_width,
  type = "b",
  pch = 19,
  xlab = "Number of farm types",
  ylab = "Average silhouette width")
abline(v = best_k, col = "red", lty = 2)


## ----------------------------------------------------------------------------------------
final_pam <- cluster::pam(
  pco_var,
  k = best_k,
  diss = FALSE)

grpPCO <- final_pam$clustering
typo$type <- factor(grpPCO)

# Add farm type to the original dataset
data <- merge(data, typo[, c("ID", "type")], by = "ID")


## ----------------------------------------------------------------------------------------
plot(pco$points[, 1], pco$points[, 2], col = grpPCO,
     xlab = "PCO1", ylab = "PCO2")
plot(pco$points[, 1], pco$points[, 3], col = grpPCO,
     xlab = "PCO1", ylab = "PCO3")
plot(pco$points[, 2], pco$points[, 3], col = grpPCO,
     xlab = "PCO2", ylab = "PCO3")


## ----------------------------------------------------------------------------------------
data$type = as.factor(data$type)

rf_type = randomForest(type ~ ., data = data[, -c(1:3)], ntree = 1500)
print(rf_type)


## ----fig.width = 10, fig.height = 8, dpi = 300-------------------------------------------
varImpPlot(rf_type)


## ----message=FALSE, warning=FALSE--------------------------------------------------------
data[, -c(1:3)] %>%
  tbl_summary(
    by = type,
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{p}%"),
    type = list(hdds ~ "continuous"),
    digits = all_continuous() ~ 1,
    label = list(age_hhh ~  "Age of the head of the household",
                 sex_hhh ~  "Female-headed households",
                 edu_hhh ~  "Education of the head of the household higher than primary",
                 family_size ~  "Family size",
                 eq_value ~  "Equipment value (USD)",
                 cropped_area ~  "Total cropped area (ha)",
                 fallow ~ "Fallow land (ha)",
                 prop_non_cereals ~  "Non-cereal crops (% total cropped area)",
                 fertilizers ~  "Total fertilizer used (kg)",
                 amendment ~  "Total manure + compost used (kg)",
                 cattle ~  "Cattle (n)",
                 small_rum ~  "Small ruminants (n)",
                 poultry ~  "Poultry (n)",
                 bee_hives ~  "Bee keeping",
                 ind_garden ~  "Owning an individual garden",
                 com_garden ~  "Having access to a communal garden",
                 main_food_source ~  "Own production as main source of food",
                 main_income_source ~  "Farming as main source of income",
                 food_security ~ "Proportion of the year being food secured",
                 hdds ~  "24H household dietary diversity score (0-12)",
                 community_seed_banks ~  "Using a community seed bank",
                 small_grains ~  "Using small grains",
                 crop_rotation ~  "Using crop rotation",
                 intercropping ~  "Using intercropping",
                 cover_crops ~  "Using cover crops",
                 mulching ~  "Using mulching",
                 integrated_pest_management ~  "Using integrated pest management",
                 compost_manure ~  "Using compost and manure",
                 tree_planting_on_farm ~  "Planting trees on-farm",
                 tree_retention_natural_regeneration_on_farm	 ~  "Retaining naturally regenerated trees on-farm",
                 homemade_feeds ~  "Using homemade animal feed",
                 fodder_production ~  "Produccing fodder",
                 fodder_preservation ~  "Preserving fodder",
                 survival_feeding ~  "Using survival feeding",
                 tot_div ~  "Total farm diversity (n species)",
                 cereal_prod ~  "Total cereal production (kg/yr)",
                 offtake ~  "Total livestock offtake (TLU/yr)"))%>%
  add_p(test = list(all_continuous() ~ "kruskal.test",
                    all_categorical() ~ "chisq.test"))%>%
  add_overall()%>%
  as_kable(format = "latex", booktabs = TRUE, longtable = TRUE)%>%
  kable_styling(latex_options = c("scale_down","repeat_header"), font_size = 6)%>%
  landscape()


## ----------------------------------------------------------------------------------------
medoid_farms <- data.frame(
  ID = typo$ID[final_pam$id.med],
  type = factor(
    final_pam$clustering[final_pam$id.med],
    levels = sort(unique(final_pam$clustering))))

medoid_farms



## ----------------------------------------------------------------------------------------
most_representative_farms <- merge(
  medoid_farms,
  data,
  by = c("ID", "type"),
  sort = FALSE)

# Transpose the table
farm_table <- as.data.frame(
  t(as.matrix(
    most_representative_farms %>%
      dplyr::select(-ID))),
  check.names = FALSE)

# Assign farm IDs to columns
colnames(farm_table) <- paste0(
  "Farm ",
  most_representative_farms$ID)

knitr::kable(
  farm_table,
  format = "latex",
  booktabs = TRUE,
  row.names = TRUE,
  col.names = c("", colnames(farm_table)),
  caption = "Characteristics of the most representative farms",
  align = "lccc",
  escape = TRUE) %>%
  kableExtra::kable_styling(
    latex_options = "hold_position",
    position = "center",
    font_size = 8,
    full_width = FALSE) %>%
  kableExtra::column_spec(
    1,
    width = "5cm",
    bold = FALSE) %>%
  kableExtra::row_spec(
    0,
    bold = TRUE)


## ----------------------------------------------------------------------------------------
sample1 = as.data.frame(stratified(data, "type", 30/nrow(data)))
table(sample1$type)


## ----------------------------------------------------------------------------------------
sample2 = as.data.frame(stratified(data, c("ward", "type"), 30/nrow(data)))
table(sample2$type, sample2$ward)

