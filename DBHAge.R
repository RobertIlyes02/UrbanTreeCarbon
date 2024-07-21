#==============================================
#   This script was used to create and apply
#        Age ~ Height Regression Models
#           Robert Ilyes - Geog 481
#==============================================

# Note: Code be skipped until line 84, everything above it was exploratory

# Import Libraries
library(ggplot2)
library(dplyr)
library(randomForest)
library(plotly)
library(terra)

# Import Trees
trees = read.csv("C://Users/robik/Downloads/outputTreesAgeMergedCleaned.csv")


# ======================================
# Random Forest Implementation
rf = randomForest(AGE~zq90+SPECIES_NA.x, data=trees3)

# ======================================
# Create 
k2024 = read.csv("C://Users/rober/Downloads/Kitchener_TreeInventory_GeoHub.csv")
k2018 = read.csv("C://Users/robik/Documents/k2018Joined.csv")
# Merge trees with street inventory
trees2 = merge(k2018, trees, by="TREEID")
trees2$height = trees2$Estimated.tree.height.from.2019.LiDAR..m.
Regression_V4 = lm(AGE ~ height, data=trees4)
summary(Regression_V4)
plot(trees2$height, trees2$AGE)
abline(regressuib_V4)

# Remove outliers and create new regression
trees3 = trees2 %>% filter(AGE < 100) %>% filter(height < 30)
Regression_V5 = lm(AGE ~ height, data=trees3)
summary(Regression_V5)
plot(trees3$height, trees3$AGE)
abline(Regression_V5)

# Try and use different families
Regression_V6 = glm(AGE~height, family = inverse.gaussian(), data=trees2)
summary(Regression_V6$r.squared)
1 - Regression_V6$deviance/Regression_V6$null.deviance

# ====================================
# Best model yet
k2018joined = read.csv("C://Users/rober/OneDrive/Documents/k2018_Joined.csv")
trees = read.csv("C://Users/rober/Downloads/outputTreesAgeMergedCleaned.csv")
trees2 = merge(trees, k2018joined, by="TREEID")
trees3 = trees3 %>% filter(zq90 > 2)
lm3 = lm(AGE~zq90, data=trees3)

# =====================================
# Create models for each species
fit_model <- function(data){
  lm(AGE~zq90, data=data) # Turns out we can't use zq90 because it's not included in all records
}

get_r_squared <- function(model){
  summary(model)$r.squared
}

split_trees = split(trees3, trees3$SPECIES_NA.x)
models = lapply(split_trees, fit_model)
rsq = lapply(get_r_squared, models)

# =======================================
# Plot all the species
ggplot(trees3, aes(x=zmean, y=AGE, color=species_group))+geom_point()
species = unique(trees4$SPECIES_NA.x)
rsquares = c()
for (i in 1:length(species)){
  new = trees4 %>% filter(SPECIES_NA.x == species[i])
  plot(new$zmean, new$AGE, main=species[i])
  print(species[i])
  summ = summary(lm(AGE~zmean, data=new))
  rsquares = rbind(c(species[i], summ$r.squared, summ$df[2]), rsquares)
}

# =======================================
# PART USED FOR FINAL MODEL
# Attempt #4 of regression models
trees = read.csv("C://Users/robik/Downloads/outputTreesAgeMergedCleaned.csv")
k2018 = read.csv("C://Users/robik/Documents/k2018Joined.csv")
k2018joined = read.csv("C://Users/robik/Documents/k2018Joined.csv")
trees2 = merge(trees, k2018joined, by="TREEID")
trees4 = trees2 %>% filter(AGE < 100) %>% filter(zmean < 30) 

lm1 = lm(AGE~zmean, data=trees3)
lm2 = lm(AGE~zmean+SPECIES_NA.x, data=trees3)
lm_zmean = lm(AGE~zmean+species_group, data=trees3) 

rf = randomForest(AGE~zmean+SPECIES_NA.x, data=trees3)


# =======================================
# Try applying it to Individual tree dataset
indtrees3 = vect("C://Users/robik/Downloads/IndividualTreesNew/indivTreesExtract.shp")
indtreesages_lm1 = predict(lm1, indtrees2)
indtreesage_rf = 
indtrees2$AGE = indtreesages_lm1
plot_ly(x=indtrees2$DBH, y=indtrees2$zmean, z=indtrees2$AGE)

# =======================================
# This section is used to train new data using regression models created above


# Define species groups, because not all species are included in both training and testing datasets
maples <- c("Sugar Maple", "Norway Maple Species", "Soft Maple*", "Maple", "Red Maple", "Norway Maple",
            "Columnare Maple", "Norway Maple Red Variety", "Globe Norway Maple", "Autumn Blaze Maple",
            "Black Maple", "Green Mountain Sugar Maple", "Schwedleri Norway Maple", "Crimson King Maple",
            "Greencolumn Black Maple", "Legacy Sugar Maple", "Scarlet Maple", "Bonfire Sugar Maple",
            "Armstrong Maple", "Pacific Sunset Maple", "October Glory Red Maple", "Bowhall Red Maple")
ashes <- c("White Ash", "Green Ash Species", "Green Ash", "Black Ash", "Blue Ash", "Carolina Blue Ash", "Ash Species")
locusts <- c("Honeylocust Species", "Shademaster Locust", "Black Locust", "Sunburst Locust", "Skyline Locust", "Street Keeper Honey Locust", "Imperial Locust")
oaks <- c("Northern Red Oak", "Pin Oak")
pines <- c("Austrian Pine", "Eastern White Pine")
spruces <- c("Blue Spruce", "Spruce")

indtrees3df = as.data.frame(fortrees$NewSpecies)

trees4 <- trees4 %>%
  mutate(species_group = case_when(
    SPECIES_NA.x %in% maples ~ "Maples",
    SPECIES_NA.x %in% ashes ~ "Ashes",
    SPECIES_NA.x %in% locusts ~ "Locusts",
    SPECIES_NA.x %in% oaks ~ "Oaks",
    SPECIES_NA.x %in% pines ~ "Pines",
    SPECIES_NA.x %in% spruces ~ "Spruces",
    TRUE ~ "Others"
  ))

species_groups <- indtrees3df %>%
  mutate(species_group = case_when(
    fortrees$NewSpecies %in% maples ~ "Maples",
    fortrees$NewSpecies %in% ashes ~ "Ashes",
    fortrees$NewSpecies %in% locusts ~ "Locusts",
    fortrees$NewSpecies %in% oaks ~ "Oaks",
    fortrees$NewSpecies %in% pines ~ "Pines",
    fortrees$NewSpecies %in% spruces ~ "Spruces",
    TRUE ~ "Others"
  ))

#So far this is best that we can use
lm_zmean = lm(AGE~zmean+species_group, data=trees3) 
lm_zmax = lm(AGE~zmax+species_group, data=trees3)

lm_zmean2 = lm(AGE~zmean, data=trees3) 
lm_zmax2 = lm(AGE~zmax, data=trees3)

# Predict new values using all 4 regression models
fortrees$species_group = species_groups$species_group
predict_zmean = predict(lm_zmean, fortrees)
predict_zmax = predict(lm_zmax, fortrees)
predict_zmean2 = predict(lm_zmean2, fortrees)
predict_zmax2 = predict(lm_zmax2, fortrees)
predict_lm1 = predict(lm1, fortrees)
fortrees$AGE_zmean_species = predict_zmean
fortrees$AGE_zmax_species = predict_zmax
fortrees$AGE_zmean = predict_zmean2
temp = indtrees3
temp$zmax = temp$Height
fortrees$AGE_zmax = predict_zmax2


# This is even better
lm4 = lm(AGE~zmean+SPECIES_NA.x+WARD, data=trees3)

# try it with RF .. somehow it's worse than before
rf2 = randomForest(AGE~zmean+species_group+WARD, data=trees3)

# Try with less numbers
test = trees3 %>% filter(zmean < 15)
testlm = lm(AGE~zmean+species_group+WARD, data=test)

# Plot just for fun
plot_ly(x=testsample$zmean, y=testsample$DBH, z=testsample$AGE, color=testsample$species_group)

# =======================================
# Get R-Squared Value from nls
predicted_values <- predict(nlsLM_model)

# Calculate the total sum of squares (SST)
sst <- sum((trees3$AGE - mean(trees3$AGE))^2)

# Calculate the residual sum of squares (SSR)
ssr <- sum((trees3$AGE - predicted_values)^2)

# Calculate the R-squared value
r_squared <- 1 - (ssr / sst)

