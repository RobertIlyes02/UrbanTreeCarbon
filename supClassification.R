library(sf)
library(ggplot2)
library(dplyr)
library(terra)
library(rpart)
library(raster)
library(randomForest)
library(dismo)

# Load tree points and imagery
samples <- st_read("../treeMay2018AccAssessActive.shp", crs = st_crs(26917))
plabs <- rast('../plabs_clip.tif')
names(plabs) <- c("Blue", "Green", "Red", "NIR")

# Make training points
randomSample <- st_sample(samples, 140, type = "random")
str(randomSample)
randomSample <- st_cast(randomSample, "POINT")
randomSamplePoints <- st_geometry(randomSample)
randomSamplePoints <- st_as_sf(randomSamplePoints, crs = st_crs(samples))
randomSamplePoints <- st_join(randomSamplePoints, samples)

# Extract values for training sites
sampvals <- extract(plabs, randomSamplePoints, bands = c("Blue", "NIR"), df = TRUE)
sampvals <- sampvals[, -1]
sampdata <- data.frame(Classvalue = randomSamplePoints$Classvalue, sampvals[, c("Blue", "NIR")])

# Train the model and predict
cart <- rpart(as.factor(Classvalue)~., data=sampdata, method = 'class', minsplit = 5)
classified <- predict(plabs, cart, type='class')
plot(classified)


# # Plot the trained classification tree
# plot(cart, uniform=TRUE, main="Classification Tree")
# text(cart, cex = 0.8)

# spectral separability
# spectral_data <- cbind(randomSamplePoints$Classvalue, sampvals)
# spectral_data <- as.data.frame(spectral_data)
# colnames(spectral_data) <- c("Classvalue", "Blue", "Green", "Red", "NIR")
# mean_spectral <- aggregate(. ~ Classvalue, data = spectral_data, FUN = mean)

## for testing separability with whole dataset
# spectral_data <- cbind(samples$Classvalue, sampvals)
# spectral_data <- as.data.frame(spectral_data)
# colnames(spectral_data) <- c("Classvalue", "Blue", "Green", "Red", "NIR")
# mean_spectral <- aggregate(. ~ Classvalue, data = spectral_data, FUN = mean)

## plot separability
# ggplot(mean_spectral, aes(x = Green, y = NIR, color = factor(Classvalue))) +
#   geom_point(size = 3) +
#   scale_color_manual(values = c("blue", "red", "green", "purple", "orange", "black", "pink", "grey", "brown", "navy", "yellow", "violet")) +  # Adjust colors as needed
#   labs(x = "Green Band", y = "NIR Band", color = "Class") +
#   ggtitle("Spectral Separability: Blue vs Green Bands")

## accuracy assessment
# j <- kfold(sampdata, k = 5, by=sampdata$Classvalue)
# table(j)
# x <- list()
# for (k in 1:5) {
#   train <- sampdata[j!= k, ]
#   test <- sampdata[j == k, ]
#   cart <- rpart(as.factor(Classvalue)~., data=train, method = 'class', minsplit = 5)
#   pclass <- predict(cart, test, type='class')
#   # create a data.frame using the reference and prediction
#   x[[k]] <- cbind(test$Classvalue, as.integer(pclass))
# }
# y <- do.call(rbind, x)
# y <- data.frame(y)
# colnames(y) <- c('observed', 'predicted')
# conmat <- table(y)
# n <- sum(conmat)
# n
# diag <- diag(conmat)
# OA <- sum(diag) / n
# OA
# conmat

## write raster to file
writeRaster(classified, filename="classified2.tif", overwrite=TRUE)