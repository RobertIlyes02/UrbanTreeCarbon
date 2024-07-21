#==============================================
#   This script was used to segment trees
#        from LiDAR Point Cloud
#           Robert Ilyes - Geog 481
#==============================================

# NOTE: Ground points were removed in a separate script

# Import Libraries
library(future)
library(lidR)
library(future)
library(terra)
library(leaflet)
library(sf)

# Read folder of Las Files
ctg <- readLAScatalog("C:/Users/rober/OneDrive/Desktop/LasCatalog_GroundRemoved/")

# Set-up parallel processing 
plan(multisession, workers = 4L)
set_lidr_threads(4L)
  
# Main function used in segmenting trees
segmentation_function_map <- function(chunk){
  # Read Chunk into memory and check if empty
  chunk <- readLAS(chunk)
   if (lidR::is.empty(chunk)){
     return(NULL)
   }
  # Get Land-use file from Kitchener and clip to vegetation so only vegetation points exist
  VLUSE <- st_read("C://Users/rober/OneDrive/Documents/Geog 490/VLUSE.shp")
  VLUSETR <- st_transform(VLUSE, st_crs(chunk))
  las_clipped <- clip_roi(chunk, VLUSETR)
  
  # Filter out empty polygons and combine
  las_clipped2<-las_clipped[!sapply(las_clipped,is.null)]
  combined_las <- do.call(rbind, las_clipped2)
  print(combined_las)
  if (is.empty(combined_las)){
    return(NULL)
  }
  # Normalize and filter outliers
  nlas <- normalize_height(combined_las, knnidw())
  chunk <- filter_poi(nlas, Z >= 0 & Z < quantile(Z, .90))
  f <- function(x) {x * 0.5 + 3} # the parameters here can be adjusted to change dynamic window
  # Find Tree tops
  ttops <- locate_trees(chunk, lmf(f))
  chm <- rasterize_canopy(chunk, 0.5, p2r(subcircle = 0.2), pkg = "terra")
  # Tree Segmentation
  algo <- dalponte2016(chm, ttops)
  segmented <- segment_trees(chunk, algo)
  cr = crown_metrics(segmented, .stdmetrics_z, geom="concave")
  # Write to an output
  st_write(cr, paste0("D://LAS_REDO/", sample(1000000:10000000000,1), ".shp"))
}

# Apply the function in parallel to the las catalog
catalog_apply(ctg, segmentation_function_map)
