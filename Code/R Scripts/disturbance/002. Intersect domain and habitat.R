## script which intersects each point within the domain with the sediment map
## Takes quite a while to execute

rm(list = ls()) # reset

library(MiMeMo.tools) # everything we need
library(raster)

source("./Code/R Scripts/@_Region file.R")
domain <- readRDS("./Objects/domain/Domains.rds") # domain

# Depth per pixel
GEBCO <- raster("I:/Science/MS/users/students/Hatton_Matthew/Documents/PhD/24-25/West_Greenland_Implementation/Objects/data/GEBCO_2019.nc")

crop <- as(extent(W,E,S,N),"SpatialPolygons") #defined as (xmin,xmax,ymin,ymax) <- (W,E,S,N)
crs(crop) <- crs(GEBCO)

GEBCO <- crop(GEBCO, crop)

Depths <- GEBCO

depth_df <- as.data.frame(Depths, xy = TRUE) %>% 
  mutate(shore = case_when(
    Elevation.relative.to.sea.level >= -30 ~ "in",
    Elevation.relative.to.sea.level < -30 ~ "off",
  )) %>% 
  st_as_sf(coords = c("x","y"),crs = 4326) %>% 
  st_transform(crs = crs)# from domain script

# Read in sediment map
habitat <- read.csv("./Objects/sediment/NorthSeaSediment.csv") %>% 
  st_as_sf(coords = c("X","Y"), crs = crs)

# Intersect
intersection <- st_intersection(depth_df,habitat)

saveRDS(intersection,"./Objects/disturbance/habitat_depth intersection.RDS")
