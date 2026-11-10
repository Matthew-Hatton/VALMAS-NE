
## Set repeated commands specific to the project region
implementation <- "North_Sea"

library(sf)
# 
# #EPSG <- rgdal::make_EPSG()
# #EPSG2 <- filter(EPSG, str_detect(note, "Cape"))
crs <- 4326                                                              # Specify the map projection for the project
# These values can just be taken as the corners of the sediment map.
minmax <- read.csv("./Data/Sediment/NorthSeaRegions.csv") %>% 
  filter(Region == "North Sea")

S <- min(minmax$Latitude)
N <- max(minmax$Latitude)
W <- min(minmax$Longitude)
E <- max(minmax$Longitude)
buffer <- 3

lims <- c(xmin = W-buffer, xmax = E+buffer, ymin = S-buffer, ymax = N+buffer)# Specify limits of plotting window, also used to clip data grids

zoom <- coord_sf(xlim = c(lims[["xmin"]], lims[["xmax"]]), ylim = c(lims[["ymin"]], lims[["ymax"]]), expand = FALSE) # Specify the plotting window for SF maps in this region

ggsave_map <- function(filename, plot) {
  ggsave(filename, plot, scale = 1, width = 12, height = 10, units = "cm", dpi = 500, bg = "white")

}                             # Set a new default for saving maps in the correct size
# pre <- list(scale = 1, width = 12, height = 10, units = "cm", dpi = 500) # The same settings if you need to pass them to a function in MiMeMo.tools

SDepth <- 30                  # Shallow deep boundary
DDepth <- 81                 # Maximum depth
# 
# #### bathymetry.5 MODEL DOMAIN ####
#
shape <- function(matrix, label = "DUMMY") {

  shape <-  matrix %>%
    list() %>%
    st_polygon() %>%
    st_sfc() %>%
    st_sf(Region = label, .)
  st_crs(shape) <- st_crs(4326)
  shape <- st_transform(shape, crs = crs)
  return(shape)

}                      # Convert a matrix of lat-lons to an sf polygon

sed_pts  <- st_as_sf(minmax, coords = c("Longitude", "Latitude"), crs = 4326)
sed_poly <- st_concave_hull(st_union(sed_pts), ratio = 0.05) %>% 
  st_sf(Region = implementation, geometry = .)
# 
# ggplot(Region_mask)+geom_sf()
# 
# #### bounds.2 MAKE TRANSECTS ####
# 
# ## Polygons to mark which transects are along the open ocean-inshore boundary
# 
# Inshore_Ocean1 <- matrix(c(27.98, 27.98, 28.02, 28.02, 27.98,    # Longitudes
#                            -38.5, -32, -32, -38.5, -38.5), ncol = 2, byrow = F) %>% 
#   shape()
# 
# Inshore_Ocean2 <- matrix(c(14.3, 14.55, 16.3, 16.9, 16.8, 14.3,               # Longitudes
#                            -30, -30,   -28.9, -28.1, -28.1, -30), ncol = 2, byrow = F) %>% 
#   shape()
# 
# Inshore_ocean_boundaries <- rbind(Inshore_Ocean1, Inshore_Ocean2)
# 
# rm(Inshore_Ocean1, Inshore_Ocean2)
# 
# #### expand polygon for sampling rivers ####
# 
# river_expansion <- matrix(c(13, 73,
#                             0, 80,
#                             0, 85,
#                             63, 85,
#                             73, 77,
#                             30, 71,
#                             13, 73),
#                           ncol = 2, byrow = T) %>% 
#   list() %>% 
#   st_polygon() %>% 
#   st_sfc() %>% 
#   st_sf(Region = implementation,.)
# st_crs(river_expansion) <- st_crs(4326)                                          
# river_expansion <- st_transform(river_expansion, crs = crs)
# 
