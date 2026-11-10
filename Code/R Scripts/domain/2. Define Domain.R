# Create an object defining the geographic extent of the model domain

#### Set up ####

rm(list=ls())                                                   

Packages <- c("tidyverse", "sf", "stars", "rnaturalearth", "raster")        # List handy packages
lapply(Packages, library, character.only = TRUE)                            # Load packages

source("./Code/R scripts/@_Region file.R")                                       # Define project region

world <- ne_countries(scale = "medium", returnclass = "sf") %>%             # Get a world map
  st_transform(crs = crs)                                                   # Assign polar projection

# this was stored for PhD and is a large file
GEBCO <- raster("I:/Science/MS/users/students/Hatton_Matthew/Documents/PhD/24-25/West_Greenland_Implementation/Objects/data/GEBCO_2019.nc")
crop  <- as(extent(W, E, S, N), "SpatialPolygons")
crs(crop) <- crs(GEBCO)
GEBCO <- crop(GEBCO, crop)

sf_use_s2(FALSE)

sed_pts <- st_as_sf(distinct(minmax, Longitude, Latitude, Zone),
                    coords = c("Longitude", "Latitude"), crs = 4326)
hull <- st_concave_hull(st_union(sed_pts), ratio = 0.05)

cells <- st_voronoi(st_union(sed_pts)) %>% 
  st_collection_extract("POLYGON") %>% 
  st_sf(geometry = .) %>% 
  st_join(sed_pts["Zone"])

clipped <- st_intersection(cells, hull) %>% 
  group_by(Zone) %>% 
  summarise() %>% 
  st_make_valid()

Domains <- clipped %>% 
  mutate(Shore     = Zone,
         area      = as.numeric(st_area(.)),
         Elevation = exactextractr::exact_extract(GEBCO, ., "mean")) %>% 
  st_transform(crs = crs)

saveRDS(Domains, "./Objects/domain/Domains.rds")

map <- ggplot() + 
  geom_sf(data = Domains, aes(fill = Shore), colour = NA) +
  #  geom_sf(data = Region_mask, colour = "red", fill = NA) + 
  geom_sf(data = world, size = 0.1, fill = "black") +
  scale_fill_manual(values = c(Inshore = "yellow", Offshore = "yellow3"), name = "Zone") +
  zoom +
  theme_minimal() +
  #  theme(axis.text = element_blank()) +
  labs(caption = "Final model area") +
  NULL
map
ggsave_map("./Figures/domain/Domains.png", map)