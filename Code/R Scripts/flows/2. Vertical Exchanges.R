
# Summarise the data extracted from NEMO-MEDUSA, dealing with deep convection issues
# readRDS("./Objects/vertical boundary/.")  # Marker so network script can see where the data is being pulled from

#### Setup ####

rm(list=ls())                                                                   # Wipe the brain

Packages <- c("tidyverse", "data.table", "furrr" ,"sf")                         # List packages
lapply(Packages, library, character.only = TRUE)                                # Load packages
source("./Code/R Scripts/@_Region file.R")

plan(multisession,workers = availableCores() - 2)

offshore <- readRDS("./Objects/domain/Domains.rds") %>% 
  filter(Shore == "Offshore")

deep_convection_is <- 0.14                                                      # Threshold above which vertical diffusivity = deep convection

data <- list.files("./Objects/vertical boundary/", full.names = T) %>% # Import data
  future_map(readRDS) %>% 
  rbindlist()

#### Vertical velocities ####

sf_use_s2(F)

samples <- filter(data, Year == 2015, Month == 1 ) %>% 
  st_as_sf(coords = c("longitude", "latitude"), remove = F)

area <- st_union(samples) %>%                                               # Combine              
  st_voronoi() %>%                                                          # And create a voronoi tesselation
  st_collection_extract(type = "POLYGON") %>%                               # Expose the polygons
  sf::st_sf() %>%                                                           # Reinstate sf formatting
  st_join(samples) %>%                                                       # Rejoin meta-data from points
  arrange(x, y) %>%                                                         # Order the polygons to match the points
  st_set_crs(4326) %>% 
  st_transform(crs = st_crs(offshore)) %>% 
  st_make_valid() %>% 
  st_intersection(offshore) %>% 
  mutate(area_m2 = as.numeric(st_area(.))) %>% 
  dplyr::select(x, y, area_m2)

rm(samples)

ggplot(area) +                                                              # Check the polygons match correctly with points
  geom_sf(aes(fill = area_m2), size = 0.05, colour = "white") +
  theme_minimal() +
  NULL

area <- st_drop_geometry(area)

gc()
gc()

exchanges <- dplyr::select(data, Vertical_velocity, Year, Month, x, y, Forcing, SSP) %>%   # Discard excess variables
  mutate(Direction = ifelse(Vertical_velocity > 0, "Upwelling", "Downwelling")) %>% # Identify upwelling and downwelling
  filter(between(Year,start_year,end_year)) %>% 
  left_join(area) %>% 
  mutate(Vertical_velocity = abs(Vertical_velocity)*area_m2) %>%                    # Scale up flow rate to volume of water
  group_by(Year, Month, Direction, Forcing, SSP, x, y) %>%                          # Average across days in a month per pixel
  summarise(Vertical_velocity = mean(Vertical_velocity, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(Year, Month, Direction, Forcing, SSP) %>%                                # Create a monthly time series
  summarise(Vertical_velocity = sum(Vertical_velocity, na.rm = T)) %>% 
  ungroup() %>% 
  pivot_wider(c(Year, Month, Forcing, SSP), names_from = Direction, values_from = Vertical_velocity)

saveRDS(exchanges, "./Objects/chemistry/SO_DO exchanges.rds")