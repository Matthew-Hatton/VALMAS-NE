rm(list = ls())

library(tidyverse)
library(sf)

domain <- read.csv("./Data/Sediment/NorthSeaRegions.csv") %>%
  filter(Region == "North Sea") %>%
  dplyr::select(Longitude, Latitude, Zone) %>%
  mutate(Shore = Zone) %>%
  dplyr::select(-Zone) %>%
  st_as_sf(
    coords = c("Longitude", "Latitude"),
    crs = 4326
  ) %>% 
  group_by(Shore) %>% 
  summarise(
    geometry = st_union(geometry),
    .groups = "drop"
  ) %>% 
  mutate(Area = st_area(geometry))

ggplot() +
  geom_sf(data = domain,aes(color = Shore))
