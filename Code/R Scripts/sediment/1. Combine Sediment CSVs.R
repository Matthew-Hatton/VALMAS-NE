rm(list = ls())

library(tidyverse)
library(sf)
library(raster)
library(rnaturalearth)

# map of the UK and Ireland
uk <- ne_countries(country = "United Kingdom", returnclass = "sf",scale = 10)
# uk_regions <- ne_states(country = "United Kingdom", returnclass = "sf")
ireland <- ne_countries(country = "Ireland", returnclass = "sf",scale = 10)

# read csvs
rock <- read.csv("./Data/Sediment/NorthSeaRock.csv") %>%
  filter(Rock >= 0.5) %>%
  st_as_sf(
    coords = c("Longitude", "Latitude"),
    crs = 4326
  ) %>%
  mutate(Type = "Rock (≥0.5)")

sediment <- read.csv("./Data/Sediment/NorthSeaRegions.csv") %>% 
  filter(Region == "North Sea") %>% 
  st_as_sf(coords = c(longitude = "Longitude",
                      latitude = "Latitude"),
           crs = 4326)

# Retain rock points that overlap the North Sea sediment domain
rock <- rock[lengths(st_intersects(rock, sediment)) > 0, ]

# Remove sediment pixels where dominant rock occurs
overlap <- st_intersects(sediment, rock)

sediment_filtered <- sediment[lengths(overlap) == 0, ]

# Combine
sediment_final <- bind_rows(
  rock,
  sediment_filtered
)

# Extract coordinates from sf geometry
sediment_plot <- sediment_final %>%
  mutate(
    Longitude = st_coordinates(.)[, 1],
    Latitude = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry()

# Plot as raster
p <- ggplot() +
  geom_raster(
    data = sediment_plot,
    aes(
      x = Longitude,
      y = Latitude,
      fill = Type
    )
  ) +
  geom_sf(data = uk, fill = "grey95", colour = "black") +
  geom_sf(data = ireland, fill = "grey95", colour = "black") +
  scale_fill_viridis_d(
    name = "Sediment type",
    na.value = "transparent"
  ) +
  coord_sf(
    crs = 4326,
    default_crs = 4326
  ) +
  theme_minimal() +
  labs(
    x = "Longitude",
    y = "Latitude"
  )

ggsave(
  "./Figures/Sediment/CombinedSedimentMap.png",
  p,
  width = 10,
  height = 8,
  dpi = 300
)
