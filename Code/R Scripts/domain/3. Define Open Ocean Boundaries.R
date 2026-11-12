rm(list = ls())

library(rnaturalearth)
library(tidyverse)
library(sf)

source("./Code/R Scripts/@_Region file.R")

world <- ne_countries(scale = "medium", returnclass = "sf") %>%             # Get a world map
  st_transform(crs = crs)  

Domains <- readRDS("./Objects/domain/Domains.RDS")

## define open ocean inshore boundaries
inshore_polygon_south <- matrix(c(
  1.0, 51,
  1.0,50.85,
  1.55, 50.75,
  1.7, 50.90,
  1.0, 51
), ncol = 2, byrow = TRUE) %>% 
  shape()

inshore_polygon_north <- matrix(c(
  8.075, 56.95,
  8.075, 57.2,
  8.30, 57.20,
  8.30, 56.95,
  8.075, 56.95
), ncol = 2, byrow = TRUE) %>%
  shape()

inshore_polygon <- rbind(inshore_polygon_south,inshore_polygon_north)

map <- ggplot() + 
  geom_sf(data = Domains, aes(fill = Shore), colour = NA) +
  #  geom_sf(data = Region_mask, colour = "red", fill = NA) + 
  geom_sf(data = world, size = 0.1, fill = "black") +
  geom_sf(data = inshore_polygon) +
  scale_fill_manual(values = c(Inshore = "yellow", Offshore = "yellow3"), name = "Zone") +
  zoom +
  theme_minimal() +
  #  theme(axis.text = element_blank()) +
  labs(caption = "Final model area") +
  NULL
map

