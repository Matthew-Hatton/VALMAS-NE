#### Set up ####
rm(list = ls())

packages <- c("tidyverse", "data.table", "ncdf4", "sf", "rnaturalearth")
invisible(lapply(packages, library, character.only = TRUE))
sf_use_s2(FALSE)   # treat lon/lat as planar (avoids s2 issues with hulls)

source("./Code/R Scripts/helper/HelperFunctions.R")
CLIP_TO_SEDIMENT <- T   # FALSE = keep the full rectangular window

#### Bounds and outline from the sediment file ####
minmax <- read.csv("./Data/Sediment/NorthSeaRegions.csv") %>% 
  filter(Region == "North Sea")

S <- min(minmax$Latitude)
N <- max(minmax$Latitude)
W <- min(minmax$Longitude)
E <- max(minmax$Longitude)

sed_pts <- st_as_sf(minmax, coords = c("Longitude", "Latitude"), crs = 4326)

# hull around points
sed_poly <- st_concave_hull(st_union(sed_pts), ratio = 0.05)

#### GEBCO window ####
nc_raw <- nc_open("I:/Science/MS/users/students/Hatton_Matthew/Documents/PhD/24-25/West_Greenland_Implementation/Objects/data/GEBCO_2019.nc")

nc_lat <- ncvar_get(nc_raw, "lat")   # 1D vector
nc_lon <- ncvar_get(nc_raw, "lon")   # 1D vector

# Convert lat/lon bounds into index positions
lon_idx <- which(nc_lon >= W & nc_lon <= E)
lat_idx <- which(nc_lat >= S & nc_lat <= N)
stopifnot(length(lon_idx) > 0, length(lat_idx) > 0)   # catches 0-360 vs -180-180 mismatch

# Read only that window: matrix with rows = lon, cols = lat
nc_bath <- ncvar_get(nc_raw, "elevation",
                     start = c(min(lon_idx), min(lat_idx)),
                     count = c(length(lon_idx), length(lat_idx)))

sub_lon <- nc_lon[lon_idx]
sub_lat <- nc_lat[lat_idx]

nc_close(nc_raw)
rm(nc_raw, nc_lon, nc_lat)

#### Reduce resolution ####
step <- 2

bath_out <- expand.grid(Longitude = sub_lon[seq(1, length(sub_lon), step)],
                        Latitude  = sub_lat[seq(1, length(sub_lat), step)])
bath_out$Elevation <- as.vector(nc_bath[seq(1, nrow(nc_bath), step),
                                        seq(1, ncol(nc_bath), step)])

if (CLIP_TO_SEDIMENT) bath_out <- clip_to_sed(bath_out)

#### Plotting objects ####
step_plot <- 8
Con_bath <- nc_bath[seq(1, nrow(nc_bath), step_plot), seq(1, ncol(nc_bath), step_plot)]
Con_lon  <- sub_lon[seq(1, length(sub_lon), step_plot)]
Con_lat  <- sub_lat[seq(1, length(sub_lat), step_plot)]

cont <- contourLines(Con_lon, Con_lat, as.matrix(Con_bath), 
                     levels = c(-30,-40,-50,-60,-200,-1000)) %>% # Extract contours
  lapply(data.frame, stringsAsFactors = FALSE) %>%              # Convert results to dataframes
  rbindlist(id = TRUE)                                      # Bind a list of dataframes (the fast way) with an id column


stopifnot(nrow(Con_bath) == length(Con_lon),
          ncol(Con_bath) == length(Con_lat))

bath_long <- expand.grid(Longitude = Con_lon, Latitude = Con_lat)
bath_long$Elevation <- as.vector(Con_bath)
bath_long <- bath_long %>% 
  filter(Elevation <= 0)
if (CLIP_TO_SEDIMENT) bath_long <- clip_to_sed(bath_long)

#### Map ####
countries <- c("United Kingdom", "Norway", "Denmark", "Germany", "Netherlands",
               "Belgium", "France", "Sweden", "Ireland")
land <- ne_countries(scale = "large", returnclass = "sf")
land <- land[land$admin %in% countries, ]

pad = 3

levels_keep <- c(-30, -60, -100)

cont3 <- cont[cont$level %in% levels_keep, ]

p <- ggplot() +
  geom_raster(data = bath_long, aes(Longitude, Latitude, fill = Elevation)) +
  geom_sf(data = sed_poly, fill = NA, colour = "darkred", linewidth = 0.4) +
  geom_sf(data = land, fill = "grey85", colour = "grey30", linewidth = 0.3) +
  geom_path(data = cont3, aes(x = x, y = y, group = .id),
            colour = "white", linewidth = 0.3) +
  coord_sf(xlim = c(W - pad, E + pad), ylim = c(S - pad, N + pad), expand = FALSE) +
  scale_fill_viridis_c(name = "Elevation (m)") +
  labs(x = "Longitude", y = "Latitude") +
  theme_bw()
p

## Save out ##
saveRDS(bath_out, file = "./Objects/domain/Bathymetry/Bathymetry_points.rds")
saveRDS(cont, file = "./Objects/domain/Bathymetry/Bathymetry_lines.rds")
ggsave(plot = p,"./Figures/domain/Bathymetry.png")
