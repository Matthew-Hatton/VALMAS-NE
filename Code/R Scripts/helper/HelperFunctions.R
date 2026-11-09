## Helper functions for extraction
clip_to_sed <- function(df, lon = "Longitude", lat = "Latitude") {
  pts    <- st_as_sf(df, coords = c(lon, lat), crs = 4326, remove = FALSE)
  inside <- lengths(st_intersects(pts, sed_poly)) > 0
  st_drop_geometry(pts[inside, ])
}