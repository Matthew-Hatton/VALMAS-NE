## Helper functions for extraction
clip_to_sed <- function(df, lon = "Longitude", lat = "Latitude") {
  pts    <- st_as_sf(df, coords = c(lon, lat), crs = 4326, remove = FALSE)
  inside <- lengths(st_intersects(pts, sed_poly)) > 0
  st_drop_geometry(pts[inside, ])
}

point_plot_save <- function (data, var) 
{
  decade  <- data$Decade[1]
  depth   <- data$slab_layer[1]
  forcing <- data$Forcing[1]
  ssp     <- data$SSP[1]
  
  if (depth == "D" & var %in% c("Ice", "Ice_conc", "Ice_Thickness", 
                                "Snow_Thickness")) {
    print("Skipped deep ice plot")
  } else {
    print(paste("plotting", var, "for", decade, depth, forcing, ssp))
    
    map <- ggplot2::ggplot() + 
      ggplot2::theme_minimal() + 
      ggplot2::labs(title = paste("Decade:", decade),
                    subtitle = paste("Water layer:", depth),
                    x = NULL, y = NULL) +
      ggplot2::geom_raster(data = data, 
                           ggplot2::aes(x = x, y = y, fill = .data[[var]])) +
      viridis::scale_fill_viridis(option = "viridis", name = var, na.value = "red") +
      ggplot2::facet_wrap(ggplot2::vars(Month))
    
    ggplot2::ggsave(
      paste0("./Figures/grids/NE.map ", var, " ", depth, " ", decade, " ",
             forcing, " ", ssp, ".png"),
      plot = map, scale = 1, width = 32, height = 20, units = "cm",
      dpi = 500, bg = "white")
  }
}