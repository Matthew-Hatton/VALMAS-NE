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

characterise_flows2 <- function(lines, domain, precision = 1000) {
  lines$current <- st_transform(lines, crs = 4326) %>% st_coordinates() %>%
    data.frame() %>% group_by(L1) %>%
    summarise(Lon_dif = abs(X[1] - X[2]), Lat_dif = abs(Y[1] - Y[2])) %>%
    mutate(current = ifelse(Lon_dif > Lat_dif, "Meridional", "Zonal")) %>% .$current
  
  midpoints <- sf::st_centroid(lines) %>% st_transform(crs = 4326) %>%
    st_coordinates() %>% cbind(1:nrow(.))
  
  minus <- data.frame(ifelse(lines$current == "Meridional",
                             list(c(0, -(1/precision), 0)), list(c(-(1/precision), 0, 0)))) %>% t()
  plus  <- data.frame(ifelse(lines$current == "Meridional",
                             list(c(0,  (1/precision), 0)), list(c( (1/precision), 0, 0)))) %>% t()
  
  mid_minus <- midpoints + minus %>% as.data.frame()
  rownames(mid_minus) <- NULL
  mid_minus <- st_as_sf(mid_minus, coords = c("V1", "V2"), crs = 4326) %>%
    sf::st_transform(crs) %>% sf::st_cast("POINT") %>%
    mutate(side = "minus", focal = lines$Shore)
  
  mid_plus <- midpoints + plus %>% as.data.frame()
  rownames(mid_plus) <- NULL
  mid_plus <- st_as_sf(mid_plus, coords = c("V1", "V2"), crs = 4326) %>%
    sf::st_transform(crs) %>% sf::st_cast("POINT") %>%
    mutate(side = "plus", focal = lines$Shore)
  
  tests <- bind_rows(mid_plus, mid_minus) %>%
    sf::st_join(domain, join = st_intersects) %>%
    tidyr::replace_na(list(Shore = "Ocean")) %>%
    dplyr::mutate(Contained = dplyr::if_else(focal == Shore, "in", "out")) %>%
    dplyr::mutate(Flip = dplyr::if_else(side == "plus" & Contained == "out" |
                                          side == "minus" & Contained == "in", TRUE, FALSE)) %>%
    group_by(V3)
  
  check <- mutate(tests,
                  test   = length(unique(Shore)),
                  n_rows = dplyr::n(),
                  n_in   = sum(Contained == "in")) %>%
    filter(test != 2 | n_rows != 2 | n_in != 1)
  
  if (nrow(check) == 0) {
    tests <- mutate(tests, Neighbour = Shore[Shore != focal]) %>%
      ungroup() %>% dplyr::filter(Contained == "in") %>%
      sf::st_set_geometry(NULL) %>% arrange(V3) %>%
      dplyr::select(Flip, Neighbour)
    lines2 <- cbind(lines, tests)
    return(lines2)
  } else {
    print("Ambiguous transect labels, often from a lack of precision. Inspect returned transects.")
    return(list(check, lines))
  }
}

## Build all model combinations
## Full build of the model: every decade x forcing x SSP, in parallel
rm(list = ls())

library(tidyverse)
library(MiMeMo.tools)
library(furrr)

build_all <- function(n_workers = 4) {
  
  builds <- expand_grid(start_year = seq(2010, 2060, by = 10),
                        forcing    = c("GFDL", "CNRM"),
                        ssp        = c("ssp126", "ssp370")) %>%
    mutate(end_year = start_year + 9)
  
  scripts <- list.files("./Code/R Scripts/se2e/", full.names = TRUE)
  scripts <- scripts[basename(scripts) != "@_batch_build.R"]
  
  plan(multisession, workers = n_workers)
  on.exit(plan(sequential))                                  # tidy up workers when done
  
  results <- future_pmap_chr(builds, function(start_year, end_year, forcing, ssp) {
    id <- paste(start_year, end_year, forcing, ssp, sep = "-")
    
    Sys.setenv(BUILD_START   = start_year,
               BUILD_END     = end_year,
               BUILD_FORCING = forcing,
               BUILD_SSP     = ssp)
    
    tryCatch({
      for (s in scripts) MiMeMo.tools::execute(s)
      paste("OK    ", id)
    }, error = function(e) paste("FAILED", id, ":", conditionMessage(e)))
    
  }, .options = furrr_options(seed = NULL), .progress = TRUE)
  
  walk(results, message)                                     # summary of all 24 builds
  invisible(results)
}
