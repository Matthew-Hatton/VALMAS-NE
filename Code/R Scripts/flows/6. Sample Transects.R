# readRDS("./Objects/NE_Days/.")    # Marker so network script can see where the data is coming from

#### Set up ####

rm(list=ls())                                                               # Wipe the brain
Packages <- c("tidyverse", "nemoRsem", "data.table", "furrr")                         # List packages
lapply(Packages, library, character.only = TRUE)                                # Load packages
source("./Code/R Scripts/@_Region file.R")                                       # Define project region 
plan(multisession,workers = availableCores() - 2)                                                          # Choose the method to parallelise by with furrr

Transects <- readRDS("./Objects/chemistry/Boundary_transects.rds")                    # Import transects to sample at

look <- list.files("./Objects/NE_Days/", full.names = T) %>% 
  .[1] %>%
  readRDS()

#### Summarise along transects ####
Summary <- list.files("./Objects/NE_Days/", full.names = T) %>%              # Get the names of all data files
  future_map(NE_boundary_summary, transects = Transects, 
             vars = c("NO3", "NH4", "Detritus", "Diatoms", "Other_phytoplankton"), .progress = T) # Sample NE output along domain boundary

#### Save water exchanges between compartments ####

Flows <- map(Summary, `[[`, 1) %>%                                          # Subset the summary results
  data.table::rbindlist() %>% 
  saveRDS("./Objects/physics/H-Flows.rds")                                          

#### Save boundary conditions ####

Boundary <- map(Summary, `[[`, 2) %>%                                       # Subset the summary results
  data.table::rbindlist()
saveRDS(Boundary, "./Objects/chemistry/Boundary measurements.rds")