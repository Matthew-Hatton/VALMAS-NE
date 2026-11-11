
## Visualise summarised nemo-medusa model output

#### Set up ####

rm(list=ls(all.names = TRUE))                                               # Wipe the brain

packages <- c("tidyverse", "nemoRsem", "furrr", "tictoc")                       # List packages
source("./Code/R Scripts/helper/HelperFunctions.R")
lapply(packages, library, character.only = TRUE)                                # Load packages
plan(multisession,workers = availableCores())                                                          # Instructions for parallel processing

force <- c("GFDL")
ssp <- c("ssp126") # check capitalisation

TS <- readRDS(paste0("./Objects/physics/",force,".",ssp,".TS.rds")) #%>% 
#  filter(Year < 2051)

ggplot() +
  #geom_smooth(data = TS,aes(x = date,y = Temperature_avg)) +
  geom_line(data = TS,aes(x = date,y = Temperature_avg,color = ssp)) +
  facet_wrap(~Compartment,ncol = 1)

# Read in time series
vars_ts <- c("NO3_avg", "NH4_avg", 
             "Diatoms_avg", "Other_phytoplankton_avg", 
             "Detritus_avg", "Temperature_avg")                                 # Variables to plot   

SP <- readRDS("./Objects/spatial/SPATIAL.rds") %>%                                      # Read in spatial data
  map(drop_na, Shore)

vars_sp <- str_remove(vars_ts, "_avg") %>%                                  # Tweak the variable names for spatial plots
  c("Speed")

#### Plotting ####

walk(vars_ts, ts_plot)                                                      # Save a time series figure for each variable.

future_map2(rep(SP, each = length(vars_sp)),                                # For each decade
            rep(vars_sp, times = length(SP)), point_plot_save,                   # And each variable
            .progress = TRUE)                                               # Plot spatial maps in parallel

#ggplot(SP[["2010.S"]]) +
#  geom_raster(aes(x=x, y=y, fill = Shore, alpha = Bathymetry != 0))