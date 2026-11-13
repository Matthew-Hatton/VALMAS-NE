## Overwrite example boundary data

#### Setup ####

rm(list=ls())                                                               # Wipe the brain
Packages <- c("MiMeMo.tools", "exactextractr", "raster", "lubridate")       # List packages
lapply(Packages, library, character.only = TRUE)   
source("./Code/R Scripts/@_Region file.R")

Boundary_template <- read.csv("C:/Users/psb22188/AppData/Local/R/win-library/4.5/StrathE2E2/extdata/Models/North_Sea/1970-1999/Driving/chemistry_NORTH_SEA_1970-1999.csv")  # Read in example boundary drivers

## Iterate over different time periods ##
#### Last minute data manipulation ####

My_boundary_data<- readRDS("./Objects/chemistry/Boundary measurements.rds") %>% # Import data
  filter(between(Year, start_year, end_year), Forcing == forcing, SSP == ssp) %>%   # Limit to outputs from a specific run and time
  group_by(Month,Compartment) %>% 
  pivot_wider(names_from = Variable,values_from = Measured) %>% # Average across years
  summarise(across(NO3:Other_phytoplankton, ~ mean(.x, na.rm = T))) %>% 
  ungroup() %>% 
  arrange(Month)

My_atmosphere <- readRDS(stringr::str_glue("./Objects/chemistry/NE.Atmospheric N deposition.rds")) %>%
  filter(between(Year, start_year, end_year)) %>%     
  filter(SSP == paste0("ssp",ssp) | SSP == "hist") %>% # Limit to reference period
  group_by(Month, Oxidation_state, Shore,  Year) %>%
  summarise(Measured = sum(Measured, na.rm = T)) %>%                                         # Sum across deposition states
  summarise(Measured = mean(Measured, na.rm = T)) %>%                                        # Average over years
  ungroup() %>%
  pivot_wider(names_from = c(Shore, Oxidation_state), values_from = Measured) %>%            # Spread to match template
  arrange(Month)                                                                                  # Order months ascending

My_river_N <- readRDS("./Objects/chemistry/NM River input.rds")

#### Create new file ####

Boundary_new <- Boundary_template %>% 
  mutate(SO_nitrate = My_boundary_data %>% filter(Compartment == "Offshore S") %>% .$NO3,
         SO_ammonia = My_boundary_data %>% filter(Compartment == "Offshore S") %>% .$NH4,
         SO_phyt =  My_boundary_data %>% filter(Compartment == "Offshore S") %>% .$Diatoms +
           My_boundary_data %>% filter(Compartment == "Offshore S") %>% .$Other_phytoplankton,
         SO_detritus =  My_boundary_data %>% filter(Compartment == "Offshore S") %>% .$Detritus,
         D_nitrate =  My_boundary_data %>% filter(Compartment == "Offshore D") %>% .$NO3, 
         D_ammonia = My_boundary_data %>% filter(Compartment == "Offshore D") %>% .$NH4, 
         D_phyt = My_boundary_data %>% filter(Compartment == "Offshore D") %>% .$Diatoms + 
           My_boundary_data %>% filter(Compartment == "Offshore D") %>% .$Other_phytoplankton,
         D_detritus = My_boundary_data %>% filter(Compartment == "Offshore D") %>% .$Detritus,
         SI_nitrate = My_boundary_data %>% filter(Compartment == "Inshore S") %>% .$NO3,
         SI_ammonia = My_boundary_data %>% filter(Compartment == "Inshore S") %>% .$NH4,
         SI_phyt = My_boundary_data %>% filter(Compartment == "Inshore S") %>% .$Other_phytoplankton, 
         SI_detritus = My_boundary_data %>% filter(Compartment == "Inshore S") %>% .$Detritus,
         ## Rivers
         RIV_nitrate = My_river_N$NO3,
         RIV_ammonia = My_river_N$NH4,
         RIV_detritus = 0,
         ## Atmosphere, daily deposition as monthly averages
         SO_ATM_nitrate_flux = My_atmosphere$Offshore_O,
         SO_ATM_ammonia_flux = My_atmosphere$Offshore_R,
         SI_ATM_nitrate_flux = My_atmosphere$Inshore_O,
         SI_ATM_ammonia_flux = My_atmosphere$Inshore_R,
         SI_other_nitrate_flux = 0,   # Can be used for scenarios
         SI_other_ammonia_flux = 0,
         SO_other_nitrate_flux = 0,   # Can be used for scenarios
         SO_other_ammonia_flux = 0,
  ) 

write.csv(Boundary_new, file = paste0("C:/Users/psb22188/AppData/Local/R/win-library/4.5/StrathE2EPolar/extdata/Models/West_Greenland/",start_year,"-",end_year,"-",forcing,"-SSP",ssp,"/Driving/chemistry_WG_",start_year,"-",end_year,"-",forcing,"-SSP370.csv"),
          row.names = F)