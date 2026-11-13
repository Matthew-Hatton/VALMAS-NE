
## Overwrite the entries in the example Celtic Sea Physical parameters file which we have recalculated for this region

#### Setup ####

rm(list=ls())                                                               # Wipe the brain
library(tidyverse)
source("./Code/R scripts/@_Region file.R")

Physical_parameters <- read.csv("C:/Users/psb22188/AppData/Local/R/win-library/4.5/StrathE2E2/extdata/Models/North_Sea/1970-1999/Param/physical_parameters_NORTH_SEA.csv")  # Read in example boundary drivers

#### Last minute data manipulation ####

My_space <- readRDS("./Objects/domain/Domains.rds") %>%                            # Calculate the volume of the three zones
  sf::st_drop_geometry() %>% 
  mutate(S = T,
         D = case_when(Shore == "Inshore" ~ F,
                       Shore == "Offshore" ~ T)) %>% 
  gather(key = "Depth", value = "Exists", S, D) %>% 
  filter(Exists == T) %>%
  mutate(Elevation = case_when(Shore == "Inshore" ~ Elevation,
                               Shore == "Offshore" & Depth == "D" ~ Elevation + SDepth,
                               Shore == "Offshore" & Depth == "S" ~ -SDepth,)) %>% 
  mutate(Volume = area * abs(Elevation)) 

#### Update Spatial file ####

Physical_parameters[1,"Value"] <- filter(My_space, Shore == "Offshore", Depth == "S")$Elevation * -1 # Offshore_Shallow_layer_thickness_(m)
Physical_parameters[2,"Value"] <- filter(My_space, Shore == "Offshore", Depth == "D")$Elevation * -1 # Offshore_Deep_layer_thickness_(m)
Physical_parameters[3,"Value"] <- filter(My_space, Shore == "Inshore", Depth == "S")$Elevation * -1  # Inshore_Shallow_layer_thickness_(m)

Physical_parameters[21,"Value"] <- -1.035                  # Parameter_1_for_relationship_between_porosity_and_grainsize. Values from Matt Pace's thesis
Physical_parameters[22,"Value"] <- -0.314                  # Parameter_2_for_relationship_between_porosity_and_grainsize. The values are also the defaults in the D50_to_porosity function
Physical_parameters[23,"Value"] <- -0.435                  # Parameter_3_for_relationship_between_porosity_and_grainsize
Physical_parameters[24,"Value"] <- 0.302                   # Parameter_4_for_relationship_between_porosity_and_grainsize

Physical_parameters[34,"Value"] <- 0                       # 1 to use the following porosity values, 0 calculates using the relationship above
# Physical_parameters[35,"Value"] <- filter(My_sediment, Habitat == "Inshore Silt")$Porosity       # Defined_porosity_of_inshore_sediment_s1_(muddy)
# Physical_parameters[36,"Value"] <- filter(My_sediment, Habitat == "Inshore Sand")$Porosity       # Defined_porosity_of_inshore_sediment_s2_(sandy)
# Physical_parameters[37,"Value"] <- filter(My_sediment, Habitat == "Inshore Gravel")$Porosity     # Defined_porosity_of_inshore_sediment_s3_gravelly)
# Physical_parameters[38,"Value"] <- filter(My_sediment, Habitat == "Offshore Silt")$Porosity      # Defined_porosity_of_offshore_sediment_d1_(muddy)
# Physical_parameters[39,"Value"] <- filter(My_sediment, Habitat == "Offshore Sand")$Porosity      # Defined_porosity_of_offshore_sediment_d2_(sandy)
# Physical_parameters[40,"Value"] <- filter(My_sediment, Habitat == "Offshore Gravel")$Porosity    # Defined_porosity_of_offshore_sediment_d3_(gravelly)
# 
Physical_parameters[41,"Value"] <- 0                     # 1 to use the following permeability values, 0 calculates using the relationship above
# Physical_parameters[42,"Value"] <- filter(My_sediment, Habitat == "Inshore Silt")$Permeability   # Defined_permeability_of_inshore_sediment_s1_(m-2)
# Physical_parameters[43,"Value"] <- filter(My_sediment, Habitat == "Inshore Sand")$Permeability   # Defined_permeability_of_inshore_sediment_s2_(m-2)
# Physical_parameters[44,"Value"] <- filter(My_sediment, Habitat == "Inshore Gravel")$Permeability # Defined_permeability_of_inshore_sediment_s3_(m-2)
# Physical_parameters[45,"Value"] <- filter(My_sediment, Habitat == "Offshore Silt")$Permeability  # Defined_permeability_of_offshore_sediment_d1_(m-2)
# Physical_parameters[46,"Value"] <- filter(My_sediment, Habitat == "Offshore Sand")$Permeability  # Defined_permeability_of_offshore_sediment_d2_(m-2)
# Physical_parameters[47,"Value"] <- filter(My_sediment, Habitat == "Offshore Gravel")$Permeability# Defined_permeability_of_offshore_sediment_d3_(m-2)
# 
Physical_parameters[48,"Value"] <- 0                     # 1 to use the following nitrogen values, 0 calculates using the relationship above
# Physical_parameters[49,"Value"] <- filter(My_sediment, Habitat == "Inshore Silt")$Nitrogen       # Defined_total_N%_of_inshore_sediment_s1_(%DW)
# Physical_parameters[50,"Value"] <- filter(My_sediment, Habitat == "Inshore Sand")$Nitrogen       # Defined_total_N%_of_inshore_sediment_s2_(%DW)
# Physical_parameters[51,"Value"] <- filter(My_sediment, Habitat == "Inshore Gravel")$Nitrogen     # Defined_total_N%_of_inshore_sediment_s3_(%DW)
# Physical_parameters[52,"Value"] <- filter(My_sediment, Habitat == "Offshore Silt")$Nitrogen      # Defined_total_N%_of_offshore_sediment_d1_(%DW)
# Physical_parameters[53,"Value"] <- filter(My_sediment, Habitat == "Offshore Sand")$Nitrogen      # Defined_total_N%_of_offshore_sediment_d2_(%DW)
# Physical_parameters[54,"Value"] <- filter(My_sediment, Habitat == "Offshore Gravel")$Nitrogen    # Defined_total_N%_of_offshore_sediment_d3_(%DW)

write.csv(Physical_parameters,
          file = paste0("C:/Users/psb22188/AppData/Local/R/win-library/4.5/StrathE2E2/extdata/Models/North_Sea/",start_year,"-",end_year,"-",forcing,"-",ssp,"/Param/physical_parameters_NORTH_SEA.csv"), 
          row.names = F)
