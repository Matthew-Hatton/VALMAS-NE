## Calculates daily proportion disturbed by natural bed shear stress

rm(list = ls())

library(MiMeMo.tools) # everything we need
library(raster)

source("./Code/R Scripts/@_Region file.R")

# Aggregate Sediment
intersection <- readRDS("./Objects/physical/habitat_depth intersection.RDS")
intersection_agg <- intersection %>% ## aggregation into SE2E terms
  mutate(Name = case_when(
    Name == "Bedrock with Mud" ~ "Gravel",
    Name == "Muddy Sand" ~ "Sand",
    Name == "Gravelly Mud" ~ "Gravel",
    Name == "Coarse Rocky Ground" ~ "Gravel",
    Name == "Mud" ~ "Silt",
    Name == "Gravelly Sand" ~ "Sand",
    Name == "Bedrock with Sand" ~ "Gravel",
    T ~ Name
  )) %>% 
  rename(sediment = Name)

# Read in model
model <- readRDS("./Objects/physics/Disturbance model.RDS")


# Calculate disturbance
gravel_S <- intersection_agg %>% 
  filter(shore == "in" & sediment == "Gravel") %>% 
  summarise(depth = mean(depth)) %>% 
  mutate(shore = "in") %>% 
  predict(model,.) %>% 
  mean(.) %>% 
  exp(.)
gravel_D <- intersection_agg %>% 
  filter(shore == "off" & sediment == "Gravel") %>% 
  summarise(depth = mean(depth)) %>% 
  mutate(shore = "off") %>% 
  predict(model,.) %>% 
  mean(.) %>% 
  exp(.)

sand_S <- intersection_agg %>% 
  filter(shore == "in" & sediment == "Sand") %>% 
  summarise(depth = mean(depth)) %>% 
  mutate(shore = "in") %>% 
  predict(model,.) %>% 
  mean(.) %>% 
  exp(.)
sand_D <- intersection_agg %>% 
  filter(shore == "off" & sediment == "Sand") %>% 
  summarise(depth = mean(depth)) %>% 
  mutate(shore = "off") %>% 
  predict(model,.) %>% 
  mean(.) %>% 
  exp(.)

silt_S <- intersection_agg %>% 
  filter(shore == "in" & sediment == "Silt") %>% 
  summarise(depth = mean(depth)) %>% 
  mutate(shore = "in") %>% 
  predict(model,.) %>% 
  mean(.) %>% 
  exp(.)
silt_D <- intersection_agg %>% 
  filter(shore == "off" & sediment == "Silt") %>% 
  summarise(depth = mean(depth)) %>% 
  mutate(shore = "off") %>% 
  predict(model,.) %>% 
  mean(.) %>% 
  exp(.)

my_stress <- data.frame(month = seq(1,12),
                        habS1_pdist = silt_S,
                        habS2_pdist = sand_S,
                        habS3_pdist = gravel_S,
                        habD1_pdist = silt_D,
                        habD2_pdist = sand_D,
                        habD3_pdist = gravel_D)
saveRDS(my_stress,"./Objects/physics/My_Stress.RDS")
