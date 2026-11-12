## Run batches of R scripts.
rm(list = ls()) #reset

library(tidyverse)
library(MiMeMo.tools)
library(tictoc)

#### Batch process scripts ####

scripts <- list.files(
  "./Code/R Scripts/flows/",
  full.names = TRUE
)

scripts <- scripts[!basename(scripts) %in% c(
  "@_batch_flows.R",
  "1. Vertical Diffusivity.R",
  "2. Vertical Exchanges.R",
  "3. Waves.R", # doesn't change between NM and NE
  "4. Make Transects.R",
  NULL
)] %>% 
  map(MiMeMo.tools::execute)