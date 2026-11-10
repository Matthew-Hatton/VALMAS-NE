## Run batches of R scripts.
rm(list = ls()) #reset

library(tidyverse)
library(MiMeMo.tools)
library(tictoc)

#### Batch process scripts ####

scripts <- setdiff(
  list.files("./Code/R Scripts/extraction/"),
  c("@_batch_extract.R","1. Bathymetry.R","2. Monthly Extraction.R")
) %>% 
  map(MiMeMo.tools::execute)