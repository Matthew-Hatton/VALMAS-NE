## Full build of the model.
rm(list = ls()) #reset

library(tidyverse)
library(MiMeMo.tools)
library(tictoc)
source("./regionFile.R")

#### Batch process scripts ####

scripts <- list.files(
  "./Code/R Scripts/se2e/",
  full.names = TRUE
)

scripts <- scripts[!basename(scripts) %in% c(
  "@_batch_build.R",
  NULL
)] %>% 
  map(MiMeMo.tools::execute)