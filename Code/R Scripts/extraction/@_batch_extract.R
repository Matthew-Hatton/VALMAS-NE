## Run batches of R scripts.
rm(list = ls()) #reset

library(tidyverse)
library(MiMeMo.tools)
library(tictoc)

#### Batch process scripts ####

scripts <- list.files(
  "./Code/R Scripts/extraction/",
  full.names = TRUE
)


# comment out what you WANT TO RUN
scripts <- scripts[!basename(scripts) %in% c(
  "@_batch_extract.R",
  "1. Bathymetry.R",
  "2. Monthly Extraction.R",
  "3. Daily Extraction.R",
  "4. Vertical Extraction.R",
  "5. Spatial.R",
  "6. Time Series.R",
  "7. Plotting.R",
  # "8. SPM.R",
  # "9. Light.R",
  NULL
  
)] %>% 
  map(MiMeMo.tools::execute)