## Full build of the model: every decade x forcing x SSP
rm(list = ls())

library(tidyverse)
library(MiMeMo.tools)
library(tictoc)

# Everything lives inside a function so the loop state survives the
# rm(list = ls()) calls inside the numbered scripts.
build_all <- function() {
  
  builds <- expand_grid(start_year = seq(2010, 2060, by = 10),
                        forcing    = c("GFDL", "CNRM"),
                        ssp        = c("ssp126", "ssp370")) %>%
    mutate(end_year = start_year + 9)                      # 24 builds
  
  scripts <- list.files("./Code/R Scripts/se2e/", full.names = TRUE)
  scripts <- scripts[basename(scripts) != "@_batch_build.R"]
  
  pwalk(builds, function(start_year, end_year, forcing, ssp) {
    message("==== Building ", start_year, "-", end_year, "-", forcing, "-", ssp, " ====")
    Sys.setenv(BUILD_START   = start_year,
               BUILD_END     = end_year,
               BUILD_FORCING = forcing,
               BUILD_SSP     = ssp)
    
    tryCatch(walk(scripts, MiMeMo.tools::execute),          # one failure won't stop the other builds
             error = function(e) message("FAILED: ", start_year, "-", end_year, "-",
                                         forcing, "-", ssp, ": ", conditionMessage(e)))
  })
}

build_all()