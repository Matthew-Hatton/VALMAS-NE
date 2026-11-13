## Full build of the model: every decade x forcing x SSP
rm(list = ls())

library(tidyverse)
library(MiMeMo.tools)
library(tictoc)

source("./Code/R Scripts/helper/HelperFunctions.R")

build_all()