## Compare the driving data (physics + chemistry) of the new 2010-2019 models
## against the 2003-2013 reference model.
## NOTE: run this AFTER the batch build, on its own. It is excluded from @_batch_build.R.

#### Setup ####

rm(list = ls())
library(tidyverse)

source("./Code/R Scripts/helper/HelperFunctions.R")

models_dir <- "C:/Users/psb22188/AppData/Local/R/win-library/4.5/StrathE2E2/extdata/Models/North_Sea"
ref_model  <- "2003-2013"                                                   # Model to compare against
new_period <- "2010-2019"                                                   # Decade of new models to compare
out_dir    <- paste0("./Figures/driver comparison/", new_period)            # Where the plots go
make_individual_plots <- TRUE                                               # One png per variable as well as the overview

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

#### Find the models ####

all_models <- list.dirs(models_dir, recursive = FALSE, full.names = FALSE)
new_models <- all_models[str_detect(all_models, paste0("^", new_period, "-(GFDL|CNRM)-ssp\\d+$"))]

if (!ref_model %in% all_models) stop("Reference model not found: ", ref_model)
if (length(new_models) == 0) stop("No ", new_period, " models found in ", models_dir)
message("Comparing ", ref_model, " to: ", paste(new_models, collapse = ", "))

drivers <- expand_grid(model = c(ref_model, new_models),
                       type = c("physics", "chemistry")) %>%
  pmap(~ read_driver(..1, ..2)) %>%
  bind_rows() %>%
  mutate(Forcing = map_chr(Model, ~ meta(.x)[["Forcing"]]),
         SSP = map_chr(Model, ~ meta(.x)[["SSP"]]),
         Is_ref = Model == ref_model,
         Forcing = factor(Forcing, levels = c("GFDL", "CNRM", ref_model)),
         SSP = factor(SSP, levels = c("ssp126", "ssp370", ref_model)))

#### Plotting ####

colours <- setNames(c("#0072B2", "#D55E00", "black"), c("GFDL", "CNRM", ref_model))
lines   <- setNames(c("solid", "longdash", "solid"), c("ssp126", "ssp370", ref_model))

# Plot just a single scenario - Think Mike seemed more interested in this
singleScenario = T

for (type in unique(drivers$Type)) {
  if (singleScenario == T) {
    type_data <- filter(drivers, Type == type & SSP %in% c("ssp126","2003-2013") & Forcing %in% c("CNRM","2003-2013"))
  } else{
    type_data <- filter(drivers, Type == type)
  }
  
  n_vars <- n_distinct(type_data$Variable)
  
  ncol <- 5
  nrow <- ceiling(n_vars / ncol)
  overview <- compare_plot(type_data, ncol = ncol, base_size = 9) +
    labs(title = paste(type, "drivers"))
  
  ggsave(file.path(out_dir, paste0(type, " drivers ", new_period, " vs ", ref_model, ".png")),
         overview, width = 34, height = 5 * nrow + 2, units = "cm", dpi = 300,
         bg = "white", limitsize = FALSE) # for plot with all
  
  ## One plot per variable
  if (make_individual_plots) {
    var_dir <- file.path(out_dir, type)
    dir.create(var_dir, showWarnings = FALSE)
    
    for (v in unique(type_data$Variable)) {
      p <- compare_plot(filter(type_data, Variable == v)) +
        labs(title = paste(type, "-", v))
      ggsave(file.path(var_dir, paste0(safe_name(v), ".png")),
             p, width = 20, height = 10, units = "cm", dpi = 300, bg = "white")
    }
  }
  message("Saved ", type, " plots (", n_vars, " variables)")
}
