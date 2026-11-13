source("./Code/R Scripts/@_Region file.R")

## Remove old csvs
fns <- c(paste0("C:/Users/psb22188/AppData/Local/R/win-library/4.5/StrathE2E2/extdata/Models/North_Sea/",start_year,"-",end_year,"-",forcing,"-",ssp,"/Driving/chemistry_NORTH_SEA_1970-1999.csv"),
         paste0("C:/Users/psb22188/AppData/Local/R/win-library/4.5/StrathE2E2/extdata/Models/North_Sea/",start_year,"-",end_year,"-",forcing,"-",ssp,"/Driving/physics_NORTH_SEA_1970-1999.csv"))

for (i in fns) {
  if (file.exists(i)) {
    print("i")
    #Delete file if it exists
    file.remove(i)
  }
}

## Model setup
model_setup <- read.csv(paste0("C:/Users/psb22188/AppData/Local/R/win-library/4.5/StrathE2E2/extdata/Models/North_Sea/",start_year,"-",end_year,"-",forcing,"-",ssp,"/MODEL_SETUP.csv"))

# Names
model_setup[2,1] <- paste0("physics_NORTH_SEA_",start_year,"-",end_year,".csv")
model_setup[3,1] <- paste0("chemistry_NORTH_SEA_",start_year,"-",end_year,".csv")
model_setup[4,1] <- paste0("initial_values_NORTH_SEA_",start_year,"-",end_year,".csv")
# model_setup[5,1] <- paste0("event_timing_NORTH_SEA_",start_year,"-",end_year,".csv")
# model_setup[6,1] <- paste0("fixed_consumers_NORTH_SEA_",start_year,"-",end_year,".csv")
# model_setup[11,1] <- paste0("fishing_fleet_NORTH_SEA_",start_year,"-",end_year,".csv")
# model_setup[12,1] <- paste0("fishing_activity_NORTH_SEA_",start_year,"-",end_year,".csv")
# model_setup[13,1] <- paste0("fishing_power_NORTH_SEA_",start_year,"-",end_year,".csv")
# model_setup[23,1] <- paste0("annual_observed_NORTH_SEA_",start_year,"-",end_year,".csv")
# model_setup[24,1] <- paste0("monthly_observede_NORTH_SEA_",start_year,"-",end_year,".csv")
# model_setup[25,1] <- paste0("region_harvest_r_NORTH_SEA_",start_year,"-",end_year,".csv")
# model_setup[26,1] <- paste0("zonal_harvest_r_NORTH_SEA_",start_year,"-",end_year,".csv")

# Comments
model_setup[2,4] <- paste0("North Sea ",start_year,"-",end_year," data from NEMO-ERSEM and elsewhere")
model_setup[3,4] <- paste0("North Sea ",start_year,"-",end_year," data from NEMO-ERSEM and elsewhere")


write.csv(model_setup,paste0("C:/Users/psb22188/AppData/Local/R/win-library/4.5/StrathE2E2/extdata/Models/North_Sea/",start_year,"-",end_year,"-",forcing,"-",ssp,"/MODEL_SETUP.csv"),
          row.names = FALSE)

