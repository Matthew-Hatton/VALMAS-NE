## Initialise model

source("./Code/R Scripts/@_Region file.R")

R.utils::copyDirectory(paste0("C:/Users/psb22188/AppData/Local/R/win-library/4.5/StrathE2E2/extdata/Models/North_Sea/2003-2013"),# Copy example model 
                       paste0("C:/Users/psb22188/AppData/Local/R/win-library/4.5/StrathE2E2/extdata/Models/North_Sea/",start_year,"-",end_year,"-",forcing,"-",ssp))    # Into new implementation
