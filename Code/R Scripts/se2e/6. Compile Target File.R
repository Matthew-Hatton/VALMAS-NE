rm(list = ls()) # reset

source("./Code/R Scripts/@_Region file.R")

library(dplyr)
library(tibble)
library(lubridate)
library(sf)
library(units)

period      <- 2011:2019
year_col    <- "Year"      # year column in WhalesInDomainAllYears.csv
catch_col   <- "Catch"     # column holding number of whales caught

## ---- inputs ---------------------------------------------------------------
target_template <- read.csv("C:/Users/psb22188/AppData/Local/R/win-library/4.5/StrathE2E2/extdata/Models/North_Sea/2003-2013/Target/annual_observed_NORTH_SEA_2003-2013.csv")

domain <- readRDS("./Objects/domain/Domains.rds")

# inshore + offshore area, forced to plain numeric m2
domain_area <- sum(st_area(domain)) %>%
  set_units("m^2") %>%
  drop_units()

nitrogen_WW <- read.csv("./Objects/fishing/nitrogen_WW_conversions.csv")

## ---- whaling --------------------------------------------------------------
whaling <- read.csv("./Objects/fishing/whaling/WhalingInDomain.csv") %>%
  filter(between(year(date), min(period), max(period)))

whaling_cropped <- read.csv("./Objects/fishing/whaling/WhalesInDomainAllYears.csv")

cet_WW_N <- as.numeric(nitrogen_WW$Cetaceans)[1]   # mmol N per g WW

# If there is no year column but there is a date column, derive the year
if (!year_col %in% names(whaling_cropped) && "date" %in% names(whaling_cropped)) {
  whaling_cropped[[year_col]] <- year(as.Date(whaling_cropped$date))
}
stopifnot(year_col %in% names(whaling_cropped),
          catch_col %in% names(whaling_cropped),
          "Species" %in% names(whaling_cropped))

# mean mass per individual, tonnes WW (rough, adult averages)
mass_lookup <- tibble::tribble(
  ~Species, ~mass_t,
  "TBlue",  100,    # (Antarctic) true blue
  "PBlue",  50,     # pygmy blue
  "Fin",    50,
  "Spm",    25,     # sperm
  "Hbk",    30,     # humpback
  "Sei",    17.5,
  "Bryd",   15,     # Bryde's
  "CMi",    6,      # common minke
  "AMi",    7,      # Antarctic minke
  "Gray",   20,
  "Bhd",    60,     # bowhead
  "Ri",     40,     # right whales
  "Bot",    6,      # northern bottlenose whale (Hyperoodon)
  "Ki",     4,      # killer whale
  "Pi",     2,      # pilot whale
  "BBk",    11      # Baird's beaked whale
)

whaling_mass <- whaling_cropped %>%
  rename(Year_ = all_of(year_col), Catch_ = all_of(catch_col)) %>%
  filter(Species != "Total") %>%                 # drop summed rows to avoid double counting
  left_join(mass_lookup, by = "Species")

# Unsp / Oth have no mass: use the catch-weighted mean of the identified animals
known <- whaling_mass %>% filter(!is.na(mass_t))
mean_mass_known <- sum(known$Catch_ * known$mass_t, na.rm = TRUE) /
  sum(known$Catch_, na.rm = TRUE)

whaling_mass <- whaling_mass %>%
  mutate(mass_t    = coalesce(mass_t, mean_mass_known),
         biomass_t = Catch_ * mass_t)

## ---- annual catch (t WW per year) -----------------------------------------
# Sum within each year then average
annual_catch <- tibble(Year_ = period) %>%
  left_join(whaling_mass %>%
              group_by(Year_) %>%
              summarise(biomass_t = sum(biomass_t, na.rm = TRUE), .groups = "drop"),
            by = "Year_") %>%
  mutate(biomass_t = coalesce(biomass_t, 0))

catch_CT_t <- mean(annual_catch$biomass_t)       # t WW per year

## ---- nitrogen flux: mmol N m-2 y-1 ----------------------------------------
# t WW -> g WW (1e6), x mmol N per g WW, / area in m2
N_mmol_m2_y <- catch_CT_t * 1e6 * cet_WW_N / domain_area
cat("\n\nWhaling N (mmol N m-2 y-1): ", N_mmol_m2_y, "\n")

## ---- update --------------------------------------------------------
target_template[nrow(target_template),1] = N_mmol_m2_y # whaling is last
target_template[nrow(target_template),8] = "2011-2019"
target_template[nrow(target_template),9] = "Extracted and cropped to the StrathE2E North Sea domain from the International Whaling Comission."

## ---- annual PP --------------------------------------------------------
## from Capuzzo, Elisa, et al. "A decline in primary production in the North Sea over 25 years, associated with reductions in zooplankton abundance and fish stock recruitment." Global change biology 24.1 (2018): e352-e364.
capuzzo_mean <- 234
capuzzp_sd <- 17

mmN_mean_PP <- 234 * 12.56814 # C:N = 106:16 -> 1000/12.01 (atomic mass of carbon) * 16/106 (Redfield ratio) = 12.56814
mmN_sd_PP <- 17 * 12.56814

## ---- update --------------------------------------------------------
target_template[1,1] = mmN_mean_PP
target_template[1,2] = mmN_sd_PP
target_template[1,8] = "2011-2019"
target_template[1,9] = "Capuzzo 2018"
