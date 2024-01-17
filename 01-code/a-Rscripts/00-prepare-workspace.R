#### Load required packages ####
library(readxl)
library(ggplot2)
library(gridExtra)
library(GGally)
library(carData)
library(stats)
library(car)
library(dplyr)
library(tidyr)
library(lme4)
library(nlme)
library(effects)
library(ggplot2)
library(tidyverse) #y - pipe (%>%)
library(cowplot) #y - cowplot
library(MuMIn)
library(r2glmm)
library(here) #y - here

#### Read in prepared data ####
# Read in "Crawfish eHf vs. U/Pb age" data table
crawfish_hf <- read.csv(here::here("00-data", "b-prepared", "Crawfish-eHf-age.csv"))

# Check structure and data types
str(crawfish_hf)

# Read in "Crawfish eNd and 87Sr/86Sr vs. U/Pb age" data table
crawfish_nd_sr <- read.csv(here::here("00-data", "b-prepared", "Crawfish-eNd-87Sr-age.csv"))

# Check structure and data types
str(crawfish_nd_sr)

# Read in "Crawfish [Y] vs. U/Pb age" data table
crawfish_y <- read.csv(here::here("00-data", "b-prepared", "Crawfish-Ycn-age.csv"))

# Check structure and data types
str(crawfish_y)

# Read in "Eastern SBB U/Pb age vs. distance from Sanak" data table
eastern_sbb <- read.csv(here::here("00-data", "b-prepared", "eastern-sbb-age-distance-zircons-final.csv"))

# Check structure and data types
str(eastern_sbb)

# Read in "SBB rock age vs. distance from Sanak for all available age dates" data table
sbb_age_distance_all <- read.csv(here::here("00-data", "b-prepared", "sbb-age-distance-all-samples.csv"))

# Check structure and data types
str(sbb_age_distance_all)

# Read in "SBB U/Pb age vs. distance from Sanak for all concordant U/Pb zircon ages" data table
sbb_age_distance_zircons <- read.csv(here::here("00-data", "b-prepared", "sbb-age-distance-zircons-final.csv"))

# Check structure and data types
str(sbb_age_distance_zircons)

# Read in "SBB eHf vs. U/Pb age for all SBB plutons" data table
sbb_eHf_UPb_plutons <- read.csv(here::here("00-data", "b-prepared", "sbb-eHf-UPb-pluton-zircons.csv"))

# Check structure and data types
str(sbb_eHf_UPb_plutons)

# Read in "SBB eHf vs. U/Pb age for all CPW sediments with U/Pb age < 540 Ma" data table
sbb_eHf_UPb_seds_540Ma <- read.csv(here::here("00-data", "b-prepared", "sbb-eHf-UPb-sediment-zircons-540Ma.csv"))

# Check structure and data types
str(sbb_eHf_UPb_seds_540Ma)

# Read in "SBB eHf vs. U/Pb age for all CPW sediments" data table
sbb_eHf_UPb_seds <- read.csv(here::here("00-data", "b-prepared", "sbb-eHf-UPb-sediment-zircons.csv"))

# Check structure and data types
str(sbb_eHf_UPb_seds)

# Read in "Sr/Y vs. distance from Sanak for all SBB intrusive rocks with SiO2 > 54 wt %" data table
sbb_SrY_distance_54Si <- read.csv(here::here("00-data", "b-prepared", "sbb-SrY-distance-compilation-54SiO2-cutoff.csv"))

# Check structure and data types
str(sbb_SrY_distance_54Si)

# Read in "Western SBB U/Pb age vs. distance from Sanak Island" data table
western_sbb <- read.csv(here::here("00-data", "b-prepared", "western-sbb-age-distance-zircons-final.csv"))

# Check structure and data types
str(western_sbb)





