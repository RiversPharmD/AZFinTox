################################################################################

## Author: Zach Rivers, @riverspharmd, zrivers@fredhutch.org
## Contributors:

## Purpose: After first pass through data, identified missingness, got new
## predictors. Need to incorporate them and then update the number of missing
## PARTIDs so I can send it off to the nice person who will help me.

## Depends on:
## tidyverse
## readxl

## Input:
## "RawData/final full sample master demog 11302021.xlsx"

## Output:

################################################################################

# Packages______________________________________________________________________

library(tidyverse)
library(readxl)

# Functions_____________________________________________________________________

# Data In_______________________________________________________________________

dat <- read_xlsx(path = "RawData/final full sample master demog 11302021.xlsx")

# Analysis______________________________________________________________________

## Select columns of interest

dat <- dat %>%
    select(
        partid,
        role,
        age,
        race,
        ethnicity,
        gender,
        marital,
        canc_site,
        canc_stage
    )

## Separate site/stage into own file

dat_cancer <- dat %>%
    filter(role == "PT") %>%
    select(partid, canc_site, canc_stage)

## Separate patient and caregiver, drop stage

dat_pt <- dat %>%
filter(role == "PT") %>%
select(-c(canc_site, canc_stage))

dat_cg <- dat %>%
filter(role == "CG") %>%
select(-c(canc_site, canc_stage))

# Data Out______________________________________________________________________

path <- "IntData/"

write_csv(x = dat_cancer, file = paste0(path, "dz_update.csv"))
write_csv(x = dat_cg, file = paste0(path, "cg_update.csv"))
write_csv(x = dat_pt, file = paste0(path, "pt_update.csv"))
