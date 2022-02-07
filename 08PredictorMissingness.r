################################################################################

## Author: Zach Rivers, @riverspharmd, zrivers@fredhutch.org
## Contributors:

## Purpose: Create a table 1 of predictor variables

## Depends on:
## 06PredictorJoin.r
## 00Functions.r
## corrplot
## naniar
## table1
## tidyverse

## Input:
## IntData/pred_wide.csv

## Output:
## IntData/missing_partid.csv
## IntData/dat_long.csv
## IntData/dat_wide.csv

## Comments: This file is maybe something that shouldn't be run independently,
## it shows how I did EDA on the predictor variables and handled missingness.
## With every predictor variable, I came up with a programatic way to identify
## the PARTID that needed an update for that variable or was missing. The
## updates were handled in-line, and the ones that were being removed were
## collected under varname_missing_partid. Each variable's missing list was
## joined into missing_partid, which was then exported as a single column .csv
## file. This .csv then allows future analysis to drop all the partids that we
## identify in this exploration.

################################################################################

# Packages______________________________________________________________________
library(corrplot)
library(naniar)
library(table1)
library(tidyverse)
source("00Functions.r")

# Functions_____________________________________________________________________

# Data In_______________________________________________________________________
dat <- read_csv(file = "IntData/pred_wide.csv")
dat_long <- read_csv(
    file = "IntData/pred.csv",
    col_types = "cccc"
)

# Exploration___________________________________________________________________

## Visualize missingness--------------------------------------------------------

### Vis_Miss()
naniar::vis_miss(dat)

### gg_miss_upset()
naniar::gg_miss_upset(dat, nsets = n_var_miss(dat))

### Create vector to hold missing partids
vec_missing_partid <- vector()

## Address Income Missingness---------------------------------------------------

### Align missingness
dat_long <- align_missing(
    dat = dat_long,
    x = "income"
)

### Align discordance
dat_long <- align_discordant(
    dat = dat_long,
    x = "income"
)

### Calculate discordance measure
income_list <- discordance(
    dat = dat_long,
    x = "income"
)

### extract income missing partids
vec_missing_partid <- c(extract_missing(
    dat = income_list[[1]],
    x = disc
), vec_missing_partid)

## Children---------------------------------------------------------------------

### Filter out missing demographics
dat_long <- dat_long %>%
    filter(!partid %in% vec_missing_partid)

### Align missing
dat_long <- align_missing(
    dat = dat_long,
    x = "children"
)

### Align Discordant
dat_long <- align_discordant(
    dat = dat_long,
    x = "children"
)

### Calculate Discordance
children_list <- discordance(
    dat = dat_long,
    x = "children"
)

### Extract Missing partids
vec_missing_partid <- c(extract_missing(
    dat = children_list[[1]],
    x = disc
), vec_missing_partid)

## Visualize Missingnes---------------------------------------------------------

### Drop missingness
dat_long <- dat_long %>%
    filter(!partid %in% vec_missing_partid)

### Split Data Again
pat <- dat_long %>%
    select(-value_caregiver)

care <- dat_long %>%
    select(-value_patient)

### Widen
pat_wide <- pat %>%
    pivot_wider(
        names_from = observation,
        values_from = value_patient
    ) %>%
    mutate(source = 0)

care_wide <- care %>%
    pivot_wider(
        names_from = observation,
        values_from = value_caregiver
    ) %>%
    mutate(source = 1)

### Rejoin
dat_wide <- pat_wide %>%
    rbind(care_wide)

### gg_miss_upset()
naniar::gg_miss_upset(dat_wide, nsets = n_var_miss(dat_wide))

## Create final drop list-------------------------------------------------------

### Find all variables with a NA left
dat_wide <- dat_wide %>%
    mutate(count_na = rowSums(is.na(.)))

### Select the rows where count_na is larger than 0
dat_missing <- dat_wide %>%
    filter(count_na > 0)

### Pull unique partids
vec_other_missing <- unique(dat_missing$partid)

### Join with other drop list
vec_missing_partid <- unique(c(vec_missing_partid, vec_other_missing))

### Apply drop list to long and wide datasets
dat_long <- dat_long %>%
    filter(!partid %in% vec_missing_partid)

dat_wide <- dat_wide %>%
    filter(!partid %in% vec_missing_partid) %>%
    select(-count_na)

# Data out______________________________________________________________________
write_csv(dat_long, "IntData/dat_long.csv")
write_csv(dat_wide, "IntData/dat_wide.csv")
write_csv(as_tibble(vec_missing_partid), "IntData/partid_missing_demo.csv")
