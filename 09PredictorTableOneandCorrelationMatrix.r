################################################################################

## Author: Zach Rivers, @riverspharmd, zrivers@fredhutch.org
## Contributors:

## Purpose: Produce a table one, and a correlation matrix for the predictor
## variables.

## Depends on:
## 08PredictorMissingness.r
## corrplot
## table1
## tidyverse

## Input:
## IntData/dat_long.csv
## IntData/dat_wide.csv

## Output:

################################################################################

# Packages______________________________________________________________________
library(corrplot)
library(table1)
library(tidyverse)
source("00Functions.r")

# Functions_____________________________________________________________________

# Data In_______________________________________________________________________
demo_long <- read_csv("IntData/dat_long.csv")
demo_wide <- read_csv("IntData/dat_wide.csv")

# Analysis______________________________________________________________________

## Create Table One-------------------------------------------------------------

### Factor Data
demo_wide <- demo_wide %>%
label_factors()

### Split into source
pat_wide <- demo_wide %>%
filter(source == "Patient") %>%
select(partid,
location, age, gender, ethnicity, race, education)

care_wide <- demo_wide %>%
filter(source == "Caregiver") %>%
select(partid,
location, age, gender, ethnicity, race, education)

dyad_wide <- demo_wide %>%



# Data Out______________________________________________________________________
