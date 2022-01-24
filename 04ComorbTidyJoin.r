###############################################################################
## Purpose: This file takes the comorbidity files from
## 01PatientDataCleanSplit.r and 02CaregiverDataCleanSplit.r and joins them.

## Depends on:
## 00Functions.r
## 01PatientDataCleanSplit.r
## 02CaregiverDataCleanSplit.r
## tidyverse
##

## Input:
## IntData/dat_pat.csv
## IntData/dat_care.csv

## Output:
## IntData/comor_allobs.csv
## IntData/comor.csv
################################################################################
## Packages:

library(tidyverse)
source("00Functions.r")


## Data in

### Patient
dat_pat <- read_csv("IntData/comor_pat.csv")

### Caregiver
dat_care <- read_csv("IntData/comor_care.csv")

## Tidying

### Remove other rows: comorbidities were only captured at the first timepoint

#### Patient
dat_pat <- timepoint(dat_pat, 1)

#### Caregiver
dat_care <- timepoint(dat_care, 1)

### Make long

#### Patient
dat_pat_long <- lengthen(dat_pat, x = "patient")

#### Caregiver
dat_care_long <- lengthen(dat_care, x = "caregiver")

### Join data

dat_long <- dat_pat_long %>%
  left_join(dat_care_long, by = c("partid", "observation"))

### Create dataset with only cumulative score

dat_long_cumu <- dat_long %>%
  filter(observation == "comorbid_sum") ## is this just sum, or is it weighted?

## Data Out
write_csv(x = dat_long, file = "IntData/comor_allobs.csv")
write_csv(x = dat_long_cumu, file = "IntData/comor.csv")
