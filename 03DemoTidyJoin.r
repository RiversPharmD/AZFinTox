###############################################################################
## Purpose: This file takes the demographic files from
## 01PatientDataCleanSplit.r and 02CaregiverDataCleanSplit.r and joins them.
## Depends on:
## 00Functions.r
## 01PatientDataCleanSplit.r
## 02CaregiverDataCleanSplit.r
## tidyverse

## Input:
## IntData/demo_pat.csv
## IntData/demo_care.csv

## Output:
## IntData/demo.csv

################################################################################
## Packages:
library(tidyverse)
source("00Functions.r")
## Data in

### Patient
dat_pat <- read_csv("IntData/demo_pat.csv")

### Caregiver
dat_care <- read_csv("IntData/demo_care.csv")

## Tidying

### Remove other rows: demographics were only captured at the first timepoint

#### Patient
## The 1000 cases come from Duke, the 2000 cases from SCCA.
dat_pat <- timepoint(dat_pat, num = 1) %>%
    mutate(
        location = if_else(partid < 2000, 0, 1),
        race = shift_left(.$race),
        education = shift_left(.$education),
        income = shift_left(.$income),
        marital = shift_left(.$marital)
    )

#### Caregiver
dat_care <- timepoint(dat_care, num = 1) %>%
    mutate(
        location = if_else(partid < 2000, 0, 1),
        race = shift_left(.$race),
        education = shift_left(.$education),
        income = shift_left(.$income),
        marital = shift_left(.$marital)
    )
### Make long

#### Patient
dat_pat_long <- lengthen(dat_pat, x = "patient")

#### Caregiver
dat_care_long <- lengthen(dat_care, x = "caregiver")

### Join data

demo <- dat_pat_long %>%
    full_join(dat_care_long, by = c("partid", "observation")) %>%
    filter(observation != "childrennum")
## drop childrennum because it's missing for a lot

## Data Out

write_csv(x = demo, file = "IntData/demo.csv")
