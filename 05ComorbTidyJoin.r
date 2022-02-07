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

#### Patient (remove cancer from score here because all patients have it, also
## created another variable where all patients get the point for cancer)
dat_pat <- timepoint(dat_pat, 1) %>%
    select(partid,
        redcap_event_name,
        hrt_dz = comorbid01,
        htn = comorbid02,
        lng_dz = comorbid03,
        diab = comorbid04,
        stm_dz = comorbid05,
        kid_dz = comorbid06,
        liv_dz = comorbid07,
        bld_dz = comorbid08,
        can = comorbid09,
        dep = comorbid10,
        art_dz = comorbid11,
        back = comorbid12,
        rhe_dz = comorbid13
    ) %>%
    mutate(can_adj = 1) %>%
    mutate(#comorbid_adj = rowSums(across(c(hrt_dz:bld_dz, dep:can_adj))),
        comorbid_sum = rowSums(across(c(hrt_dz:bld_dz, dep:rhe_dz)), na.rm =TRUE)

    )




#### Caregiver
dat_care <- timepoint(dat_care, 1) %>%
    select(partid,
        redcap_event_name,
        hrt_dz = comorbid01,
        htn = comorbid02,
        lng_dz = comorbid03,
        diab = comorbid04,
        stm_dz = comorbid05,
        kid_dz = comorbid06,
        liv_dz = comorbid07,
        bld_dz = comorbid08,
        can = comorbid09,
        dep = comorbid10,
        art_dz = comorbid11,
        back = comorbid12,
        rhe_dz = comorbid13
    ) %>%
    mutate(comorbid_sum = rowSums(across(c(hrt_dz:rhe_dz)), na.rm =TRUE)
)

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
    filter(observation == "comorbid_sum" )

## Data Out
write_csv(x = dat_long, file = "IntData/comor_allobs.csv")
write_csv(x = dat_long_cumu, file = "IntData/comor.csv")
