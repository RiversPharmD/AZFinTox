################################################################################

## Author: Zach Rivers, @riverspharmd, zrivers@fredhutch.org
## Contributors:

## Purpose: This file loads the raw cancer case data and tidys it. It recodes
## cancer stage into a number so that it accurately handles the levels of stage.

## Depends on:
## tidyverse
## readxl


## Input:
## RawData/final DX and stage 092920.xlsx

## Output:
## IntData/dztype.csv

################################################################################

## Packages

library(tidyverse)
library(readxl)
source("00Functions.r")

## Data in

dat <- readxl::read_xlsx("RawData/final DX and stage 092920.xlsx")



## Change Cancer Stage to a number
dat <- dat %>%
mutate(stage = as.character(case_when(stage == "2" ~ 0,
stage == "2A" ~ 1,
stage == "2B" ~ 2,
stage == "3" ~ 3,
stage == "3A" ~ 4,
stage == "3B" ~ 5,
stage == "3C" ~ 6,
stage == "4" ~ 7,
stage == "4A" ~ 8,
stage == "4B" ~ 9)),
dx = as.character(case_when(
    DX == "Breast" ~ 0,
    DX == "Colon" ~ 1,
    DX == "Lung" ~ 2,
    DX == "Rectum" ~ 3
))) %>%
select (-DX)

## Descriptive Stats

dat_sum <- dat %>%
    group_by(dx, stage) %>%
    count()
## Add column so I can use my silly function

dat <- dat %>%
mutate(redcap_event_name = 1) %>%
mutate(partid = study_ID) %>%
select(-study_ID)

## Make Long
dat_long <- dat %>%
lengthen(x = "patient"
) %>%
mutate(value_caregiver = value_patient) ## this just makes it so I can join

## Data Out

write_csv(dat_long, file = "IntData/dztype.csv")
