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
## IntData/dz_update.csv

## Output:
## IntData/dztype.csv

################################################################################

## Packages

library(tidyverse)
library(readxl)
source("00Functions.r")

## Data in

dat <- readxl::read_xlsx("RawData/final DX and stage 092920.xlsx")
dat_update <- read_csv("IntData/dz_update.csv")

## Join together

dat <- dat %>%
    full_join(dat_update, by = c("study_ID" = "partid"))

## Select variables

dat <- dat %>%
    select(study_ID,
        DX,
        "stage" = canc_stage
    )
## Change Cancer Stage to a number
dat <- dat %>%
    mutate(
        stage = as.character(case_when(
            stage == "2" ~ 0,
            stage == "2A" ~ 0,
            stage == "2B" ~ 0,
            stage == "2C" ~ 0,
            stage == "3" ~ 1,
            stage == "3A" ~ 1,
            stage == "3B" ~ 1,
            stage == "3C" ~ 1,
            stage == "4" ~ 2,
            stage == "4A" ~ 2,
            stage == "4B" ~ 2
        )),
        dx = as.character(case_when(
            DX == "Breast" ~ 0,
            DX == "Colon" ~ 1,
            DX == "Lung" ~ 2,
            DX == "Rectum" ~ 3
        ))
    ) %>%
    select(-DX)

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
    lengthen(x = "patient") %>%
    mutate(value_caregiver = value_patient) ## this just makes it so I can join

## Data Out

write_csv(dat_long, file = "IntData/dztype.csv")
