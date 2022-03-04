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
source("Functions/tidying.r")

## Functions
collapse_vars <- function(dat) {
    dat <- dat %>%
        mutate(
            education = case_when(
                education == 0 | education == 1 ~ 0,
                education == 2 | education == 3 ~ 1,
                education == 4 ~ 2
            ),
            income = case_when(
                income == 0 | income == 1 | income == 2 ~ 0,
                income == 3 | income == 4 | income == 5 ~ 1,
                income == 6 ~ 2
            ),
            race = case_when(
                race == 0 ~ 0,
                race == 1 | race == 2 ~ 1,
                race == 3 ~ 2,
                race == 4 ~ 3,
                race == 5 ~ 4
            )
        )
}
## Data in

### Patient
dat_pat <- read_csv("IntData/demo_pat.csv")
dat_pat_update <- read_csv("IntData/pt_update.csv")

### Caregiver
dat_care <- read_csv("IntData/demo_care.csv")
dat_care_update <- read_csv("IntData/cg_update.csv")

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

dat_pat_update <- dat_pat_update %>%
    mutate(
        race = shift_left(.$race),
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
dat_care_update <- dat_care_update %>%
    mutate(
        race = shift_left(.$race),
        marital = shift_left(.$marital)
    )

### Join updated data and update missingness

dat_pat <- dat_pat %>%
    full_join(dat_pat_update,
        by = "partid"
    ) %>%
    mutate(
        age = case_when(
            is.na(age.x) == TRUE ~ age.y,
            TRUE ~ age.x
        ),
        gender = if_else(
            is.na(gender.x) == TRUE, gender.y, gender.x
        ),
        ethnicity = if_else(
            is.na(ethnicity.x) == TRUE, ethnicity.y, ethnicity.x
        ),
        race = if_else(
            is.na(race.x) == TRUE, race.y, race.x
        ),
        marital = if_else(
            is.na(marital.x) == TRUE, marital.y, marital.x
        )
    ) %>%
    select(!contains("."))
dat_pat$role <- NULL

dat_care <- dat_care %>%
    full_join(dat_care_update,
        by = "partid"
    ) %>%
    mutate(
        age = case_when(
            is.na(age.x) == TRUE ~ age.y,
            TRUE ~ age.x
        ),
        gender = if_else(
            is.na(gender.x) == TRUE, gender.y, gender.x
        ),
        ethnicity = if_else(
            is.na(ethnicity.x) == TRUE, ethnicity.y, ethnicity.x
        ),
        race = if_else(
            is.na(race.x) == TRUE, race.y, race.x
        ),
        marital = if_else(
            is.na(marital.x) == TRUE, marital.y, marital.x
        )
    ) %>%
    select(!contains("."))
dat_care$role <- NULL

### Collapse Income and Education
dat_pat <- dat_pat %>%
    collapse_vars()

dat_care <- dat_care %>%
    collapse_vars()

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
