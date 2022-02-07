###############################################################################
## Purpose: this file loads COST survey data
## from each timepoint, cleans it, and outputs a joined dataset.

## Depends:
## O1PatientDataCleanSplit.r
## 02CaregiverDataCleanSplit.r
## tidyverse

## Inputs:
## IntData/facitcost_pat.csv
## IntData/facitcost_care.csv
## IntData/partid_missing_demo.csv

## Outputs:
## IntData/cost.csv
## IntData/cost_full.csv

###############################################################################

## Packages
library(tidyverse)
source(file = "00Functions.r")

## Functions


## Data In

### Patient

dat_p <- read_csv(
    file = "IntData/facitcost_pat.csv"
)

### Caregiver

dat_c <- read_csv(
    file = "IntData/facitcost_care.csv"
)

partid_missing_demo <- read_csv(
    file = "IntData/partid_missing_demo.csv"
)$value
## Data Tidy

### Create list
df_list <- list(dat_p, dat_c)
df_list_scored <- list()
### Loop Over List

for (i in seq_along(df_list)) {

    #### Patient

    ##### Subtracted 1 from all scores to match actual FACIT-COST

    dat <- df_list[[i]] %>%
        mutate(across(matches("facit_cost"), shift_left))

    ##### Per rubric, 2,3,4,5,8,9, and 10 are reverse scored

    dat <- dat %>%
        mutate(across(
            matches(c("02", "03", "04", "05", "08", "09", "10")),
            ~ case_when(
                . == 0 ~ 4,
                . == 1 ~ 3,
                . == 2 ~ 2,
                . == 3 ~ 1,
                . == 4 ~ 0
            )
        ))

    ##### Scores
    dat <- dat %>%
        rowwise() %>% ## swaps it so I can mutate this way
        mutate(sum = sum(c_across(facit_cost01:facit_cost11))) %>%
        ungroup()

    ### Dichotomize COST


    dat <- dat %>%
        mutate(cost_lgl = if_else(sum < 26, 1, 0))


    ### Categorize COST

    dat <- dat %>%
        mutate(cost_cat = case_when(
            sum >= 26 ~ 0,
            sum < 26 & sum >= 14 ~ 1,
            sum < 14 & sum > 0 ~ 2,
            sum == 0 ~ 3
        ))

    df_list_scored[[i]] <- dat
}


### Merge datasets



#### Narrow datasets

##### Patient

dat_narrow_p <- df_list_scored[[1]] %>%
    select(partid, redcap_event_name,
        "sum_p" = sum,
        "lgl_p" = cost_lgl,
        "cat_p" = cost_cat
    )

##### Caregiver
dat_narrow_c <- df_list_scored[[2]] %>%
    select(partid, redcap_event_name,
        "sum_c" = sum,
        "lgl_c" = cost_lgl,
        "cat_c" = cost_cat
    )

#### Full datasets

##### Patient
dat_p <- df_list_scored[[1]] %>%
    mutate(src = "patient")

##### Caregiver
dat_c <- df_list_scored[[2]] %>%
    mutate(src = "caregiver")


#### Create COST dataset

##### Narrow
dat <- dat_narrow_p %>%
    left_join(dat_narrow_c, by = c("partid", "redcap_event_name"))

##### Full

dat_full <- rbind(dat_p, dat_c)

#### Look for missingness

dat_full <- dat_full %>%
    mutate(count_na = rowSums(is.na(.)))

#### Pull out partid's of indiviudals with <11 but >0 NAs

dat_missing_some_answers <- dat_full %>%
    filter(count_na > 0 & count_na < 14)

#### Filter for missingness

dat_full <- dat_full %>%
filter(count_na == 0)

### Create concordance variable

#### Directional

dat <- dat %>%
    mutate(
        con_raw = sum_p - sum_c, ## Raw
        con_dich = case_when( ## Logical
            lgl_p == lgl_c ~ 0,
            lgl_p < lgl_c ~ 1,
            lgl_p > lgl_c ~ 2
        ),
        con_cat = case_when( ## Categorical
            cat_p == cat_c ~ 0,
            cat_p < cat_c ~ 1,
            cat_p > cat_c ~ 2
        )
    )


### Lengthen and add survey type

dat <- dat %>%
    pivot_longer(
        cols = !partid:redcap_event_name,
        names_to = "observation",
        values_to = "score"
    ) %>%
    mutate(survey = "COST") %>%
    select(partid, redcap_event_name, survey, observation, score)

## Data out

write_csv(dat, file = "IntData/cost.csv")
write_csv(dat_full, file = "IntData/cost_full.csv")
write_csv(dat_missing_some_answers, file = "IntData/COST_missing_answers.csv")
