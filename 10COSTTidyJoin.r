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

## Note: The logical and categorical variables are created using an adjusted sum
## that sets all missing values to 4, as this represents the highest possible
## score a respondent could provide. This means that some patients will show up
## in the lgl/cat datasets, but not in the continuous dataset.

###############################################################################

# Packages______________________________________________________________________
library(tidyverse)
source(file = "00Functions.r")

# Functions_____________________________________________________________________

# Data In_______________________________________________________________________
## Patient
dat_p <- read_csv(
    file = "IntData/facitcost_pat.csv"
)

## Caregiver
dat_c <- read_csv(
    file = "IntData/facitcost_care.csv"
)

## PartID of missing demographics
partid_missing_demo <- read_csv(
    file = "IntData/partid_missing_demo.csv"
)$value

# Analysis______________________________________________________________________
## Score and Dichotomize Cost--------------------------------------------------

### Create lists for looping input and output
df_list <- list(dat_p, dat_c)

df_list_scored <- list()

### Loop Over List:

for (i in seq_along(df_list)) {
    ##### Subtracted 1 from all scores to match actual FACIT-COST
    dat <- df_list[[i]] %>%
        mutate(across(matches("facit_cost"), shift_left))

    ##### Reverse score per rubric
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
    ##### Count_NA add
    dat <- dat %>%
        mutate(count_na = rowSums(is.na(.)))

    #### Filter for non-answers
    dat <- dat %>%
        filter(count_na < 3) ## Keeps 2 or fewer per conversation

    ##### Summarize scores
    dat <- dat %>%
        rowwise() %>% ## swaps it so I can mutate this way
        mutate(
            sum = sum(c_across(facit_cost01:facit_cost11), na.rm = TRUE)
        ) %>%
        ungroup()

    #### Filter for non-answers


    #### Dichotomize COST


    dat <- dat %>%
        mutate(
            cost_lgl = if_else(sum < 26, 1, 0)
        )


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


## Merge datasets---------------------------------------------------------------

### Create narrow datasets
dat_narrow_p <- df_list_scored[[1]] %>%
    select(partid, redcap_event_name,
        "sum_p" = sum,
        "lgl_p" = cost_lgl,
        "cat_p" = cost_cat
    )

dat_narrow_c <- df_list_scored[[2]] %>%
    select(partid, redcap_event_name,
        "sum_c" = sum,
        "lgl_c" = cost_lgl,
        "cat_c" = cost_cat
    )

dat <- dat_narrow_p %>%
    left_join(dat_narrow_c, by = c("partid", "redcap_event_name"))

### Create full datasets
dat_p <- df_list_scored[[1]] %>%
    mutate(src = "patient")

dat_c <- df_list_scored[[2]] %>%
    mutate(src = "caregiver")

dat_full <- rbind(dat_p, dat_c)



### Create concordance variable
dat <- dat %>%
    mutate(
        con_raw = sum_p - sum_c, ## Raw
        con_dich = case_when( ## Logical
            lgl_p == 0 & lgl_c == 0 ~ 0,
            lgl_p < lgl_c ~ 2,
            lgl_p > lgl_c ~ 3,
            lgl_p == 1 & lgl_c == 1 ~ 1
        ),
        con_cat = case_when( ## Categorical
            cat_p == 0 & cat_c == 0 ~ 0,
            cat_p < cat_c ~ 2,
            cat_p > cat_c ~ 3,
            cat_c == cat_p ~ 1
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

# Data out______________________________________________________________________
write_csv(dat, file = "IntData/cost.csv")
write_csv(dat_full, file = "IntData/cost_full.csv")
