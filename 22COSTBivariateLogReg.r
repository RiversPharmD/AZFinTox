################################################################################

## Author: Zach Rivers, @riverspharmd, zrivers@fredhutch.org
## Contributors:

## Purpose: This file runs the logistic regression model for bivariate logreg.

## Depends on:
## 07PredictorExploration.r
## 10COSTTidyJoin.r
## tidymodels

## Input:
##
## Output:

## NOTE: This scoring system is inverse of COST scoring, where 0 is no FT and 1
## is FT.

################################################################################


# Packages______________________________________________________________________
library(tidyverse)
library(gtsummary)
library(flextable)


# Functions_____________________________________________________________________
source("00Functions.r")

bounded_style_ratio <- function(x, min = -Inf, max = Inf, ...) {
    purrr::map_chr(
        x,
        function(x) {
            if (isTRUE(x < min)) {
                return(paste0("<", gtsummary::style_ratio(min, ...)))
            }
            if (isTRUE(x > max)) {
                return(paste0(">", gtsummary::style_ratio(max, ...)))
            }
            gtsummary::style_ratio(x, ...)
        }
    )
}
# Data In_______________________________________________________________________

## Predictors

pred_wide <- read_csv(
    "IntData/dat_wide.csv"
)

## COST

cost <- read_csv("IntData/cost.csv",
    col_types = "ccccn"
)

# Data Prep_____________________________________________________________________

## Clean COST

cost <- cost %>%
    dplyr::filter(observation == "con_dich") %>%
    select(-survey) %>%
    rename("con" = score) %>%
    mutate(con = as_factor(if_else(con > 0, 1, con))) %>%
    select(-observation)

## Split COST by timepoint
cost_list <- cost %>%
    group_by(redcap_event_name) %>%
    group_split()

## Clean Predictors

### Factor

pred_wide <- label_factors(dat = pred_wide)

### caregiver
#### Split out data
care_wide <- pred_wide %>%
    filter(source == "Caregiver")

#### Select columns that are caregiver specific
care_wide <- care_wide %>%
    select(partid,
        "age_c" = age,
        "gender_c" = gender,
        "ethnicity_c" = ethnicity,
        "race_c" = race,
        "education_c" = education,
        "comorbid_sum_c" = comorbid_sum
    )

### Patient

#### Pull out data
pat_wide <- pred_wide %>%
    filter(source == "Patient")

#### Rename columns

pat_wide <- pat_wide %>%
    select(partid,
        "age_p" = age,
        "gender_p" = gender,
        "ethnicity_p" = ethnicity,
        "race_p" = race,
        "education_p" = education,
        "comorbid_sum_p" = comorbid_sum,
        everything()
    ) %>%
    select(-source)

### Rejoin predictors

pred_wide <- pat_wide %>%
    left_join(care_wide) %>%
    mutate(partid = as.character(partid))

### pull out predictor names

pred_names <- pred_wide %>%
    select(-partid) %>%
    names()

## Join with outcomes

cost_list <- cost_list %>%
    map(~ left_join(
        x = .,
        y = pred_wide,
        by = "partid"
    ))





# Run Models____________________________________________________________________
## Build empty list-------------------------------------------------------------
lr_list <- list()

## loop over timepoints---------------------------------------------------------
for (i in 1:4) {
    lr_list[[i]] <- cost_list[[i]] %>%
        select(-c(redcap_event_name, partid)) %>%
        filter(is.na(age_p) == FALSE) %>%
        tbl_uvregression(
            y = con,
            method = glm,
            method.args = list(family = binomial),
            exponentiate = TRUE,
            estimate_fun = purrr::partial(bounded_style_ratio, min = 0.001, max = 10)
        ) %>%
        modify_footnote(c(estimate, ci) ~ "ORs <0.001 or larger than 10 shown as '<0.001' and '>10.00'") %>%
        as_flex_table()
}

# Data Out _____________________________________________________________________
## Path-------------------------------------------------------------------------
start <- "OutData/"
fn <- "uv_lr_"
end <- ".docx"

## Loop over print function-----------------------------------------------------
for (i in 1:4) {
    save_as_docx(
        lr_list[[i]],
        path = paste0(
            start,
            fn,
            i,
            end
        )
    )
}
