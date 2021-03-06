################################################################################

## Author: Zach Rivers, @riverspharmd, zrivers@fredhutch.org
## Contributors:

## Purpose:

## Depends on:
## 07PredictorExploration.r
## 10COSTTidyJoin.r
## tidyverse
## gtsummary
## flextable

## Input:
## IntData/dat_wide.csv
## IntData/cost.csv

## Output:

################################################################################

# Packages______________________________________________________________________
library(tidyverse)
library(gtsummary)
library(flextable)

# Functions_____________________________________________________________________
source("Functions/importing.r")
source("Functions/regressions.r")
source("Functions/tidying.r")
# Data In_______________________________________________________________________
pred_wide <- read_csv(
    "IntData/dat_wide.csv"
)

## COST

cost <- read_csv("IntData/cost.csv",
    col_types = "ccccn"
)
# Data Prep_____________________________________________________________________
## Clean COST-------------------------------------------------------------------
cost_outcomes <- build_outcome(dat = cost)

## Prep Predictors-------------------------------------------------------------
### Factor
pred_wide <- label_factors(dat = pred_wide)

### Build predictor datset
pred_wide <- build_predictors(dat = pred_wide)

### Label predictor dataset
var_label(pred_wide) <- lr_labels

## Join with outcomes-----------------------------------------------------------
pred_outcomes <- join_pred_out(
    pred_dat = pred_wide,
    outcome_dat = cost_outcomes
)
# Run Models____________________________________________________________________

### Build Model Pieces
pat_var <- c(
    "age_p",
    "gender_p",
    "ethnicity_p",
    "race_p",
    "education_p",
    "comorbid_sum_p"
)
clin_var <- c(
    "location",
    "stage",
    "dx"
)
care_var <- c(
    "age_c",
    "gender_c",
    "ethnicity_c",
    "race_c",
    "education_c",
    "comorbid_sum_c"
)

dyad_var <- c(
    "marital",
    "income",
    "children"
)
### Build Model Inputs
pat_input <- list(
    p1 = c(pat_var),
    p2 = c(pat_var, clin_var),
    p3 = c(pat_var, care_var),
    p4 = c(pat_var, dyad_var),
    p5 = c(pat_var, clin_var, care_var),
    p6 = c(pat_var, clin_var, dyad_var, "income * location"),
    p7 = c(pat_var, care_var, dyad_var),
    p8 = c(pat_var, clin_var, care_var, dyad_var, "income * location")
)

care_input <- list(
    c1 = c(care_var),
    c2 = c(care_var, clin_var),
    c3 = c(care_var, pat_var),
    c4 = c(care_var, dyad_var),
    c5 = c(care_var, clin_var, pat_var),
    c6 = c(care_var, clin_var, dyad_var, "income * location"),
    c7 = c(care_var, pat_var, dyad_var),
    c8 = c(care_var, clin_var, pat_var, dyad_var, "income * location")
)

dyad_input <- c(pat_input[1:7], care_input[c(1, 2, 4, 6, 8)])
mod_input <- list(pat_input, care_input, dyad_input)
### Build Model Outcomes
outcome_list <- list("p_fintox", "c_fintox", "con")

### Build Table Spanners
p_spanner <- c(
    "Bivariate",
    "Patient",
    "Patient and Clinical",
    "Patient and Caregiver",
    "Patient and Dyad",
    "Patient, Clinical, and Caregiver",
    "Patient, Clinical, and Dyad",
    "Patient, Caregiver, and Dyad",
    "All Predictors"
)
c_spanner <- c(
    "Bivariate",
    "Caregiver",
    "Caregiver and Clinical",
    "Caregiver and Patient",
    "Caregiver and Dyad",
    "Caregiver, Clinical, and Patient",
    "Caregiver, Clinical, and Dyad",
    "Caregiver, Patient, and Dyad",
    "All Predictors"
)

dyad_spanner <- c(p_spanner[1:8], c_spanner[c(2, 3, 4, 7, 9)])
span_list <- list(p_spanner, c_spanner, dyad_spanner)

### Model output list
joined_model_list <- list(NA, NA, NA)
### Run models
# https://aosmith.rbind.io/2019/06/24/function-for-model-fitting/
for (i in 1:3) {
    log_reg_list <- map(.x = mod_input[[i]], ~ mv_log_reg(
        input = .x,
        resp = outcome_list[[i]],
        dat = pred_outcomes
    ))

    ### Tidy model fits
    log_reg_list_tidy <- map(
        .x = log_reg_list,
        ~ tidy_mv_log_reg(mod_fit = .x)
    )

    ### Join Models

    joined_model_list[[i]] <- tbl_merge(log_reg_list_tidy,
        tab_spanner = span_list[[i]]
    )
    print(paste("Completed Loop", i, "of Three"))
}
joined_model_list
# Data out______________________________________________________________________
