################################################################################

## Author: Zach Rivers, @riverspharmd, zrivers@fredhutch.org
## Contributors:

## Purpose: This file runs the logistic regression model for bivariate and
## multivariate logreg across three different outcomes.

## Depends on:
## 07PredictorExploration.r
## 10COSTTidyJoin.r
## tidyverse
## gtsummary
## flextable
## labelled

## Input:
## IntData/dat_wide.csv
## IntData/cost.csv

## Output:
##

## NOTE: This scoring system is inverse of COST scoring, where 0 is no FT and 1
## is FT. This applies to the discordance as 1: Discordant, 0: Concordant

################################################################################


# Packages______________________________________________________________________
library(tidyverse)
library(gtsummary)
library(flextable)
library(labelled)


# Functions_____________________________________________________________________
source("Functions/importing.r")
source("Functions/regressions.r")
source("Functions/tidying.r")

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
## Prep Outcomes----------------------------------------------------------------
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
) %>%
    select(-c(partid)) %>%
    filter(is.na(age_p) == FALSE)

# Run Models____________________________________________________________________
## Prep Data Structures---------------------------------------------------------
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
# This just ensures no duplicates in the dyad analysis
dyad_input <- c(
    pat_input[1:7],
    care_input[c(1, 2, 4, 6, 8)]
)

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

# Same as before, no duplicates in the dyad analysis
dyad_spanner <- c(
    p_spanner[1:8],
    c_spanner[c(2, 3, 4, 7, 9)]
)

### Build empty lists
bv_lr_list <- list()
mv_lr_list <- list()
joined_model_list <- list()
joined_model_list_tidy <- list()

### Build loop input Lists
outcome_list <- list("p_fintox", "c_fintox", "con")
span_list <- list(p_spanner, c_spanner, dyad_spanner)
mod_input <- list(pat_input, care_input, dyad_input)
title_list <- list(
    "Patient Financial Toxicity",
    "Caregiver Financial Toxicity",
    "Discordance of Financial Toxicity Between Patients and Caregivers"
)

### loop over datsets
for (i in 1:3) {
    # Pull out parameters for this iteration
    mv_input <- mod_input[[i]]
    resp <- outcome_list[[i]]
    outcome_exclude <- unlist(outcome_list[-i])
    exp <- TRUE

    # Fit and tidy bivariate logreg
    bv_lr_list[[i]] <- bv_log_reg(
        dat = pred_outcomes,
        resp = resp,
        exp = exp
    )
    # Tell me that you're still running
    print(paste("Completed bivariate", i, "of Three"))

    # Fit multivariate logreg
    mv_log_reg_fits <- map(
        .x = mv_input,
        ~ mv_log_reg(
            input = .x,
            resp = resp,
            dat = pred_outcomes
        )
    )

    ### Tidy multivariate logreg
    mv_lr_list[[i]] <- map(
        .x = mv_log_reg_fits,
        ~ tidy_mv_log_reg(
            mod_fit = .x,
            exp = exp
        )
    )
    # Keep running
    print(paste("Completed multivariate", i, "of Three"))

    ### Join bv and mv together by outcome
    joined_model_list[[i]] <- c(
        bv_lr_list[i],
        mv_lr_list[[i]]
    )

    joined_model_list_tidy[[i]] <- tbl_merge(joined_model_list[[i]],
        tab_spanner = span_list[[i]]
    ) %>%
        modify_caption(title_list[[i]])

    # Wheeeee more loops
    print(paste("Completed Loop", i, "of Three"))
}


# Data Out _____________________________________________________________________


## Logistic Regression-----------------------------------------------------
