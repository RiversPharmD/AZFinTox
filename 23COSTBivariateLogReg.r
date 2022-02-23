################################################################################

## Author: Zach Rivers, @riverspharmd, zrivers@fredhutch.org
## Contributors:

## Purpose: This file runs the logistic regression model for bivariate logreg,
## across three different outcomes.

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
##

## NOTE: This scoring system is inverse of COST scoring, where 0 is no FT and 1
## is FT. This applies to the discordance as 1: Discordant, 0: Concordant

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
## Clean COST-------------------------------------------------------------------
### Filter for timepoint 1
cost <- cost %>%
    filter(redcap_event_name == 1)

### Filter for survey availability

cost <- cost %>%
    import_available_survey_data(
        timepoint = 1,
        survey = "COST",
        obs = "dyad"
    )


### Build outcomes
cost_pat_fintox <- cost %>%
    dplyr::filter(observation == "lgl_p") %>%
    select(-c(survey, observation)) %>%
    rename("p_fintox" = score)

cost_care_fintox <- cost %>%
    dplyr::filter(observation == "lgl_c") %>%
    select(-c(survey, observation)) %>%
    rename("c_fintox" = score)

cost_convergence <- cost %>%
    dplyr::filter(observation == "con_dich") %>%
    select(-c(survey)) %>%
    rename("con" = score) %>%
    mutate(con = as_factor(if_else(con > 1, 1, 0))) %>%
    select(-observation)

### Join outcomes together
cost_outcomes <- cost_pat_fintox %>%
    left_join(cost_care_fintox) %>%
    left_join(cost_convergence)

## Clean Predictors-------------------------------------------------------------
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

### pull out factor predictor names

pred_names <- pred_wide %>%
    select(-partid) %>%
    select(where(is.factor)) %>%
    names()

## Join with outcomes-----------------------------------------------------------

pred_outcomes <- pred_wide %>%
    inner_join(cost_outcomes)

# Run Models____________________________________________________________________
## Labels
lr_labels <- list(
    age_p ~ "Patient Age",
    gender_p ~ "Patient Gender",
    ethnicity_p ~ "Patient Ethnicity",
    race_p ~ "Patient Race",
    education_p ~ "Patient Education",
    comorbid_sum_p ~ "Patient Comorbidities",
    age_c ~ "Caregiver Age",
    gender_c ~ "Caregiver Gender",
    ethnicity_c ~ "Caregiver Ethnicity",
    race_c ~ "Caregiver Race",
    education_c ~ "Caregiver Education",
    comorbid_sum_c ~ "Caregiver Comorbidities",
    location ~ "Study Site",
    marital ~ "Marital Status",
    stage ~ "Tumor Stage",
    dx ~ "Tumor Site",
    income ~ "Household Income",
    children ~ "Presence of Children Under 18 Years Old"
)

## Logistic Regression----------------------------------------------------------
### Build empty list
lr_list <- list()
outcome_list <- list("p_fintox", "c_fintox", "con")

### loop over timepoints
for (i in 1:3) {
    outcome_exclude <- case_when(
        i == 1 ~ c(outcome_list[[2]], outcome_list[[3]]),
        i == 2 ~ c(outcome_list[[1]], outcome_list[[3]]),
        i == 3 ~ c(outcome_list[[1]], outcome_list[[2]])
    )
    lr_list[[i]] <- pred_outcomes %>%
        select(-c(partid)) %>%
        filter(is.na(age_p) == FALSE) %>%
        tbl_uvregression(
            y = outcome_list[[i]],
            label = lr_labels,
            method = glm,
            include = -outcome_exclude,
            method.args = list(family = binomial),
            exponentiate = TRUE,
            estimate_fun = purrr::partial(bounded_style_ratio, min = 0.001, max = 10)
        ) %>%
        modify_footnote(c(estimate, ci) ~ "ORs <0.001 or larger than 10 shown as '<0.001' and '>10.00'")
}

### Merge tables

biv_logreg <- tbl_merge(
    tbls = lr_list,
    tab_spanner = c(
        "Patient Financial Toxicity",
        "Caregiver Financial Toxicity",
        "Concordant Financial Toxicity"
    )
) %>%
    as_flex_table()
# Data Out _____________________________________________________________________


## Logistic Regression-----------------------------------------------------

save_as_docx(
    biv_logreg,
    path = "OutData/COSTBivariateLogisticRegression.docx"
)
