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

## Create list to hold timepoints
biv_list <- list()
for (i in 1:4) {
    ## Create tibble to hold data

    biv_df <- tibble(
        term = character(),
        estimate = numeric(),
        std.error = numeric(),
        statistic = numeric(),
        p.value = numeric(),
        conf.low = numeric(),
        conf.high = numeric()
    )

    ## Build Model
    log_reg_model <-
        logistic_reg() %>%
        set_engine("glm")


    ## Loop over predictors
    for (z in seq_along(pred_names)) {
        x <- pred_names[z]
        ## Fit model
        ## (https://aosmith.rbind.io/2019/06/24/function-for-model-fitting)
        log_reg_fit <-
            log_reg_model %>%
            fit(as.formula(paste("con ~", x)),
                data = cost_list[[i]]
            )
        ## Extract outcomes
        tidy_fit <- tidy(log_reg_fit,
            conf.int = TRUE,
            exponentiate = TRUE
        )
        ##
        ## Attach Outcomes
        biv_df <- biv_df %>%
            rbind(tidy_fit)
    }

    biv_df <- biv_df %>%
        filter(term != "(Intercept)") %>%
        mutate(timepoint = i)

    biv_list[[i]] <- biv_df
}

biv_df_out <- rbind(biv_list[[1]], biv_list[[2]]) %>%
    rbind(biv_list[[3]]) %>%
    rbind(biv_list[[4]])

# Tidy Output___________________________________________________________________

## Get rid of variables I don't want
tidy_biv_df <- biv_df_out %>%
    select(
        -c(statistic, std.error)
    )

## Cap stuff
## if a value is = 0.0009, it's super small, and if it's 101, its super big
tidy_biv_df <- tidy_biv_df %>%
    mutate(
        estimate = round(case_when(
            estimate < 0.001 ~ 0.001,
            estimate > 100 ~ 101,
            TRUE ~ estimate
        ), digits = 4),
        p.value = round(case_when(
            p.value < 0.001 ~ 0.001,
            p.value > 100 ~ 101,
            TRUE ~ p.value
        ), digits = 4),
        conf.low = round(case_when(
            conf.low < 0.001 ~ 0.001,
            is.na(conf.low) ~ 0.001,
            conf.low > 100 ~ 101,
            TRUE ~ conf.low
        ), digits = 4),
        conf.high = round(case_when(
            conf.high < 0.001 ~ 0.001,
            conf.high > 100 ~ 101,
            is.na(conf.high) ~ 101,
            TRUE ~ conf.high
        ), digits = 4)
    )

## Join confidence intervals

tidy_biv_df <- tidy_biv_df %>%
    mutate(confidence_interval = paste0(
        "(",
        as.character(conf.low),
        "-",
        conf.high,
        ")"
    )) %>%
    select(-c(
        conf.low,
        conf.high
    ))

## Break Term into Three categories

tidy_biv_df <- tidy_biv_df %>%
    mutate(
        observation = case_when(
            str_detect(term, "^age_") == TRUE ~ "Age",
            str_detect(term, "^children") == TRUE ~ "Children",
            str_detect(term, "^comorbid_sum") == TRUE ~ "Comorbidities",
            str_detect(term, "^dx") == TRUE ~ "Diagnosis",
            str_detect(term, "^education_") == TRUE ~ "Education",
            str_detect(term, "^ethnicity_") == TRUE ~ "Ethnicity",
            str_detect(term, "^gender_") == TRUE ~ "Gender",
            str_detect(term, "^income") == TRUE ~ "Income",
            str_detect(term, "^location") == TRUE ~ "Location",
            str_detect(term, "^marital") == TRUE ~ "Marital",
            str_detect(term, "^race_") == TRUE ~ "Race",
            str_detect(term, "^stage") == TRUE ~ "Stage",
            TRUE ~ "ERROR"
        ),
        person = case_when(
            str_detect(term, "_p") == TRUE ~ "Patient",
            str_detect(term, "_c") == TRUE ~ "Caregiver",
            TRUE ~ "Dyad"
        ),
        level = case_when(
            str_detect(term, "Yes") == TRUE ~ "Yes",
            str_detect(term, "Transgender") == TRUE ~ "Transgender",
            str_detect(term, "Colon") == TRUE ~ "Colon",
            str_detect(term, "Lung") == TRUE ~ "Lung",
            str_detect(term, "Rectum") == TRUE ~ "Rectum",
            str_detect(term, "cBA") == TRUE ~ "College Degree",
            str_detect(term, "pBA") == TRUE ~ "College Degree",
            str_detect(term, "HS") == TRUE ~ "High School",
            str_detect(term, "LTBA") == TRUE ~ "Some College",
            str_detect(term, "PostBA") == TRUE ~ "Some Graduate School",
            str_detect(term, "Hispanic") == TRUE ~ "Hispanic",
            str_detect(term, "Female") == TRUE ~ "Female",
            str_detect(term, "100-120k") == TRUE ~ "100-120k",
            str_detect(term, "20-39k") == TRUE ~ "20-39k",
            str_detect(term, "40-59k") == TRUE ~ "40-59k",
            str_detect(term, "60-79k") == TRUE ~ "60-79k",
            str_detect(term, "80-99k") == TRUE ~ "80-99k",
            str_detect(term, "gt120k") == TRUE ~ "Greater than 120k",
            str_detect(term, "SCCA") == TRUE ~ "SCCA",
            str_detect(term, "Partnered") == TRUE ~ "Marital",
            str_detect(term, "Black") == TRUE ~ "Black",
            str_detect(term, "Multi") == TRUE ~ "Multiracial",
            str_detect(term, "Asian") == TRUE ~ "Asian",
            str_detect(term, "AIAN") == TRUE ~ "American Indian or Alaskan Native",
            str_detect(term, "NHPI") == TRUE ~
            "Native Hawaiian or Pacific Islander",
            str_detect(term, "2A") == TRUE ~ "2A",
            str_detect(term, "2B") == TRUE ~ "2B",
            str_detect(term, "2C") == TRUE ~ "2C",
            str_detect(term, "3$") == TRUE ~ "3",
            str_detect(term, "3A") == TRUE ~ "3A",
            str_detect(term, "3B") == TRUE ~ "3B",
            str_detect(term, "3C") == TRUE ~ "3C",
            str_detect(term, "4$") == TRUE ~ "4",
            str_detect(term, "4A") == TRUE ~ "4A",
            str_detect(term, "4B") == TRUE ~ "4B",
            str_detect(term, "^age") == TRUE ~ "1 year",
            str_detect(term, "^comorbid") == TRUE ~ "1 point",
            TRUE ~ "error"
        )
    )
## Rename stuff

tidy_biv_df <- tidy_biv_df %>%
    select(
        "Timepoint" = timepoint,
        "Person" = person,
        "Observation" = observation,
        "Level" = level,
        "Odds Ratio" = estimate,
        "Confidence Interval" = confidence_interval,
        "P Value" = p.value
    ) %>%
    arrange(
        Timepoint,
        Person,
        Observation,
        Level
    )
# Data Out _____________________________________________________________________

write_csv(biv_df_out, file = "IntData/bivariate_COST_logistic_regression.csv")
write_csv(tidy_biv_df, file = "OutData/TidyCOSTlogisticregression.csv")
