###############################################################################
## Purpose: this file loads joined datasets and analyzes them.

## Depends:
## O7PredictorExploration.r
## 16SurveyExploration.r
## tidyverse
## epiR

## Inputs:
##

## Outputs:
##

###############################################################################

## Packages
library(tidyverse)
library(epiR)

## Functions

import_partid <- function(timepoint,
                          survey,
                          obs_type = "dyad") {

    ## create file name
    file_name <- paste0(
        "IntData/SurveyPartIDs/tp_",
        timepoint, "_",
        toupper(survey), "_",
        obs_type,
        ".csv"
    )
    ## read in data
    dat <- readr::read_csv(file = file_name)

    ## create vector
    vec_partid <- dat$partid

    return(vec_partid)
}

calc_correlation <- function(dat, question) {

    ## Creates empty list for data
    ques_df <- tibble(
        question = NA,
        pearson_cor = NA,
        pearson_low = NA,
        pearson_high = NA,
        ccc_cor = NA,
        ccc_low = NA,
        ccc_high = NA
    )
    ## Populates question into list
    ques_df$question <- question
    ## Pulls data out of dataframe
    dat <- dat %>%
        filter(question == {{ question }})
    ## Run Pearson
    pear_list <- cor.test(
        x = dat$patient,
        y = dat$caregiver,
        method = "pearson",
        alternative = "t"
    )
    ## Extract Pearson
    ques_df$pearson_cor <- pear_list$estimate
    ques_df$pearson_low <- pear_list$conf.int[[1]]
    ques_df$pearson_high <- pear_list$conf.int[[2]]
    ## Run CCC
    ccc_list <- epi.ccc(
        x = dat$patient,
        y = dat$caregiver
    )
    ccc_df <- ccc_list$rho.c
    ## Extract CCC
    ques_df$ccc_cor <- ccc_df$est
    ques_df$ccc_low <- ccc_df$lower
    ques_df$ccc_high <- ccc_df$upper
    ## Return
    return(ques_df)
}
creat_corr_table <- function(dat) {
    ## create empty df to hold output
    df <- tibble(
        question = NA,
        pearson_cor = NA,
        pearson_low = NA,
        pearson_high = NA,
        ccc_cor = NA,
        ccc_low = NA,
        ccc_high = NA
    )

    ## find unique question names
    vec_question_names <- unique(dat$question)
    ## Loop over unique question names
    for (i in seq_along(vec_question_names)) {
        question <- vec_question_names[i]

        correlation <- calc_correlation(
            dat = dat,
            question = question
        )

        df <- df %>%
            rbind(correlation) %>%
            filter(!is.na(ccc_cor))
    }
    ## Return
    return(df)
}
## Data In

### Predictors

#### Wide

pred_wide <- read_csv(file = "IntData/dat_wide.csv")

#### Long

pred_long <- read_csv(
    file = "IntData/dat_long.csv",
    col_types = "ffcc"
)

### Surveys

#### Cost

cost <- list(
    cost_narrow = read_csv(
        file = "IntData/cost.csv",
        col_types = "ffffn"
    ),
    cost_full = read_csv(
        file = "IntData/cost_full.csv"
    )
)


## Analysis

### 1: Crude correlation at each timepoint.

#### Timepoint 1

##### Pull in dyads with available cost data

tp1_cost_dyad <- cost[[2]] %>%
    filter(
        partid %in% import_partid(
            timepoint = 1,
            survey = "COST",
            obs_type = "dyad"
        ),
        redcap_event_name == 1
    ) %>%
    select(-redcap_event_name)
##### Lengthen

tp1_cost_dyad_long <- tp1_cost_dyad %>%
    pivot_longer(
        cols = facit_cost01:sum,
        names_to = "question",
        values_to = "score"
    )

##### Widen

tp1_cost_dyad_wide <- tp1_cost_dyad_long %>%
    pivot_wider(
        names_from = src,
        values_from = score
    )
#### Create Correlation table

tp1_cost_dyad_cor <- creat_corr_table(dat = tp1_cost_dyad_wide)
