dat ###############################################################################
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
library(psych)
source("00Functions.r")

## Functions



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

import_available_survey_data <- function(data, timepoint, survey, obs) {
    dat_out <- data %>%
        filter(
            partid %in% import_partid(
                timepoint = {{ timepoint }},
                survey = {{ survey }},
                obs_type = {{ obs }}
            ),
            redcap_event_name == timepoint
        ) %>%
        select(-redcap_event_name)
    return(dat_out)
}

lengthen_widen <- function(data, start_col, end_col, names = "question",
                           values = "score") {
    dat_long <- data %>%
        tidyr::pivot_longer(
            cols = {{ start_col }}:{{ end_col }},
            names_to = names,
            values_to = values
        )
    dat_wide <- dat_long %>%
        tidyr::pivot_wider(
            names_from = src,
            values_from = score
        )
    return(dat_wide)
}

summarise_survey <- function(data) {
    data_out <- data %>%
        group_by(question) %>%
        summarise(
            mean_pt = mean(patient),
            sd_pt = sd(patient),
            mean_cg = mean(caregiver),
            sd_cg = sd(caregiver)
        )
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


# Analysis

## 1: Crude correlation at each timepoint.

### Initiate Empty Lists

#### Cost data available (First Item in Loop, reused for other sections)

cost_dat_avail <- list()
#### Cost Correlation output
cost_cor_list <- list()

### Loop over timepoints

for (i in 1:4) {

    # Read in data to include only those where both dyad members are available
    dat <- import_available_survey_data(
        data = cost$cost_full,
        timepoint = i,
        survey = "COST",
        obs = "dyad"
    )

    # Put in list to reuse later
    cost_dat_avail[[i]] <- dat

    # Reshape data structure
    dat_wide <- lengthen_widen(
        data = dat, start_col = "facit_cost01",
        end_col = "sum"
    )

    # Summarise Raw Scores
    dat_sum <- summarise_survey(data = dat_wide)

    # Create Correlation table
    dat_cor <- creat_corr_table(dat = dat_wide)

    # Join raw and correlation
    dat_raw_cor <- dat_sum %>%
        left_join(dat_cor)

    cost_cor_list[[i]] <- dat_raw_cor
}

## 2. Chronbach's alpha

### Intiate empty tibble
cost_chron_tibble <- tibble(
    tp = c(NA, NA, NA, NA),
    patient = c(NA, NA, NA, NA),
    caregiver = c(NA, NA, NA, NA)
)

### Loop over timepoints
for (i in 1:4) {

    # Store timepoint
    cost_chron_tibble$tp[i] <- i

    # Patient
    ## filter data
    dat <- cost_dat_avail[[i]] %>%
        filter(src == "patient") %>%
        select(facit_cost01:facit_cost11)

    ## Calculate Chronbach Alpha
    chr_a <- alpha(dat)$total[[1]]

    ## Store Value
    cost_chron_tibble$patient[i] <- chr_a

    # Caregiver
    ## Filter Data
    dat <- cost_dat_avail[[i]] %>%
        filter(src == "caregiver") %>%
        select(facit_cost01:facit_cost11)

    ## Calculate Chronbach Alpha
    chr_a <- alpha(dat)$total[[1]]

    ## Store Value
    cost_chron_tibble$caregiver[i] <- chr_a
}
# Tidying
## 1. Raw Correlation
### Create output list
tidy_cost_cor_list <- list()
### Loop over timepoints
for (i in 1:4) {

    # Round

    dat <- cost_cor_list[[i]] %>%
        mutate(across(mean_pt:ccc_high,
            round,
            digits = 2
        ))

    # Merge Patient

    dat <- dat %>%
        mutate(patient_mean_sd = paste0(
            mean_pt,
            " (",
            sd_pt,
            ")"
        )) %>%
        select(-(mean_pt:sd_pt))

    # Merge Caregiver

    dat <- dat %>%
        mutate(caregiver_mean_sd = paste0(
            mean_cg,
            " (",
            sd_cg,
            ")"
        )) %>%
        select(-(mean_cg:sd_cg))

    # Merge Pearson

    dat <- dat %>%
        mutate(pearson_95_ci = paste0(
            pearson_cor,
            " (",
            pearson_low,
            "-",
            pearson_high, ")"
        )) %>%
        select(-(pearson_cor:pearson_high))

    # Merge CCC
    dat <- dat %>%
        mutate(ccc_95_ci = paste0(
            ccc_cor,
            " (",
            ccc_low,
            "-",
            ccc_high, ")"
        )) %>%
        select(-(ccc_cor:ccc_high))

    # Store in list
    tidy_cost_cor_list[[i]] <- dat
}
## 2. Chronbach
tidy_cost_chron_tibble <- cost_chron_tibble %>%
    mutate(across(patient:caregiver,
        round,
        digits = 2
    )) %>%
    rename(
        "Timepoint" = tp,
        "Patient" = patient,
        "Caregiver" = caregiver
    )
# Output
## Data Out

### Int Data
path <- "IntData/CorrTables/"
for (i in 1:4) {
    write_csv(
        x = cost_cor_list[[i]],
        file = paste0(path, "tp", i, "_cost_dyad.csv")
    )
}
#### 2. Chronbach's
write_csv(
    x = cost_chron_tibble,
    file = paste0(
        path,
        "cost_chronbach.csv"
    )
)


### Clean Data
path <- "OutData/CorrTables/"
#### 1. Raw Correlation
for (i in 1:4) {
    write_csv(
        x = tidy_cost_cor_list[[i]],
        file = paste0(path, "tp", i, "_cost_dyad.csv")
    )
}
#### 2. Chronbach's
write_csv(
    x = tidy_cost_chron_tibble,
    file = paste0(
        path,
        "cost_chronbach.csv"
    )
)