################################################################################

## Author: Zach Rivers, @riverspharmd, zrivers@fredhutch.org
## Contributors:

## Purpose: Calculate how many observations are available for COST at each
## timepoint based on responses and predictor variable availability

## Depends on:
## tidyverse
## 16SurveyExploration.r
## 08PredictorMissingness.r


## Input:
## Output:

## Note: Syntax for rowwise() from "https://dcl-prog.stanford.edu/list-
## columns.html#nest"
################################################################################

# Packages______________________________________________________________________
library(tidyverse)
library(purrr)
source("00Functions.r")
# Functions_____________________________________________________________________

# Data In_______________________________________________________________________
pred <- read_csv(file = "IntData/dat_wide.csv")
survey <- read_csv(file = "IntData/SurveyAvailable.csv") %>%
    dplyr::filter(survey == "COST") %>%
    select(-survey)
# Analysis______________________________________________________________________
## Select partids from predictors-----------------------------------------------
pred_partid <- pred %>%
    distinct(partid) %>%
    mutate(available = 1)

## Nest by timepoint and observation--------------------------------------------


survey_nest <- survey %>%
    group_by(redcap_event_name, observation) %>%
    nest() %>%
    ungroup()

## Summarise availability-------------------------------------------------------
survey_summary <- survey_nest %>%
    rowwise() %>%
    mutate(available = sum(data$partid %in% pred_partid$partid)) %>%
    ungroup() %>%
    select(-data)


# Data out______________________________________________________________________
