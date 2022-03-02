################################################################################

## Author: Zach Rivers, @riverspharmd, zrivers@fredhutch.org
## Contributors:

## Purpose: This file holds all the functions written for this project

## Depends on:


## Input:
## Output: Functions to global r environment

## Naming conventions
## dat = data frame
## num = numeric variable
## x = any variable
## vec = vector
################################################################################






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
