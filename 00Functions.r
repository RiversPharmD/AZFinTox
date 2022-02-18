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

## Shift Leftare scored on a different scale, this shifts them to the
## original scale

shift_left <- function(x) {
    library(dplyr)
    y <- dplyr::if_else(is.na(x) == FALSE, (x - 1), x)
    return(y)
}

##
timepoint <- function(dat, num) {
    dat_new <- dat %>%
        dplyr::filter(redcap_event_name == num)
    return(dat_new)
}

lengthen <- function(dat, x) {
    ## make column name
    value <- stringr::str_c("value_", x)
    ## pivots longer
    dat_new <- dat %>%
        tidyr::pivot_longer(
            cols = !partid:redcap_event_name,
            names_to = "observation",
            values_to = value
        )
    ## Drops variables
    dat_new2 <- dat_new %>%
        dplyr::select(partid, observation, value)
    return(dat_new2)
}

## label_factors: This function takes the wide dataset, and recasts variables as
## factors.

label_factors <- function(dat) {

    #### Location
    dat$location <- factor(dat$location,
        levels = c(0, 1),
        labels = c("Duke", "SCCA")
    )

    #### Gender

    dat$gender <- factor(dat$gender,
        levels = c(0, 1, 2),
        labels = c(
            "Male",
            "Female", "Transgender"
        )
    )

    #### Race

    dat$race <- relevel(factor(dat$race,
        levels = c(0:5),
        labels = c(
            "American Indian or Alaskan Native",
            "Asian",
            "Native Hawaiian or Pacific Islander",
            "Black",
            "White",
            "Multiracial"
        )
    ), ref = "White")
    #### Ethnicity

    dat$ethnicity <- factor(dat$ethnicity,
        levels = c(0, 1),
        labels = c(
            "Non-Hispanic",
            "Hispanic"
        )
    )

    #### Education

    dat$education <- factor(dat$education,
        levels = c(0:4),
        labels = c(
            "Less than High School",
            "High School",
            "Less than 4 Year Degree",
            "4 Year Degree",
            "Education beyond 4 Year Degree"
        )
    )

    #### Marital
    dat$marital <- factor(dat$marital,
        levels = c(0, 1),
        labels = c("Married", "Partnered")
    )

    #### Income

    dat$income <- factor(dat$income,
        levels = c(0:6),
        labels = c(
            "Less than 20k",
            "20-39k",
            "40-59k",
            "60-79k",
            "80-99k",
            "100-120k",
            "Greater than 120k"
        )
    )

    #### Children

    dat$children <- factor(dat$children,
        levels = c(0, 1),
        labels = c("No", "Yes")
    )

    #### Stage

    dat$stage <- factor(dat$stage,
        levels = 0:2,
        labels = c("2", "3", "4")
    )

    #### dx

    dat$dx <- factor(dat$dx,
        levels = 0:3,
        labels = c("Breast", "Colon", "Lung", "Rectum")
    )
    #### Source

    dat$source <- factor(dat$source,
        levels = 0:1,
        labels = c("Patient", "Caregiver")
    )

    #### Age
    dat$age <- dat$age

    return(dat)
}


## discordance: this function takes the long dataframe, selects a specific
## observation type, and builds in a relative discordance check. It returns
## a list with two dataframes, one with the discordance for each row, and a
## summary of each observation.

discordance <- function(dat, x) {

    ## Load Packages
    library(dplyr)


    ## Creates small dataset
    dat <- dat %>%
        dplyr::filter(observation == x)

    ## Builds Discordance measure
    dat <- dat %>%
        mutate(disc = case_when(
            value_patient == value_caregiver ~ 0,
            value_patient < value_caregiver ~ -1,
            value_patient > value_caregiver ~ 1
        ))
    ## Summarize Discordance measure
    dat_sum <- dat %>%
        group_by(disc) %>%
        count()

    ## Prepare return list
    dat_list <- list(dat, dat_sum)

    ## Return list
    return(dat_list)
}

## align_missing: This function takes the long dataframe of dyad values, pulls
## out a given observation, and replaces missingness in one partner with the
## value of the other. THIS SETS BOTH TO MISSING IF BOTH ARE MISSING. It then
## returns the observation to the long dataset.

align_missing <- function(dat, x) {

    ## Dependencies
    library(dplyr)

    ## slices data

    dat_old <- dat %>%
        filter(observation != x)

    dat_update <- dat %>%
        filter(observation == x)

    ## fill in missingness

    dat_update <- dat_update %>%
        mutate(
            value_patient = if_else(is.na(value_patient) == TRUE,
                value_caregiver,
                value_patient
            ),
            value_caregiver = if_else(is.na(value_caregiver) == TRUE,
                value_patient,
                value_caregiver
            )
        )

    ## rejoins slices

    dat_new <- dat_old %>%
        rbind(dat_update)

    ## Check for correct rows
    ifelse(nrow(dat) != nrow(dat_new),
        print("The length of the new data doesn't match old data "),
        ## Return
        return(dat_new)
    )
}
## align_discordant: This function takes a long dataset, an observation of
## interest, and a logical imput. It "checks out" all of the observations of
## interest, and sets both the patient and caregiver values to the maximum of
## the two values if the logical "max" is true, or the minimum if "max" is
## false.

align_discordant <- function(dat, x, max = TRUE) {

    ## Dependencies
    library(dplyr)

    ## Slices data

    dat_old <- dat %>%
        filter(observation != x)

    dat_update <- dat %>%
        filter(observation == x)

    ## Adds placeholder column

    dat_update <- dat_update %>%
        mutate(placeholder = NA)

    ## Checks logical
    if (max == TRUE) {
        ## TRUE
        dat_update <- dat_update %>%
            rowwise() %>%
            mutate(placeholder = max(c_across(value_patient:value_caregiver)))
    } else {
        ## FALSE
        dat_update <- dat_update %>%
            rowwise() %>%
            mutate(placeholder = min(c_across(value_patient:value_caregiver)))
    }

    ## set value_patient and value_caregiver to placeholder

    dat_update <- dat_update %>%
        mutate(
            value_patient = placeholder,
            value_caregiver = placeholder
        )

    ## drop placeholder

    dat_update <- dat_update %>%
        select(-placeholder)

    ## Insert back
    dat_new <- dat_old %>%
        rbind(dat_update)

    ## Check for correct rows
    ifelse(nrow(dat) != nrow(dat_new),
        print("The length of the new data doesn't match old data "),
        ## Return
        return(dat_new)
    )
}
## extract_missing: This function pulls the partids the correspond with missing
## observations in a given column. It takes a long dataframe, scans it for NAs
## in the column named X, and returns a vector of partids.

extract_missing <- function(dat, x) {

    ## Dependencies
    library(dplyr)

    ## quosure magic that i don't really understand
    ## (https://rpubs.com/hadley/dplyr-programming)
    x <- enquo(x)

    ## filters based on X
    dat <- dat %>%
        dplyr::filter(is.na(!!x))

    ## Selects vector of partids
    vec <- dat$partid

    ## returns vector
    return(vec)
}

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
