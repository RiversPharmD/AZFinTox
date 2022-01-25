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

## Align: This function takes the long dataset and a specific observation as
## inputs, checks for partner concordance, and creates concordance. It uses the
## max argument to decide if it should set partner values to the max of both. If
## max is T, then it will set them both to max. If max is F, then it sets them
## both to the minimum

align <- function(dat, x, max = TRUE) {

## Dependencies
  library(dplyr)

ifelse(max == TRUE,

  ## True
  dat <- dat %>%
  mutate(value_patient = if_else(observation == x,
    if_else(is.na(value_patient) == T, value_caregiver,
      if_else(value_patient >= value_caregiver, value_patient,
        if_else(value_patient < value_caregiver, value_caregiver, value_patient)
      )
    ), value_patient
  )) %>%
  mutate(value_caregiver = if_else(observation == x,
    if_else(is.na(value_caregiver) == T, value_patient,
      if_else(value_caregiver >= value_patient, value_caregiver,
        if_else(value_caregiver < value_patient, value_patient, value_caregiver)
      )
    ), value_caregiver
  )),
## False
dat <- dat %>%
 mutate(value_patient = if_else(observation == x,
   if_else(is.na(value_patient) == T, value_caregiver,
     if_else(value_patient <= value_caregiver, value_patient,
       if_else(value_patient > value_caregiver, value_caregiver, value_patient)
     )
   ), value_patient
 )) %>%
 mutate(value_caregiver = if_else(observation == x,
   if_else(is.na(value_caregiver) == T, value_patient,
     if_else(value_caregiver <= value_patient, value_caregiver,
       if_else(value_caregiver > value_patient, value_patient, value_caregiver)
     )
   ), value_caregiver
 )))

 ## Return
 return(dat)
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
