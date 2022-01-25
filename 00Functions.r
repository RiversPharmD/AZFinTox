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
################################################################################

## Shift Left
## a number of surveys are scored on a different scale, this shifts them to the
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
  library(dplyr)
  library(tidyr)

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

return(dat_list)

}
