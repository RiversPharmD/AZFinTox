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
