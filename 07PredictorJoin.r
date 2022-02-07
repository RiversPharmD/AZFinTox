################################################################################

## Author: Zach Rivers, @riverspharmd, zrivers@fredhutch.org
## Contributors:

## Purpose: This file takes all of the baseline characteristics and joins them
## into a predictor dataset.

## Depends on:
## 03DemoTidyJoin.r
## 04ComorbTidyJoin.r
## 05CancerTidyJoin.r
## tidyverse


## Input:
## IntData/demo.csv
## IntData/comor.csv
## IntData/dztype.csv

## Output:
## IntData/pred.csv
## IntData/pred_wide.csv
################################################################################

## Packages

library(tidyverse)

## Data In

demo <- read_csv(file = "IntData/demo.csv")
comor <- read_csv(file = "IntData/comor.csv")
dz <- read_csv(file = "IntData/dztype.csv")


## Data Join
### Make sure we have the right number of patients
demo_partid <- demo$partid

### Join data
dat <- demo %>%
  rbind(comor) %>%
  rbind(dz) %>%
  arrange(partid)%>%
  filter(partid %in% demo_partid)



## Make Wide

### Split Data Again
pat <- dat %>%
  select(-value_caregiver)

care <- dat %>%
  select(-value_patient)

### Widen
pat_wide <- pat %>%
  pivot_wider(
    names_from = observation,
    values_from = value_patient
  ) %>%
  mutate(source = 0)

care_wide <- care %>%
  pivot_wider(
    names_from = observation,
    values_from = value_caregiver
  ) %>%
  mutate(source = 1)

### Rejoin
dat_wide <- pat_wide %>%
rbind(care_wide) %>%
filter(partid %in% demo_partid)

## Data Out

write_csv(dat, "IntData/pred.csv")
write_csv(dat_wide, "IntData/pred_wide.csv")
