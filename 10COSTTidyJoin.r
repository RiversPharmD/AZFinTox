###############################################################################
## Purpose: this file loads COST survey data
## from each timepoint, cleans it, and outputs a joined dataset.

## Depends:
## O1PatientDataCleanSplit.r
## 02CaregiverDataCleanSplit.r
## tidyverse

## Inputs:
## IntData/facitcost_pat.csv
## IntData/facitcost_care.csv
## IntData/partid_missing_demo.csv

## Outputs:
## IntData/cost.csv

###############################################################################

## Packages
library(tidyverse)
source(file = "00Functions.r"
)

## Functions


## Data In

### Patient

dat_p <- read_csv(
  file = "IntData/facitcost_pat.csv"
)

### Caregiver

dat_c <- read_csv(
  file = "IntData/facitcost_care.csv"
)

partid_missing_demo <- read_csv(
  file = "IntData/partid_missing_demo.csv"
)$value
## Data Tidy

### Recode

#### Patient

##### Subtracted 1 from all scores to match actual FACIT-COST

dat_p <- dat_p %>%
  mutate(across(matches("facit_cost"), shift_left))

##### Per rubric, 2,3,4,5,8,9, and 10 are reverse scored

dat_p <- dat_p %>%
  mutate(across(
    matches(c("02", "03", "04", "05", "08", "09", "10")),
    ~ case_when(
      . == 0 ~ 4,
      . == 1 ~ 3,
      . == 2 ~ 2,
      . == 3 ~ 1,
      . == 4 ~ 0
    )
  ))
#### Caregiver

##### Subtracted 1 from all scores to match actual FACIT-COST

dat_c <- dat_c %>%
  mutate(across(matches("facit_cost"), shift_left))

##### Per rubric, 2,3,4,5,8,9, and 10 are reverse scored

dat_c <- dat_c %>%
  mutate(across(
    matches(c("02", "03", "04", "05", "08", "09", "10")),
    ~ case_when(
      . == 0 ~ 4,
      . == 1 ~ 3,
      . == 2 ~ 2,
      . == 3 ~ 1,
      . == 4 ~ 0
    )
  ))


### Score COST

#### Patient

dat_p <- dat_p %>%
  rowwise() %>% ## swaps it so I can mutate this way
  mutate(sum_p = sum(c_across(facit_cost01:facit_cost11))) %>%
  ungroup()

#### Caregiver

dat_c <- dat_c %>%
  rowwise() %>% ## swaps it so I can mutate this way
  mutate(sum_c = sum(c_across(facit_cost01:facit_cost11))) %>%
  ungroup()
### Merge datasets



#### Narrow datasets

##### Patient

dat_narrow_p <- dat_p %>%
  select(partid, redcap_event_name, sum_p)

##### Caregiver
dat_narrow_c <- dat_c %>%
  select(partid, redcap_event_name, sum_c)

#### Full datasets

##### Patient
dat_p <- dat_p %>%
  select(partid:facit_cost11, "sum" = sum_p) %>%
  mutate(src = "patient")

##### Caregiver
dat_c <- dat_c %>%
    select(partid:facit_cost11, "sum" = sum_c) %>%
    mutate(src = "caregiver")


#### Create COST dataset

##### Narrow
dat <- dat_narrow_p %>%
  left_join(dat_narrow_c, by = c("partid", "redcap_event_name"))

##### Full

dat_full <- rbind(dat_p, dat_c)
#### Drop the ones with missing predictors

dat <- dat %>%
filter(!partid %in% partid_missing_demo)

### Create concordance variable

#### Directional

dat <- dat %>%
  mutate(con_dir = (sum_p - sum_c))



### Lengthen and add survey type

dat <- dat %>%
pivot_longer(
    cols = !partid:redcap_event_name,
    names_to = "observation",
    values_to = "score"
  ) %>%
  mutate(survey = "COST") %>%
  select(partid, redcap_event_name,survey,observation,score)

## Data out

write_csv(dat, file = "IntData/cost.csv")
write_csv(dat_full, file = "IntData/cost_full.csv")
