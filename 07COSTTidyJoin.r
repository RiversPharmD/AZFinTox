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

cost_p <- read_csv(
  file = "IntData/facitcost_pat.csv"
)

### Caregiver

cost_c <- read_csv(
  file = "IntData/facitcost_care.csv"
)
## Data Tidy

### Recode

#### Patient

##### Subtracted 1 from all scores to match actual FACIT-COST

cost_p <- cost_p %>%
  mutate(across(matches("facit_cost"), shift_left))

##### Per rubric, 2,3,4,5,8,9, and 10 are reverse scored

cost_p <- cost_p %>%
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

cost_c <- cost_c %>%
  mutate(across(matches("facit_cost"), shift_left))

##### Per rubric, 2,3,4,5,8,9, and 10 are reverse scored

cost_c <- cost_c %>%
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

cost_p <- cost_p %>%
  rowwise() %>% ## swaps it so I can mutate this way
  mutate(facit_cost_sum_p = sum(c_across(facit_cost01:facit_cost11))) %>%
  ungroup()

#### Caregiver

cost_c <- cost_c %>%
  rowwise() %>% ## swaps it so I can mutate this way
  mutate(facit_cost_sum_c = sum(c_across(facit_cost01:facit_cost11))) %>%
  ungroup()
### Merge datasets

#### Narrow to retain only sums

##### Patient

cost_narrow_p <- cost_p %>%
  select(partid, redcap_event_name, facit_cost_sum_p)

##### Caregiver
cost_narrow_c <- cost_c %>%
  select(partid, redcap_event_name, facit_cost_sum_c)

#### Create COST dataset

cost <- cost_narrow_p %>%
  left_join(cost_narrow_c, by = c("partid", "redcap_event_name"))

### Create concordance variable

#### Directional

cost <- cost %>%
  mutate(facit_cost_con_dir = (facit_cost_sum_p - facit_cost_sum_c))


#### Absolute

cost <- cost %>%
  mutate(facit_cost_con_abs = abs(facit_cost_con_dir))

## Data out

write_csv(cost, file = "IntData/cost.csv")
