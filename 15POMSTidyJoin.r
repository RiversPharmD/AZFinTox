###############################################################################
## Purpose: this file loads POMS survey data
## from each timepoint, cleans it, and outputs a joined dataset.

## Depends:
## O1PatientDataCleanSplit.r
## 02CaregiverDataCleanSplit.r
## tidyverse

## Inputs:
## IntData/poms_pat.csv
## IntData/poms_care.csv
## IntData/partid_missing_demo.csv

## Outputs:
## IntData/poms.csv

###############################################################################

## Packages
library(tidyverse)
source(file = "00Functions.r"
)

## Functions


## Data In

### Patient

dat_p <- read_csv(
  file = "IntData/poms_pat.csv"
)

### Caregiver

dat_c <- read_csv(
  file = "IntData/poms_care.csv"
)

partid_missing_demo <- read_csv(
  file = "IntData/partid_missing_demo.csv"
)$value
## Data Tidy

### Recode

#### Patient


##### Per rubric, 1, 5, 7, 9, 11, 17, 24, 29, 32, and 35 are reverse scored

dat_p <- dat_p %>%
  mutate(across(
    matches(c("01", "05", "07", "09", "11", "17", "24", "29", "32", "35")),
    ~ case_when(
      . == 1 ~ 5,
      . == 2 ~ 4,
      . == 3 ~ 3,
      . == 4 ~ 2,
      . == 5 ~ 1
    )
  ))
#### Caregiver


##### Per rubric, 1, 5, 7, 9, 11, 17, 24, 29, 32, and 35 are reverse scored

dat_c <- dat_c %>%
mutate(across(
  matches(c("01", "05", "07", "09", "11", "17", "24", "29", "32", "35")),
  ~ case_when(
    . == 1 ~ 5,
    . == 2 ~ 4,
    . == 3 ~ 3,
    . == 4 ~ 2,
    . == 5 ~ 1
  )
))


### Score COST

#### Patient

dat_p <- dat_p %>%
  rowwise() %>% ## swaps it so I can mutate this way
  mutate(sum_p = sum(c_across(poms01:poms35))) %>%
  ungroup()

#### Caregiver

dat_c <- dat_c %>%
  rowwise() %>% ## swaps it so I can mutate this way
  mutate(sum_c = sum(c_across(poms01:poms35))) %>%
  ungroup()
### Merge datasets

#### Narrow to retain only sums

##### Patient

dat_narrow_p <- dat_p %>%
  select(partid, redcap_event_name, sum_p)

##### Caregiver
dat_narrow_c <- dat_c %>%
  select(partid, redcap_event_name, sum_c)

#### Create COST dataset

dat <- dat_narrow_p %>%
  left_join(dat_narrow_c, by = c("partid", "redcap_event_name"))

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
  mutate(survey = "POMS") %>%
  select(partid, redcap_event_name,survey,observation,score)

## Data out

write_csv(dat, file = "IntData/poms.csv")
