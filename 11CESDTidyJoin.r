###############################################################################
## Purpose: this file loads CES-D 10 survey data
## from each timepoint, cleans it, and outputs a joined dataset.

## Depends:
## O1PatientDataCleanSplit.r
## 02CaregiverDataCleanSplit.r
## tidyverse

## Inputs:
## IntData/cesd_pat.csv
## IntData/cesd_care.csv
## IntData/partid_missing_demo.csv

## Outputs:
## IntData/cesd.csv

###############################################################################

## Packages
library(tidyverse)


## Functions


## Data In

### Patient

dat_p <- read_csv(
  file = "IntData/cesd_pat.csv"
)

### Caregiver

dat_c <- read_csv(
  file = "IntData/cesd_care.csv"
)

partid_missing_demo <- read_csv(
  file = "IntData/partid_missing_demo.csv"
)$value
## Data Tidy

### Recode

#### Patient


##### Per rubric, 5 and 8 are reverse scored

dat_p <- dat_p %>%
  mutate(across(
    matches(c("05", "08")),
    ~ case_when(
      . == 0 ~ 3,
      . == 1 ~ 2,
      . == 2 ~ 1,
      . == 3 ~ 0
    )
  ))
#### Caregiver


##### Per rubric 5 and 8 are reverse scored

dat_c <- dat_c %>%
  mutate(across(
    matches(c("05", "08")),
    ~ case_when(
      . == 0 ~ 3,
      . == 1 ~ 2,
      . == 2 ~ 1,
      . == 3 ~ 0
    )
  ))


### Score COST

#### Patient

dat_p <- dat_p %>%
  rowwise() %>% ## swaps it so I can mutate this way
  mutate(cesd_sum_p = sum(c_across(cesd01:cesd10))) %>%
  ungroup()

#### Caregiver

dat_c <- dat_c %>%
  rowwise() %>% ## swaps it so I can mutate this way
  mutate(cesd_sum_c = sum(c_across(cesd01:cesd10))) %>%
  ungroup()
### Merge datasets

#### Narrow to retain only sums

##### Patient

dat_narrow_p <- dat_p %>%
  select(partid, redcap_event_name, cesd_sum_p)

##### Caregiver
dat_narrow_c <- dat_c %>%
  select(partid, redcap_event_name, cesd_sum_c)

#### Create COST dataset

dat <- dat_narrow_p %>%
  left_join(dat_narrow_c, by = c("partid", "redcap_event_name"))

#### Drop the ones with missing predictors

dat <- dat %>%
filter(!partid %in% partid_missing_demo)

### Create concordance variable

#### Directional

dat <- dat %>%
  mutate(cesd_con_dir = (cesd_sum_p - cesd_sum_c))


#### Absolute

dat <- dat %>%
  mutate(cesd_con_abs = abs(cesd_con_dir))

## Data out

write_csv(dat, file = "IntData/cesd.csv")
