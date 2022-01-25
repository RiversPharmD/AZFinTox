###############################################################################
## Purpose: this file loads IES survey data
## from each timepoint, cleans it, and outputs a joined dataset.

## Depends:
## O1PatientDataCleanSplit.r
## 02CaregiverDataCleanSplit.r
## tidyverse

## Inputs:
## IntData/ies_pat.csv
## IntData/ies_care.csv
## IntData/partid_missing_demo.csv

## Outputs:
## IntData/ies.csv

###############################################################################

## Packages
library(tidyverse)


## Functions


## Data In

### Patient

dat_p <- read_csv(
  file = "IntData/ies_pat.csv"
)

### Caregiver

dat_c <- read_csv(
  file = "IntData/ies_care.csv"
)

partid_missing_demo <- read_csv(
  file = "IntData/partid_missing_demo.csv"
)$value
## Data Tidy


### Score IES

#### Patient

dat_p <- dat_p %>%
  rowwise() %>% ## swaps it so I can mutate this way
  mutate(ies_sum_p = sum(c_across(ies01:ies15))) %>%
  ungroup()

#### Caregiver

dat_c <- dat_c %>%
  rowwise() %>% ## swaps it so I can mutate this way
  mutate(ies_sum_c = sum(c_across(ies01:ies15))) %>%
  ungroup()
### Merge datasets

#### Narrow to retain only sums

##### Patient

dat_narrow_p <- dat_p %>%
  select(partid, redcap_event_name, ies_sum_p)

##### Caregiver
dat_narrow_c <- dat_c %>%
  select(partid, redcap_event_name, ies_sum_c)

#### Create COST dataset

dat <- dat_narrow_p %>%
  left_join(dat_narrow_c, by = c("partid", "redcap_event_name"))

#### Drop the ones with missing predictors

dat <- dat %>%
filter(!partid %in% partid_missing_demo)

### Create concordance variable

#### Directional

dat <- dat %>%
  mutate(ies_con_dir = (ies_sum_p - ies_sum_c))


#### Absolute

dat <- dat %>%
  mutate(ies_con_abs = abs(ies_con_dir))

## Data out

write_csv(dat, file = "IntData/ies.csv")
