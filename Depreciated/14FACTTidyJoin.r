###############################################################################
## Purpose: this file loads COVID survey data
## from each timepoint, cleans it, and outputs a joined dataset.

## Depends:
## O1PatientDataCleanSplit.r
## 02CaregiverDataCleanSplit.r
## tidyverse

## Inputs:
## IntData/covid_pat.csv
## IntData/covid_care.csv
## IntData/partid_missing_demo.csv

## Outputs:
## IntData/covid.csv

###############################################################################

## Packages
library(tidyverse)


## Functions


## Data In

### Patient

dat_p <- read_csv(
  file = "IntData/fact_pat.csv"
)

### Caregiver

dat_c <- read_csv(
  file = "IntData/fact_care.csv"
)

partid_missing_demo <- read_csv(
  file = "IntData/partid_missing_demo.csv"
)$value
## Data Tidy

### Per rubric, 1, 2, 3, 4, 5, 6, 12, 13, 14 and 15 are reverse scored
#### Patient
dat_p <- dat_p %>%
  rename("fact01" = fact1,
"fact02" = fact2,
"fact03" = fact3,
"fact04" = fact4,
"fact05" = fact5,
"fact06" = fact6,
"fact07" = fact7,
"fact08" = fact8,
"fact09" = fact9) %>%
  mutate(across(
    matches(c("01", "02", "03", "04", "05", "06", "12", "13", "14", "15")),
    ~ case_when(
      . == 0 ~ 4,
      . == 1 ~ 3,
      . == 2 ~ 2,
      . == 3 ~ 1,
      . == 4 ~ 0
    )
  ))

#### Caregiver
  dat_c <- dat_c %>%
  rename("fact01" = fact1,
"fact02" = fact2,
"fact03" = fact3,
"fact04" = fact4,
"fact05" = fact5,
"fact06" = fact6,
"fact07" = fact7,
"fact08" = fact8,
"fact09" = fact9) %>%
    mutate(across(
      matches(c("01", "02", "03", "04", "05", "06", "12", "13", "14", "15")),
      ~ case_when(
        . == 0 ~ 4,
        . == 1 ~ 3,
        . == 2 ~ 2,
        . == 3 ~ 1,
        . == 4 ~ 0
      )
    ))
### Score COVID

#### Patient

dat_p <- dat_p %>%
  rowwise() %>% ## swaps it so I can mutate this way
  mutate(sum_p = sum(c_across(fact01:fact15))) %>%
  ungroup()

#### Caregiver

dat_c <- dat_c %>%
  rowwise() %>% ## swaps it so I can mutate this way
  mutate(sum_c = sum(c_across(fact01:fact15))) %>%
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
    mutate(survey = "FACT") %>%
    select(partid, redcap_event_name,survey,observation,score)


## Data out

write_csv(dat, file = "IntData/fact.csv")
