################################################################################

## Author: Zach Rivers, @riverspharmd, zrivers@fredhutch.org
## Contributors:

## Purpose: Create a table 1 of predictor variables

## Depends on:
## 06PredictorJoin.r
## tidyverse
## table1
## naniar


## Input:
## IntData/pred_wide.csv

## Output:
## IntData/missing_partid.csv

## Comments: This file is maybe something that shouldn't be run independently,
## it shows how I did EDA on the predictor variables and handled missingness.
## With every predictor variable, I came up with a programatic way to identify
## the PARTID that needed an update for that variable or was missing. The
## updates were handled in-line, and the ones that were being removed were
## collected under varname_missing_partid. Each variable's missing list was
## joined into missing_partid, which was then exported as a single column .csv
## file. This .csv then allows future analysis to drop all the partids that we
## identify in this exploration.

################################################################################

## Packages
library(table1)
library(tidyverse)
library(naniar)

## Data In
dat <- read_csv(file = "IntData/pred_wide.csv")
dat_long <- read_csv(file = "IntData/pred.csv")

## Set up Data structure

### Factors

#### Location
dat$location <- factor(dat$location,
  levels = c(1, 2),
  labels = c("Duke", "SCCA")
)

#### Gender

dat$gender <- factor(dat$gender,
  levels = c(0, 1, 2),
  labels = c(
    "Male",
    "Female", "Transgender"
  )
)

#### Race

dat$race <- factor(dat$race,
  levels = c(1:6),
  labels = c(
    "AIAN",
    "Asian",
    "NHPI",
    "Black",
    "White",
    "Multi"
  )
)
#### Ethnicity

dat$ethnicity <- factor(dat$ethnicity,
  levels = c(0, 1),
  labels = c(
    "Non-Hispanic",
    "Hispanic"
  )
)

#### Education

dat$education <- factor(dat$education,
  levels = c(1:5),
  labels = c(
    "LTHS",
    "HS",
    "LTBA",
    "BA",
    "PostBA"
  )
)

#### Marital
dat$marital <- factor(dat$marital,
  levels = c(1, 2),
  labels = c("Married", "Partnered")
)

#### Income

dat$income <- factor(dat$income,
  levels = c(1:7),
  labels = c(
    "lt20k",
    "20-39k",
    "40-59k",
    "60-79k",
    "80-99k",
    "100-120k",
    "gt120k"
  )
)

#### Children

dat$children <- factor(dat$children,
  levels = c(0, 1),
  labels = c("No", "Yes")
)

#### Stage

dat$stage <- factor(dat$stage,
  levels = 0:9,
  labels = c("2", "2A", "2B", "3", "3A", "3B", "3C", "4", "4A", "4B")
)

#### Source

dat$source <- factor(dat$source,
  levels = 0:1,
  labels = c("Patient", "Caregiver")
)

## Cast Table

table1(~ age + gender + race + ethnicity + education + marital + income +
  children + comorbid_sum + DX + stage | location * source, data = dat)

## There is a lot of missingness here

## Visualize missingness

### Vis_Miss()

naniar::vis_miss(dat)

### gg_miss_upset()

naniar::gg_miss_upset(dat, nsets = n_var_miss(dat))

### Missingness trend, where patients are missing demographics, n=14

## Drop Missingness

### Find missingness pattern
dat_missing_demo <- dat %>%
  filter(is.na(gender) & is.na(race) & is.na(education) & is.na(marital))

### Pull out partID
dat_missing_partid <- dat_missing_demo$partid

dat <- dat %>%
  filter(!partid %in% dat_missing_partid)
## Visualize again

### gg_miss_upset()

naniar::gg_miss_upset(dat, nsets = n_var_miss(dat))

## Look at Income

### Filter out missing demos
dat_long <- dat_long %>%
  filter(!partid %in% dat_missing_partid)

### Slice data
income <- dat_long %>%
  filter(observation == "income")

### Build discordance measure

income <- income %>%
  mutate(inc_disc = case_when(
    value_patient == value_caregiver ~ 0,
    value_patient < value_caregiver ~ -1,
    value_patient > value_caregiver ~ 1
  ))

### Summarize discordance measure

income_sum <- income %>%
  group_by(inc_disc) %>%
  count()

### Rebuild income measure

income <- income %>%
  mutate(value_patient = if_else(is.na(value_patient) == T, value_caregiver,
if_else(value_patient >= value_caregiver, value_patient,
if_else(value_patient < value_caregiver, value_caregiver, value_patient)))) %>%
mutate(value_caregiver = if_else(is.na(value_caregiver) == T, value_patient,
if_else(value_caregiver >= value_patient, value_caregiver,
if_else(value_caregiver < value_patient, value_patient, value_caregiver))))


### (re)Build discordance measure

income <- income %>%
  mutate(inc_disc = case_when(
    value_patient == value_caregiver ~ 0,
    value_patient < value_caregiver ~ -1,
    value_patient > value_caregiver ~ 1
  ))

### (re)Summarize discordance measure

income_sum <- income %>%
  group_by(inc_disc) %>%
  count()
### extract income missing partids
income_missing <- income %>%
filter(is.na(inc_disc))

income_missing_partid <- income_missing$partid

## Children
### This is the other variable where the partners should have answered the same,
### as it is # of kids in house. Will assign 1 if either partner has 1. Should
### do sensitivity analysis to check this assumption

### Slice data

children <- dat_long %>%
filter(observation == "children")

### Build Discordance

children <- children %>%
  mutate(child_disc = case_when(
    value_patient == value_caregiver ~ 0,
    value_patient < value_caregiver ~ -1,
    value_patient > value_caregiver ~ 1
  ))

### Summarize discordance
