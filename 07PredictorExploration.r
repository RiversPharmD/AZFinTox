################################################################################

## Author: Zach Rivers, @riverspharmd, zrivers@fredhutch.org
## Contributors:

## Purpose: Create a table 1 of predictor variables

## Depends on:
## 06PredictorJoin.r
## 00Functions.r
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
source("00Functions.r")

## Data In
dat <- read_csv(file = "IntData/pred_wide.csv")
dat_long <- read_csv(file = "IntData/pred.csv",
                     col_types = "cccc")

## Set up Data structure

### Factors

dat <- label_factors(dat)

## Cast Table

table1(~ age + gender + race + ethnicity + education + marital + income +
  children + comorbid_sum + dx + stage | location * source, data = dat)

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
vec_missing_partid <- dat_missing_demo$partid

dat <- dat %>%
  filter(!partid %in% vec_missing_partid)
## Visualize again

### gg_miss_upset()

naniar::gg_miss_upset(dat, nsets = n_var_miss(dat))

## Look at Income

### Filter out missing demos
dat_long <- dat_long %>%
  filter(!partid %in% vec_missing_partid)

### Calculate income Discordance

income_list <- discordance(
  dat = dat_long,
  x = "income"
)

### Align missingness

dat_long <- align_missing(
  dat = dat_long,
  x = "income"
)

### Align discordance

dat_long <- align_discordant(
  dat = dat_long,
  x = "income"
)


### Recalculate discordance measure (HAVE INCOME)

income_list <- discordance(
  dat = dat_long,
  x = "income"
)

### extract income missing partids
income_missing <- extract_missing(
  dat = income_list[[1]],
  x = disc
)

## Children
### This is the other variable where the partners should have answered the same,
### as it is # of kids in house. Will assign 1 if either partner has 1. Should
### do sensitivity analysis to check this assumption

### Filter out missing demographics

vec_missing_partid <- c(income_missing, vec_missing_partid)

dat_long <- dat_long %>%
  filter(!partid %in% vec_missing_partid)

### Calculate Discordance (HAVE INCOME)

children_list <- discordance(
  dat = dat_long,
  x = "children"
)

### Align missing

dat_long <- align_missing(
  dat = dat_long,
  x = "children"
)

dat_long <- align_discordant(
  dat = dat_long,
  x = "children"
)

### Recalculate Discordance (HAVE INCOME)

children_list <- discordance(
  dat = dat_long,
  x = "children"
)

### Extract Missing partids
children_missing <- extract_missing(
  dat = children_list[[1]],
  x = disc
)

## Re-widen data to check for missingness again

### Drop missingness

vec_missing_partid <- c(children_missing, vec_missing_partid)

dat_long <- dat_long %>%
  filter(!partid %in% vec_missing_partid)

### Split Data Again (HAVE INCOME)
pat <- dat_long %>%
  select(-value_caregiver)

care <- dat_long %>%
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
rbind(care_wide)


## Look at missingness

### gg_miss_upset()

naniar::gg_miss_upset(dat_wide, nsets = n_var_miss(dat_wide))

## Marital Status

## There are 4 dyads with an individual missing marital status

### Calculate discordance

marital_list <- discordance(dat = dat_long,
x = "marital")

### align missingness

dat_long <- align_missing(dat = dat_long,
x = "marital")

### Align discordance

dat_long <- align_discordant(dat = dat_long, x = "marital")

### Recalculate discordance

marital_list <- discordance(dat = dat_long,
x = "marital")

### Split Data Again
pat <- dat_long %>%
  select(-value_caregiver)

care <- dat_long %>%
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
rbind(care_wide)

## Look at missingness

### gg_miss_upset()

naniar::gg_miss_upset(dat_wide, nsets = n_var_miss(dat_wide))

## Create final drop list

### find all variables with a NA left

dat_wide <- dat_wide %>%
mutate(count_na = rowSums(is.na(.)))

### Select the rows where count_na is larger than 0
dat_missing <- dat_wide %>%
filter(count_na > 0)

### Pull unique partids

vec_other_missing <- unique(dat_missing$partid)

### Join with other drop list

vec_missing_partid <- unique(c(vec_missing_partid, vec_other_missing))

### apply drop list to long and wide datasets

dat_long <- dat_long %>%
filter(!partid %in% vec_missing_partid)

dat_wide <- dat_wide %>%
filter(!partid %in% vec_missing_partid) %>%
    select(-count_na)

## Data out

write_csv(dat_long, "IntData/dat_long.csv")
write_csv(dat_wide, "IntData/dat_wide.csv")
write_csv(as_tibble(vec_missing_partid), "IntData/partid_missing_demo.csv")
