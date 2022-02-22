################################################################################

## Author: Zach Rivers, @riverspharmd, zrivers@fredhutch.org
## Contributors:

## Purpose: This file takes the survey data output and provides exploration
## output.

## Depends on:
## 10COSTTidyJoin.r
## 11CESDTidyJoin.r
## 12IESTidyJoin.r
## 13COVIDTidyJoin.r
## 14FACTTidyJoin.r
## 15POMSTidyJoin.r
## tidyverse


## Input:
## "IntData/cost.csv"
## "IntData/cesd.csv"
## "IntData/ies.csv"
## "IntData/covid.csv"
## "IntData/fact.csv"
## "IntData/poms.csv"

## Output:

################################################################################

## Packages
library(tidyverse)

## Functions

tp_sum <- function(dat, obs, tp) {
  dat_out <- dat %>%
    filter(observation == obs) %>%
    filter(redcap_event_name == tp) %>%
    group_by(survey) %>%
    summarise("tp_{{tp}}" := sum(available))
}

obs_sum <- function(dat, obs) {
  dat_sum <- dat %>%
    filter(observation == obs) %>%
    group_by(partid, survey) %>%
    summarise(n_obs = sum(available)) %>%
    ungroup() %>%
    group_by(survey, n_obs) %>%
    count() %>%
    ungroup()

  dat_sum_all4 <- dat_sum %>%
    filter(n_obs == 4) %>%
    select(survey,
      "all4" = n
    )
  ##### All Timepoints

  ###### Baseline

  dat_sum_tp1 <- tp_sum(
    dat = dat,
    obs = obs,
    tp = 1
  )

  ###### Second obs

  dat_sum_tp2 <- tp_sum(
    dat = dat,
    obs = obs,
    tp = 2
  )

  ###### Third obs

  dat_sum_tp3 <- tp_sum(
    dat = dat,
    obs = obs,
    tp = 3
  )

  ###### Fourth Obs

  dat_sum_tp4 <- tp_sum(
    dat = dat,
    obs = obs,
    tp = 4
  )

  ##### Join

  dat_sum_tp <- dat_sum_tp1 %>%
    left_join(dat_sum_tp2) %>%
    left_join(dat_sum_tp3) %>%
    left_join(dat_sum_tp4) %>%
    left_join(dat_sum_all4) %>%
    mutate(part_type = as_factor(obs)) %>%
    relocate(survey, part_type)

  list_out <- list(dat_sum, dat_sum_tp)
  return(list_out)
}

## Data in

cost <- read_csv(
  file = "IntData/cost.csv",
  col_types = "ffffn"
)
cesd <- read_csv(
  file = "IntData/cesd.csv",
  col_types = "ffffn"
)
ies <- read_csv(
  file = "IntData/ies.csv",
  col_types = "ffffn"
)
covid <- read_csv(
  file = "IntData/covid.csv",
  col_types = "ffffn"
)
fact <- read_csv(
  file = "IntData/fact.csv",
  col_types = "ffffn"
)
poms <- read_csv(
  file = "IntData/poms.csv",
  col_types = "ffffn"
)

dat <- rbind(
  cost,
  cesd,
  ies,
  covid,
  fact,
  poms
)

## Data analysis

### Add availability column (0=No, 1 = Yes)

dat <- dat %>%
  mutate(available = if_else(is.na(score), 0, 1))

### Recode observation

dat <- dat %>%
  mutate(observation = case_when(
    observation == "sum_p" ~ "patient",
    observation == "sum_c" ~ "caregiver",
    observation == "con_dir" ~ "dyad",
    observation == "con_raw" ~ "dyad"
  ))

### Summarise Availability

#### Dyad
dyad_sum <- obs_sum(dat = dat, obs = "dyad")

#### Patient
patient_sum <- obs_sum(dat = dat, obs = "patient")

#### Caregiver
care_sum <- obs_sum(dat = dat, obs = "caregiver")

#### Join

surv_dat_avail <- rbind(dyad_sum[[2]], patient_sum[[2]], care_sum[[2]])

surv_dat_avail_long <- surv_dat_avail %>%
  pivot_longer(
    cols = tp_1:all4,
    names_to = "timepoint",
    values_to = "num"
  )

### Visualize availability

ggplot(
  data = surv_dat_avail_long,
  mapping = aes(
    x = timepoint,
    y = num,
    fill = part_type
  )
) +
  geom_col(position = position_dodge()) +
  facet_wrap(vars(survey))


### PartID extraction

#### Filters for surveys that are available
dat_avail <- dat %>%
  filter(available == 1,
         is.na(observation)==FALSE) %>%
  select( -(score:available))

#### Create df-column of partids
dat_avail_2 <- dat_avail %>%
  group_by(survey, observation, redcap_event_name) %>%
  nest() %>%
  mutate(dat_name = paste("tp", redcap_event_name,
    survey,
    observation,
    sep = "_"
  )) %>%
    filter(is.na(observation) == FALSE)

#### Names the datsets
vec_dat_name <- dat_avail_2$dat_name
df_list <- dat_avail_2$data
names(df_list) <- vec_dat_name

#### Exports the .csvs
file_name = list(NA)
for (i in seq_along(df_list)) {

    file_name[i] = paste0("IntData/SurveyPartIDs/",
                       vec_dat_name[i], ".csv")

    write_csv(x = df_list[[i]],
              file = file_name[[i]])
}
write_csv(dat_avail,file = "IntData/SurveyAvailable.csv")
