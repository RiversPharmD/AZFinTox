################################################################################

## Author: Zach Rivers, @riverspharmd, zrivers@fredhutch.org
## Contributors:

## Purpose: Calculate summary statistics on COST scores across timepoints

## Depends on:
## 00Functions.r
## 08PredictorMissingness.r
## 10COSTTidyJoin.r
## tidyverse

## Input:
## Output:

################################################################################

# Packages______________________________________________________________________
library(gtsummary)
library(flextable)
library(tidyverse)
source("00Functions.r")

# Functions_____________________________________________________________________
sum_observation <- function(dat, source) {
    if (source != "Dyad") {
        dat_out <- dat %>%
            select(-c(partid, src)) %>%
            mutate(
                binary = factor(binary,
                    levels = pc_bin$levels,
                    labels = pc_bin$labels
                ),
                categorical = factor(categorical,
                    levels = pc_cat$levels,
                    labels = pc_cat$labels
                )
            ) %>%
            tbl_summary(
                by = redcap_event_name,
                label = pc_labels,
                statistic = n ~ "{N}"
            ) %>%
            modify_header(all_stat_cols() ~ "**{level}**")
    } else {
        dat_out <- dat %>%
            select(-c(partid, src)) %>%
            mutate(
                binary = factor(binary,
                    levels = dyad_bin$levels,
                    labels = dyad_bin$labels
                ),
                categorical = factor(categorical,
                    levels = dyad_bin$levels,
                    labels = dyad_bin$labels
                )
            ) %>%
            tbl_summary(
                by = redcap_event_name,
                label = dyad_labels,
                statistic = n ~ "{N}"
            ) %>%
            add_p() %>%
            modify_header(all_stat_cols() ~ "**{level}**")
    }
}

sum_timepoint <- function(dat) {
    dat_sum <- dat %>%
        filter(src != "Dyad") %>%
        select(-c(redcap_event_name)) %>%
        mutate(
            binary = factor(binary,
                levels = pc_bin$levels,
                labels = pc_bin$labels
            ),
            categorical = factor(categorical,
                levels = pc_cat$levels,
                labels = pc_cat$labels
            )
        ) %>%
        tbl_summary(
            by = src,
            include = -partid,
            label = pc_labels,
            statistic = n ~ "{N}"
        ) %>%
        add_p(
            test = all_continuous() ~ "paired.t.test",
            group = partid
        ) %>%
        modify_header(all_stat_cols() ~ "**{level}**")
}

merge_list <- function(list_in) {
    tbl_merge(
        tbls = list_in,
        tab_spanner = c(
            "Timepoint 1",
            "Timepoint 2",
            "Timepoint 3",
            "Timepoint 4"
        )
    )
}
stack_list <- function(list_in) {
    tbl_stack(
        tbls = list_in,
        group_header = c("Patient", "Caregiver", "Dyad")
    )
}
# Data Prep_____________________________________________________________________

## Input -----------------------------------------------------------------------

# cost <- read_csv(
#     file = "IntData/cost_full.csv",
#     col_types = c("ffnnnnnnnnnnnnnnnnfff")
# )

dat <- read_csv(
    file = "IntData/cost.csv"
)

pred_dat <- read_csv(
    "IntData/dat_wide.csv"
)
## Split files and select columns-----------------------------------------------
### Reformat data
dat <- dat %>%
    mutate(
        src = case_when( ## Rename Source
            str_detect(observation, "_p") == TRUE ~ "Patient",
            str_detect(observation, "_c$") == TRUE ~ "Caregiver",
            str_detect(observation, "con_") == TRUE ~ "Dyad",
            TRUE ~ "ERROR"
        ),
        type = case_when( ## Rename Outcome Type
            str_detect(observation, "sum") |
                str_detect(observation, "_raw") == TRUE ~ "continuous",
            str_detect(observation, "lgl") |
                str_detect(observation, "_dich") == TRUE ~ "binary",
            str_detect(observation, "cat") == TRUE ~ "categorical",
            TRUE ~ "ERROR"
        )
    ) %>%
    select(-c(observation, survey)) %>%
    pivot_wider( ## Make wide data for ease of analysis
        names_from = type,
        values_from = score
    )

### All observations across timepoints
#### Create output structures
tp_list <- list(
    dat_tp_1 = NA,
    dat_tp_2 = NA,
    dat_tp_3 = NA,
    dat_tp_4 = NA
)

dat_paired <- tibble()

#### Identify Dyads with data available at each timepoint
for (i in 1:4) {
    tp_list[[i]] <- dat %>%
        import_available_survey_data(
            timepoint = i,
            survey = "COST",
            obs = "Dyad"
        ) %>%
        mutate( ## add these for summarising
            redcap_event_name = i,
            n = 1
        ) %>%
        select(n, everything()) ## Move n to the front for summarising
    dat_paired <- rbind(dat_paired, tp_list[[i]])
}

### All Dyads with observations at any timepoint and baseline
#### Filter for those with baseline
dat_paired_pred <- dat_paired %>%
    filter(partid %in% pred_dat$partid)

#### Create empty output structures
pred_tp_list <- list(
    pred_tp_1 = NA,
    pred_tp_2 = NA,
    pred_tp_3 = NA,
    pred_tp_4 = NA
)

data_all_pred <- tibble()

#### Split by Timepoint
for (i in 1:4) {
    pred_tp_list[[i]] <- dat_paired_pred %>%
        filter(redcap_event_name == i) %>%
        select(n, everything())
    data_all_pred <- rbind(data_all_pred, pred_tp_list[[i]])
}

### All Dyads with observations across all timepoints
#### Select only Dyads with data across all timepoints
dat_all_time_points <- dat_paired_pred %>%
    filter(partid %in% tp_list[[4]]$partid) %>%
    filter(partid %in% tp_list[[3]]$partid) %>%
    filter(partid %in% tp_list[[2]]$partid) %>%
    filter(partid %in% tp_list[[1]]$partid)

#### Cast empty output structures
all_tp_list <- list(
    dat_all_tp_1 = NA,
    dat_all_tp_2 = NA,
    dat_all_tp_3 = NA,
    dat_all_tp_4 = NA
)

dat_all_paired <- tibble()

#### Split by Timepoint
for (i in 1:4) {
    all_tp_list[[i]] <- dat_all_time_points %>%
        filter(redcap_event_name == i) %>%
        select(n, everything())
    dat_all_paired <- rbind(dat_all_paired, all_tp_list[[i]])
}

### Split by Source
#### All Dyads With Survey Data
dat_pat <- dat_paired %>%
    filter(src == "Patient")

dat_care <- dat_paired %>%
    filter(src == "Caregiver")

dat_dyad <- dat_paired %>%
    filter(src == "Dyad")

#### All Dyads With Survey Data and baseline
dat_pred_pat <- dat_paired_pred %>%
    filter(src == "Patient")

dat_pred_care <- dat_paired_pred %>%
    filter(src == "Caregiver")

dat_pred_dyad <- dat_paired_pred %>%
    filter(src == "Dyad")

#### All Dyads with Survey Data Across all 4 timepoints
dat_all_pat <- dat_all_paired %>%
    filter(src == "Patient")

dat_all_care <- dat_all_paired %>%
    filter(src == "Caregiver")

dat_all_dyad <- dat_all_paired %>%
    filter(src == "Dyad")


# Analysis______________________________________________________________________
## Formatting-------------------------------------------------------------------
### Patient/Caregiver
#### Category Labels
pc_labels <- list(
    n ~ "Number of Participants",
    continuous ~ "Raw COST Score",
    binary ~ "Financial Toxicity",
    categorical ~ "Severity of Financial Toxicity"
)

#### Binary Levels and Labels
pc_bin <- list(
    levels = c(0, 1),
    labels = c("No", "Yes")
)

#### Categorical Level and Labels
pc_cat <- list(
    levels = c(0:3),
    labels = c("None", "Mild", "Moderate", "Severe")
)

### Dyad
#### Dyad Labels
dyad_labels <- list(
    n ~ "Number of Participants",
    continuous ~ "Difference in Cost Score",
    binary ~ "Difference in Financial Toxicity",
    categorical ~ "Dfference in Severity of Financial Toxicity"
)

#### Binary Level and Labels
dyad_bin <- list(
    levels = c(0:3),
    labels = c(
        "No Financial Toxicity",
        "Both Patient and Caregiver",
        "Caregiver, Not Patient",
        "Patient, Not Caregiver"
    )
)

#### Categorical Level and Labels
dyad_cat <- list(
    levels = c(0:3),
    labels = c(
        "No Financial Toxicity",
        "Equivalent Financial Toxicity",
        "Caregiver Worse Than Patient",
        "Patient Worse Than Caregiver"
    )
)

## Summary Tables---------------------------------------------------------------

### By Observation

#### All available observations
dat_list <- list(
    dat_pat,
    dat_care,
    dat_dyad
)

source_list <- list(
    "Patient",
    "Caregiver",
    "Dyad"
)

sum_list <- map2(
    .x = dat_list,
    .y = source_list,
    ~ sum_observation(
        dat = .x,
        source = .y
    )
)

#### All observations with baseline data
dat_list <- list(
    dat_pred_pat,
    dat_pred_care,
    dat_pred_dyad
)

sum_pred_list <- map2(
    .x = dat_list,
    .y = source_list,
    ~ sum_observation(
        dat = .x,
        source = .y
    )
)

#### Only those with data across all 4 timepoints and baseline data
dat_list <- list(
    dat_all_pat,
    dat_all_care,
    dat_all_dyad
)

sum_all_list <- map2(
    .x = dat_list,
    .y = source_list,
    ~ sum_observation(
        dat = .x,
        source = .y
    )
)

### By Timepoint

#### All pairs at each timepoint
tp_sum_list <- map(
    .x = tp_list,
    ~ sum_timepoint(dat = .x)
)

#### All pairs with baseline data by timepoint
tp_pred_sum_list <- map(
    .x = pred_tp_list,
    ~ sum_timepoint(dat = .x)
)

#### Only pairs with data across all 4 timepoints and baseline data
tp_all_sum_list <- map(
    .x = all_tp_list,
    ~ sum_timepoint(dat = .x)
)

### By cohort
cohort_sum_list <- list(
    tp_sum_list[[1]],
    tp_pred_sum_list[[1]],
    tp_all_sum_list[[1]]
)
dyad_sum_list <- list(
    sum_list[[3]],
    sum_pred_list[[3]],
    sum_all_list[[3]]
)
## Joined Tables --------------------------------------------------------
sum_src <- stack_list(list_in = sum_list)
sum_pred_src <- stack_list(list_in = sum_pred_list)
sum_all_src <- stack_list(list_in = sum_all_list)

sum_tp <- merge_list(list_in = tp_sum_list)
sum_pred_tp <- merge_list(list_in = tp_pred_sum_list)
sum_all_tp <- merge_list(list_in = tp_all_sum_list)

cohort_sum <- tbl_merge(
    tbls = cohort_sum_list,
    tab_spanner = c(
        "Cohort 1",
        "Cohort 2",
        "Cohort 3"
    )
)

cohort_dyad_sum <- tbl_merge(
    tbls = dyad_sum_list,
    tab_spanner = c(
        "Cohort 1",
        "Cohort 2",
        "Cohort 3"
    )
)
sum_src
sum_pred_src
sum_all_src

sum_tp
sum_pred_tp
sum_all_tp

cohort_dyad_sum
# Data Out______________________________________________________________________
## Cohorts
path <- "IntData/"

### All Dyads with Survey Data
write_csv(
    x = dat_paired %>% filter(redcap_event_name == 1),
    file = paste0(
        path,
        "CohortOne.csv"
    )
)

### All Dyads with Survey Data and Baseline Characteristics
write_csv(
    x = dat_paired_pred %>% filter(redcap_event_name == 1),
    file = paste0(
        path,
        "CohortTwo.csv"
    )
)
### All Dyads with survey data across 4 timepoints and Baseline characteristics
write_csv(
    x = dat_all_paired,
    file = paste0(
        path,
        "CohortThree.csv"
    )
)

## Pretty Tables

path <- "OutData/"
flextable::save_as_docx(as_flex_table(cohort_sum),path = paste0(
    path,
    "COST_desc_cohort.docx"
))

flextable::save_as_docx(as_flex_table(sum_all_tp),
                        path = paste0(path, "COST_desc_timepoint.docx"))

flextable::save_as_docx(as_flex_table(cohort_dyad_sum),
                        path = paste0(path, "COST_desc_cohortdyad.docx"))
