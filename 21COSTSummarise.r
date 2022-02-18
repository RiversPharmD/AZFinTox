################################################################################

## Author: Zach Rivers, @riverspharmd, zrivers@fredhutch.org
## Contributors:

## Purpose: Calculate summary statistics on COST scores across timepoints

## Depends on:
## 00Functions.r
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

# Data Prep_____________________________________________________________________

## Input -----------------------------------------------------------------------

# cost <- read_csv(
#     file = "IntData/cost_full.csv",
#     col_types = c("ffnnnnnnnnnnnnnnnnfff")
# )

dat <- read_csv(
    file = "IntData/cost.csv"
)

## Split files and select columns-----------------------------------------------
dat <- dat %>%
    mutate(
        src = case_when(
            str_detect(observation, "_p") == TRUE ~ "patient",
            str_detect(observation, "_c$") == TRUE ~ "caregiver",
            str_detect(observation, "con_") == TRUE ~ "dyad",
            TRUE ~ "ERROR"
        ),
        type = case_when(
            str_detect(observation, "sum") |
                str_detect(observation, "_raw") == TRUE ~ "continuous",
            str_detect(observation, "lgl") |
                str_detect(observation, "_dich") == TRUE ~ "binary",
            str_detect(observation, "cat") == TRUE ~ "categorical",
            TRUE ~ "ERROR"
        )
    ) %>%
    select(-c(observation, survey)) %>%
    pivot_wider(
        names_from = type,
        values_from = score
    )


### All observations across timepoints
tp_list <- list(
    dat_tp_1 = NA,
    dat_tp_2 = NA,
    dat_tp_3 = NA,
    dat_tp_4 = NA
)
dat_paired <- tibble()
for (i in 1:4) {
    tp_list[[i]] <- dat %>%
        import_available_survey_data(
            timepoint = i,
            survey = "COST",
            obs = "dyad"
        ) %>%
        mutate(
            redcap_event_name = i,
            n = 1
        ) %>%
        select(n, everything())
    dat_paired <- rbind(dat_paired, tp_list[[i]])
}



dat_pat <- dat_paired %>%
    filter(src == "patient")

dat_care <- dat_paired %>%
    filter(src == "caregiver")

dat_dyad <- dat_paired %>%
    filter(src == "dyad")

### All dyads with observations across all timepoints
dat_all_time_points <- dat_paired %>%
    filter(partid %in% tp_list[[4]]$partid) %>%
    filter(partid %in% tp_list[[3]]$partid) %>%
    filter(partid %in% tp_list[[2]]$partid) %>%
    filter(partid %in% tp_list[[1]]$partid)

all_tp_list <- list(
    dat_all_tp_1 = NA,
    dat_all_tp_2 = NA,
    dat_all_tp_3 = NA,
    dat_all_tp_4 = NA
)

dat_all_paired <- tibble()
for (i in 1:4) {
    all_tp_list[[i]] <- dat_all_time_points %>%
        filter(redcap_event_name == i) %>%
        select(n, everything())
    dat_all_paired <- rbind(dat_all_paired, all_tp_list[[i]])
}

dat_all_pat <- dat_all_paired %>%
    filter(src == "patient")

dat_all_care <- dat_all_paired %>%
    filter(src == "caregiver")

dat_all_dyad <- dat_all_paired %>%
    filter(src == "dyad")

### Table Labels
pc_labels <- list(
    n ~ "Number of Participants",
    continuous ~ "Raw COST Score",
    binary ~ "Financial Toxicity",
    categorical ~ "Severity of Financial Toxicity"
)

dyad_labels <- list(
    n ~ "Number of Participants",
    continuous ~ "Difference in Cost Score",
    binary ~ "Difference in Financial Toxicity",
    categorical ~ "Dfference in Severity of Financial Toxicity"
)

# Analysis______________________________________________________________________

## Summary Tables---------------------------------------------------------------

### By Observation

#### All available observations
sum_pat <- dat_pat %>%
    select(-c(partid, src)) %>%
    mutate(
        binary = factor(binary,
            levels = c(0, 1),
            labels = c("No", "Yes")
        ),
        categorical = factor(categorical,
            levels = c(0:3),
            labels = c("None", "Mild", "Moderate", "Severe")
        )
    ) %>%
    tbl_summary(
        by = redcap_event_name,
        label = pc_labels,
        statistic = n ~ "{N}"
    ) %>%
    modify_header(all_stat_cols() ~ "**{level}**")

sum_care <- dat_care %>%
    select(-c(partid, src)) %>%
    mutate(
        binary = factor(binary,
            levels = c(0, 1),
            labels = c("No", "Yes")
        ),
        categorical = factor(categorical,
            levels = c(0:3),
            labels = c("None", "Mild", "Moderate", "Severe")
        )
    ) %>%
    tbl_summary(
        by = redcap_event_name,
        label = pc_labels,
        statistic = n ~ "{N}"
    ) %>%
    modify_header(all_stat_cols() ~ "**{level}**")

sum_dyad <- dat_dyad %>%
    select(-c(partid, src)) %>%
    mutate(
        binary = factor(binary,
            levels = c(0:3),
            labels = c(
                "No Financial Toxicity",
                "Both Patient and Caregiver",
                "Caregiver, Not Patient",
                "Patient, Not Caregiver"
            )
        ),
        categorical = factor(categorical,
            levels = c(0:3),
            labels = c(
                "No Financial Toxicity",
                "Equivalent Financial Toxicity",
                "Caregiver Worse Than Patient",
                "Patient Worse Than Caregiver"
            )
        )
    ) %>%
    tbl_summary(
        by = redcap_event_name,
        label = dyad_labels,
        statistic = n ~ "{N}"
    ) %>%
    modify_header(all_stat_cols() ~ "**{level}**")

#### Only those with data across all 4 timepoints
sum_all_pat <- dat_all_pat %>%
    select(-c(partid, src)) %>%
    mutate(
        binary = factor(binary,
            levels = c(0, 1),
            labels = c("No", "Yes")
        ),
        categorical = factor(categorical,
            levels = c(0:3),
            labels = c("None", "Mild", "Moderate", "Severe")
        )
    ) %>%
    tbl_summary(
        by = redcap_event_name,
        label = pc_labels,
        statistic = n ~ "{N}"
    ) %>%
    add_p() %>%
    modify_header(all_stat_cols() ~ "**{level}**")

sum_all_care <- dat_all_care %>%
    select(-c(partid, src)) %>%
    mutate(
        binary = factor(binary,
            levels = c(0, 1),
            labels = c("No", "Yes")
        ),
        categorical = factor(categorical,
            levels = c(0:3),
            labels = c("None", "Mild", "Moderate", "Severe")
        )
    ) %>%
    tbl_summary(
        by = redcap_event_name,
        label = pc_labels,
        statistic = n ~ "{N}"
    ) %>%
    add_p() %>%
    modify_header(all_stat_cols() ~ "**{level}**")

sum_all_dyad <- dat_all_dyad %>%
    select(-c(partid, src)) %>%
    mutate(
        binary = factor(binary,
            levels = c(0:3),
            labels = c(
                "No Financial Toxicity", "Both Patient and Caregiver", "Caregiver, Not Patient",
                "Patient, Not Caregiver"
            )
        ),
        categorical = factor(categorical,
            levels = c(0:3),
            labels = c(
                "No Financial Toxicity", "Equivalent Financial Toxicity",
                "Caregiver Worse Than Patient", "Patient Worse Than Caregiver"
            )
        )
    ) %>%
    tbl_summary(
        by = redcap_event_name,
        label = dyad_labels,
        statistic = n ~ "{N}"
    ) %>%
    add_p() %>%
    modify_header(all_stat_cols() ~ "**{level}**")

### By Timepoint

#### All pairs at each timepoint
tp_sum_list <- list()
for (i in 1:4) {
    dat <- tp_list[[i]]
    dat_sum <- dat %>%
        filter(src != "dyad") %>%
        select(-c(partid, redcap_event_name)) %>%
        mutate(
            binary = factor(binary,
                levels = c(0, 1),
                labels = c("No", "Yes")
            ),
            categorical = factor(categorical,
                levels = c(0:3),
                labels = c("None", "Mild", "Moderate", "Severe")
            )
        ) %>%
        tbl_summary(
            by = src,
            label = pc_labels,
            statistic = n ~ "{N}"
        ) %>%
        add_p(test = all_continuous() ~ "t.test") %>%
        modify_header(all_stat_cols() ~ "**{level}**")
    tp_sum_list[[i]] <- dat_sum
}

#### Only pairs with data across all timepoints
all_tp_sum_list <- list()
for (i in 1:4) {
    dat <- all_tp_list[[i]]
    dat_sum <- dat %>%
        filter(src != "dyad") %>%
        select(-c(partid, redcap_event_name)) %>%
        mutate(
            binary = factor(binary,
                levels = c(0, 1),
                labels = c("No", "Yes")
            ),
            categorical = factor(categorical,
                levels = c(0:3),
                labels = c("None", "Mild", "Moderate", "Severe")
            )
        ) %>%
        tbl_summary(
            by = src,
            label = pc_labels,
            statistic = n ~ "{N}"
        ) %>%
        add_p(test = all_continuous() ~ "t.test") %>%
        modify_header(all_stat_cols() ~ "**{level}**")
    all_tp_sum_list[[i]] <- dat_sum
}

## Joined Tables ---------------------------------------------------------------
sum_src <- tbl_stack(
    tbls = list(sum_pat, sum_care, sum_dyad),
    group_header = c("Patient", "Caregiver", "Dyad")
)
sum_all_src <- tbl_stack(
    tbls = list(sum_all_pat, sum_all_care, sum_all_dyad),
    group_header = c("Patient", "Caregiver", "Dyad")
)
sum_tp <- tbl_merge(
    tbls = tp_sum_list,
    tab_spanner = c(
        "Timepoint 1",
        "Timepoint 2",
        "Timepoint 3",
        "Timepoint 4"
    )
)

sum_all_tp <- tbl_merge(
    tbls = all_tp_sum_list,
    tab_spanner = c(
        "Timepoint 1",
        "Timepoint 2",
        "Timepoint 3",
        "Timepoint 4"
    )
)

sum_src
sum_all_src
sum_tp
sum_all_tp
