################################################################################

## Author: Zach Rivers, @riverspharmd, zrivers@fredhutch.org
## Contributors:

## Purpose:

## Depends on:


## Input:
## Output:

################################################################################

# Packages______________________________________________________________________
library(tidyverse)
library(gtsummary)
library(flextable)
source("Functions/longitudinal.r")

# Functions_____________________________________________________________________

# Data Prep_____________________________________________________________________
cohort <- read_csv(file = "IntData/CohortThree.csv") %>%
    select(-n)

## Prep Input Lists-------------------------------------------------------------

timepoint_list <- list(
    c(1, 2),
    c(1, 3),
    c(1, 4),
    c(2, 3),
    c(2, 4),
    c(3, 4)
)
timepoint_headers <- c(
    "Timepoint 1 -> Timepoint 2",
    "Timepoint 1 -> Timepoint 3",
    "Timepoint 1 -> Timepoint 4",
    "Timepoint 2 -> Timepoint 3",
    "Timepoint 2 -> Timepoint 4",
    "Timepoint 3 -> Timepoint 4"
)
## Format data------------------------------------------------------------------
cohort_pc <- cohort %>%
    filter(src != "dyad") %>%
    mutate(
        binary = factor(binary,
            levels = 0:1,
            labels = c("No Financial Toxicity", "Financial Toxicity")
        ),
        categorical = factor(categorical,
            levels = 0:3,
            labels = c("None", "Mild", "Moderate", "Severe")
        )
    )

cohort_dyad <- cohort %>%
    filter(src == "dyad") %>%
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
    )
# Analysis______________________________________________________________________
cont_pat_list <- map(
    .x = timepoint_list,
    ~ sum_longitudinal(
        dat_in = cohort_pc,
        src = "patient",
        tp_vec = .x,
        var = "continuous"
    )
)
cont_pat <- tbl_merge(
    tbls = cont_pat_list,
    tab_spanner = timepoint_headers
)

dich_pat_list <- map(
    .x = timepoint_list,
    ~ sum_longitudinal(
        dat_in = cohort_pc,
        src = "patient",
        tp_vec = .x,
        var = "binary"
    )
)
dich_pat <- tbl_merge(
    tbls = dich_pat_list,
    tab_spanner = timepoint_headers
)

cat_pat_list <- map(
    .x = timepoint_list,
    ~ sum_longitudinal(
        dat_in = cohort_pc,
        src = "patient",
        tp_vec = .x,
        var = "categorical"
    )
)
cat_pat <- tbl_merge(
    tbls = cat_pat_list,
    tab_spanner = timepoint_headers
)

cont_care_list <- map(
    .x = timepoint_list,
    ~ sum_longitudinal(
        dat_in = cohort_pc,
        src = "caregiver",
        tp_vec = .x,
        var = "continuous"
    )
)
cont_care <- tbl_merge(
    tbls = cont_care_list,
    tab_spanner = timepoint_headers
)

dich_care_list <- map(
    .x = timepoint_list,
    ~ sum_longitudinal(
        dat_in = cohort_pc,
        src = "caregiver",
        tp_vec = .x,
        var = "binary"
    )
)
dich_care <- tbl_merge(
    tbls = dich_care_list,
    tab_spanner = timepoint_headers
)

cat_care_list <- map(
    .x = timepoint_list,
    ~ sum_longitudinal(
        dat_in = cohort_pc,
        src = "caregiver",
        tp_vec = .x,
        var = "categorical"
    )
)
cat_care <- tbl_merge(
    tbls = cat_care_list,
    tab_spanner = timepoint_headers
)

cont_dyad_list <- map(
    .x = timepoint_list,
    ~ sum_longitudinal(
        dat_in = cohort_dyad,
        src = "dyad",
        tp_vec = .x,
        var = "continuous"
    )
)
cont_dyad <- tbl_merge(
    tbls = cont_dyad_list,
    tab_spanner = timepoint_headers
)

dich_dyad_list <- map(
    .x = timepoint_list,
    ~ sum_longitudinal(
        dat_in = cohort_dyad,
        src = "dyad",
        tp_vec = .x,
        var = "binary"
    )
)
dich_dyad <- tbl_merge(
    tbls = dich_dyad_list,
    tab_spanner = timepoint_headers
)

cat_dyad_list <- map(
    .x = timepoint_list,
    ~ sum_longitudinal(
        dat_in = cohort_dyad,
        src = "dyad",
        tp_vec = .x,
        var = "categorical"
    )
)
cat_dyad <- tbl_merge(
    tbls = cat_dyad_list,
    tab_spanner = timepoint_headers
)

pat_longitudinal <- tbl_stack(tbls = list(cont_pat, dich_pat, cat_pat))
care_longitudinal <- tbl_stack(tbls = list(cont_care, dich_care, cat_care))
dyad_longitudinal <- tbl_stack(tbls = list(cont_dyad, dich_dyad, cat_dyad))

longitudinal <- tbl_stack(
    tbls = list(pat_longitudinal, care_longitudinal, dyad_longitudinal),
    group_header = c("Patient", "Caregiver", "Dyad")
)

longitudinal

# Data out______________________________________________________________________
