################################################################################

## Author: Zach Rivers, @riverspharmd, zrivers@fredhutch.org
## Contributors:

## Purpose: Produce a table one

## Depends on:
## 08PredictorMissingness.r
## flextable
## gtsummary
## tidyverse

## Input:
## IntData/dat_long.csv
## IntData/dat_wide.csv

## Output:
## OutData/*_table_one.csv

################################################################################

# Packages______________________________________________________________________
library(flextable)
library(gtsummary)
library(tidyverse)
source("00Functions.r")
source("Functions/table_one.r")

# Functions_____________________________________________________________________

# Data In_______________________________________________________________________
cohort_list <- list("One" = NA, "Two" = NA, "Three" = NA)
for (x in c("One", "Two", "Three")) {
    path <- "IntData/Cohort"

    cohort_list[[x]] <- read_csv(file = paste0(path, x, ".csv")) %>%
        distinct(partid)
}

pred_data <- read_csv(file = "IntData/dat_wide.csv")
# Analysis______________________________________________________________________
## Join datasets----------------------------------------------------------------
cohort_list <- map(
    .x = cohort_list,
    ~ left_join(
        x = .x,
        y = pred_data
    )
)

### Add appropriate missingness
for (i in 1:3) {
    dat <- cohort_list[[i]]
    dat_na <- dat %>%
        filter(is.na(source) == TRUE)
    dat_na_pat <- dat_na %>%
        mutate(source = 0)
    dat_na_care <- dat_na %>%
        mutate(source = 1)
    dat <- dat %>%
        filter(is.na(source) == FALSE) %>%
        rbind(dat_na_pat) %>%
        rbind(dat_na_care) %>%
        mutate(location = case_when(
            is.na(location) == FALSE ~ location,
            partid < 2000 ~ 0,
            partid >= 2000 ~ 1
        ))
    cohort_list[[i]] <- dat
}

## Create Table One-------------------------------------------------------------

### Prep Data
#### Create cohort dataset

cohort_pop <- cohort_list[[2]] %>%
    mutate(cohort = if_else(partid %in% cohort_list[[3]]$partid,
        3, 2
    ))
#### Factor Data
cohort_list <- map(.x = cohort_list, .f = label_factors)

cohort_pop <- cohort_pop %>%
    label_factors()

### By Location
#### Split into source
cohort_list <- cohort_list %>%
    map(.f = split_by_source)

#### Write Tables

##### Set Vars
var_labels <- label_vars()
output_labels <- label_output()


##### Populate Tables
list_list_table_one <- list(NA, NA, NA)
list_table_one <- list(NA, NA, NA)
for (i in 1:3) {
    list_in <- cohort_list[[i]]
    for (z in 1:3) {
        list_table_one[[z]] <- populate_table_one(
            dat = list_in[[z]],
            var = var_labels[[z]],
            label = output_labels[[z]]
        )
    }
    list_list_table_one[[i]] <- list_table_one
}

list_table_one_by_group <- list(NA, NA, NA)
for (i in 1:3) {
    list_table_one_by_group[[i]] <- list(
        list_list_table_one[[1]][[i]],
        list_list_table_one[[2]][[i]],
        list_list_table_one[[3]][[i]]
    )
}
table_one_merge <- map(
    list_table_one_by_group,
    ~ tbl_merge(
        tbls = .x,
        tab_spanner = c("Cohort One", "Cohort Two", "Cohort Three")
    )
)
table_one_stack <- stack_table_one(list_table_one = table_one_merge)

table_one_stack <- footnote_table_one(table_one_stack, c(25, 58))

### By Cohort
#### Split into source
cohort_pop <- cohort_pop %>%
    split_by_source()

#### Write Tables

##### Set Vars
var_labels <- label_vars(group_var = "cohort")
output_labels <- label_output()


##### Populate Tables

list_table_one <- list(NA, NA, NA)
for (z in 1:3) {
    list_table_one[[z]] <- populate_table_one(
        dat = cohort_pop[[z]],
        var = var_labels[[z]],
        label = output_labels[[z]],
        by = "cohort"
    )
}



table_one_stack_cohort <- stack_table_one(list_table_one = list_table_one)

table_one_stack_cohort <- footnote_table_one(table_one_stack_cohort, c(20, 50))


# Data Out______________________________________________________________________

## Table One--------------------------------------------------------------------
file_path <- "OutData/"

flextable::save_as_docx(table_one_stack,
    path = paste0(
        file_path,
        "table_one_across_cohorts.docx"
    )
)
flextable::save_as_docx(table_one_stack_cohort,
                        path = paste0(
                        file_path,
                        "table_one_cohort_two_and_three.docx"))
