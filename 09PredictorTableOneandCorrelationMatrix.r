################################################################################

## Author: Zach Rivers, @riverspharmd, zrivers@fredhutch.org
## Contributors:

## Purpose: Produce a table one, and a correlation matrix for the predictor
## variables.

## Depends on:
## 08PredictorMissingness.r
## GGally
## tableone
## tidyverse

## Input:
## IntData/dat_long.csv
## IntData/dat_wide.csv

## Output:
## OutData/*_table_one.csv

################################################################################

# Packages______________________________________________________________________
library(GGally)
library(tableone)
library(tidyverse)
source("00Functions.r")

# Functions_____________________________________________________________________

# Data In_______________________________________________________________________
demo_long <- read_csv("IntData/dat_long.csv")
demo_wide <- read_csv("IntData/dat_wide.csv")

# Analysis______________________________________________________________________

## Create Table One-------------------------------------------------------------

### Factor Data
demo_wide <- demo_wide %>%
    label_factors()

### Split into source
pat_wide <- demo_wide %>%
    filter(source == "Patient") %>%
    select(
        partid, location,
        age, gender, ethnicity, race, education, comorbid_sum,
        stage, dx,
        source
    )

care_wide <- demo_wide %>%
    filter(source == "Caregiver") %>%
    select(
        partid, location,
        age, gender, ethnicity, race, education, comorbid_sum,
        source
    )

dyad_wide <- demo_wide %>%
    filter(source == "Patient") %>%
    select(
        partid, location,
        income, children, marital
    )

### Write Tables

#### Set Vars
care_vars <- c(
    "age", "gender", "ethnicity", "race", "education",
    "comorbid_sum"
)
pat_vars <- c(care_vars, "stage", "dx")
dyad_vars <- c("income", "children", "marital")


#### Populate Tables
# This returns NaN for the P-value, because it expects levels for gender to
# include transgender. Can either hand-calculate, or include gender as a
# factorVar parameter. I've left it to be hand-calculated here because I think
# it's easier to update a single number than add the transgender row.

care_table_one <- CreateTableOne(
    vars = care_vars,
    data = care_wide,
    strata = "location", test = TRUE
)

pat_table_one <- CreateTableOne(
    vars = pat_vars,
    data = pat_wide,
    strata = "location",
    test = TRUE
)

dyad_table_one <- CreateTableOne(
    vars = dyad_vars,
    data = dyad_wide,
    strata = "location",
    test = TRUE
)

#### Prep for output
table_one_list <- list(
    care_table_one,
    pat_table_one,
    dyad_table_one
)

names(table_one_list) <- c(
    "Caregiver",
    "Patient",
    "Dyad"
)

table_one_print <- list()
for (i in seq_along(table_one_list)) {
    dat <- table_one_list[[i]]

    table_one_print[[i]] <- print(dat,
        noSpaces = TRUE,
        quote = FALSE,
        printToggle = FALSE
    )
}

## Correllation Plot------------------------------------------------------------

### Set up data

#### Widen
pat_wide <- pat_wide %>%
    pivot_wider(
        names_from = source,
        values_from = age:comorbid_sum
    )

care_wide <- care_wide %>%
    pivot_wider(
        names_from = source,
        values_from = age:comorbid_sum
    )

#### Join
dat_wide <- pat_wide %>%
    left_join(care_wide) %>%
    left_join(dyad_wide) %>%
    select(-partid)

#### Split into Numeric and Factor

dat_wide_num <- dat_wide %>%
    select(where(is.numeric))

dat_wide_fac <- dat_wide %>%
    select(where(is.factor))


### Plot
num_plot_list <- list()
for(i in seq_along(dat_wide_num) {
    dat <- dat[[i]]


 }
# Data Out______________________________________________________________________

## Table One--------------------------------------------------------------------
path <- "OutData/"

for (i in seq_along(table_one_print)) {
    nam <- names(table_one_list)[[i]]

    write.csv( ## use . rather than _ to handle matrix object
        x = table_one_print[[i]],
        file = paste0(
            path,
            nam,
            "_table_one.csv"
        )
    )
}
