################################################################################

## Author: Zach Rivers, @riverspharmd, zrivers@fredhutch.org
## Contributors:

## Purpose: Produce a table one, and a correlation matrix for the predictor
## variables.

## Depends on:
## 08PredictorMissingness.r
## flextable
## gtsummary
## tidyverse

## Input:
## IntData/dat_long.csv
## IntData/dat_wide.csv

## Output:
## OutData/table_one.docx

################################################################################

# Packages______________________________________________________________________
library(flextable)
library(gtsummary)
library(tidyverse)
source("00Functions.r")
source("Functions/table_one.r")
# Functions_____________________________________________________________________

# Data In_______________________________________________________________________

demo_wide <- read_csv("IntData/dat_wide.csv")

# Analysis______________________________________________________________________

## Create Table One-------------------------------------------------------------

### Factor Data
demo_wide <- demo_wide %>%
    label_factors()

### Split into source
dat_split <- split_by_source(demo_wide)


### Write Tables

#### Set Vars
var_labels <- label_vars()
output_labels <- label_output()

#### Populate Tables

list_table_one <- list(NA, NA, NA)

for (i in 1:3) {
    list_table_one[[i]] <- populate_table_one(
        dat = dat_split[[i]],
        var = var_labels[[i]],
        label = output_labels[[i]]
    )
}

table_one_stack <- stack_table_one(
    list_table_one = list_table_one,
    vec_footnote_position = c(20, 47)
)
#### Prep for output

# Data Out______________________________________________________________________

## Table One--------------------------------------------------------------------
file_path <- "OutData/"

flextable::save_as_docx(table_one_stack,
    path = paste0(
        file_path,
        "table_one.docx"
    )
)
