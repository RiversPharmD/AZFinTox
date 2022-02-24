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
## OutData/*_table_one.csv

################################################################################

# Packages______________________________________________________________________
library(flextable)
library(gtsummary)
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
    "comorbid_sum", "location"
)
pat_vars <- c(care_vars, "stage", "dx")
dyad_vars <- c("income", "children", "marital", "location")


#### Populate Tables
# This returns NaN for the P-value, because it expects levels for gender to
# include transgender. Can either hand-calculate, or include gender as a
# factorVar parameter. I've left it to be hand-calculated here because I think
# it's easier to update a single number than add the transgender row.

care_table_one <- care_wide %>%
    select(care_vars) %>%
    tbl_summary(
        by = location,
        label = list(
            age ~ "Age",
            gender ~ "Gender",
            ethnicity ~ "Ethnicity",
            race ~ "Race",
            education ~ "Education",
            comorbid_sum ~ "Number of Comorbidities"
        )
    ) %>%
    add_p() %>%
    add_overall()

pat_table_one <- pat_wide %>%
    select(pat_vars) %>%
    tbl_summary(
        by = location,
        label = list(
            age ~ "Age",
            gender ~ "Gender",
            ethnicity ~ "Ethnicity",
            race ~ "Race",
            education ~ "Education",
            comorbid_sum ~ "Number of Comorbidities",
            stage ~ "Stage at Diagnosis",
            dx ~ "Disease Site"
        )
    ) %>%
    add_p() %>%
    add_overall()

dyad_table_one <- dyad_wide %>%
    select(dyad_vars) %>%
    tbl_summary(
        by = location,
        label = list(
            income ~ "Household Income",
            children ~ "Presence of Children Under 18 Years Old",
            marital ~ "Marital Status"
        )
    ) %>%
    add_p() %>%
    add_overall()

#### Prep for output
table_one <- tbl_stack(
    tbls = list(
        pat_table_one,
        care_table_one,
        dyad_table_one
    ),
    group_header = c("Patient", "Caregiver", "Dyad")
) %>%
    as_flex_table() %>%
    flextable::footnote(
        i = c(20, 59), ## manually added numbers
        j = 2,
        value = flextable::as_paragraph("Comorbidities include: Heart Disease,
    High Blood Pressure, Lung Disease, Diabetes, Ulcer or Stomach Disease,
    Kidney Disease, Liver Disease, Anemia or Other Blood Disease, Depression,
    Osteoarthritis/Degenerative Arthritis, Back Pain,
    and Rheumatoid Arthritis"),
        ref_symbols = "3",
        part = "body"
    )



table_one
# Data Out______________________________________________________________________

## Table One--------------------------------------------------------------------
file_path <- "OutData/"

flextable::save_as_docx(table_one,
    path = paste0(
        file_path,
        "table_one.docx"
    )
)