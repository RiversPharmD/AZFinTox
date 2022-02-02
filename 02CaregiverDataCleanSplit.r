################################################################################

## Author: Zach Rivers, @riverspharmd, zrivers@fredhutch.org
## Contributors:

## Purpose: This file takes the raw caregiver .xlsx file, splits it into the
## sections, and tidies it.

## Depends on:
## library(tidyverse)
## library(readxl)

## Input:
## RawData/"CG COST data 123121".xlsx

## Output:
## IntData/demo_care.csv
## IntData/comor_care.csv
## IntData/ies_care.csv
## IntData/poms_care.csv
## IntData/cesd_care.csv
## IntData/fact_care.csv
## IntData/facitcost_care.csv
## IntData/covid_care.csv

################################################################################

## Packages
library(tidyverse)
library(readxl)


## Read data in

dat <- read_xlsx("RawData/CG COST data 123121.xlsx")


## Splitting by Instrument
## all files will keep the partid and redcap_event_name

### Demographic

demo <- dat %>%
  select(
    partid,
    redcap_event_name,
    age,
    gender,
    ethnicity,
    race,
    education,
    marital,
    income,
    children,
    childrennum
  )

### Comorbidities

comor <- dat %>%
  select(
    partid,
    redcap_event_name,
    comorbid01:comorbid13c,
    comorbid_sum
  )

### IES

ies <- dat %>%
  select(
    partid,
    redcap_event_name,
    ies01:ies15
  )

### POMS

poms <- dat %>%
  select(
    partid,
    redcap_event_name,
    poms01:poms35,
    poms01_rec:poms35_rec,
    POMS_30_TMD_mean
  )

### CESD

cesd <- dat %>%
  select(
    partid,
    redcap_event_name,
    cesd01:cesd10,
    cesd05_rec:cesd08_rec,
    CESD_mean,
    CESD_sum
  )


### FACT

fact <- dat %>%
  select(
    partid,
    redcap_event_name,
    fact1:fact21,
    fact1_rec:fact15_rec,
    FACT_physical_mean,
    FACT_social_mean,
    FACT_emotional_mean,
    FACT_functional_mean
  )




### FACIT_COST
facit_cost <- dat %>%
  select(
    partid,
    redcap_event_name,
    facit_cost01:facit_cost11
  )




### COVID

covid <- dat %>%
  select(
    partid,
    redcap_event_name,
    covid01:covid12
  )

## Write Data out

write_csv(x = demo, file = "IntData/demo_care.csv")
write_csv(x = comor, file = "IntData/comor_care.csv")
write_csv(x = ies, file = "IntData/ies_care.csv")
write_csv(x = poms, file = "IntData/poms_care.csv")
write_csv(x = cesd, file = "IntData/cesd_care.csv")
write_csv(x = fact, file = "IntData/fact_care.csv")
write_csv(x = facit_cost, file = "IntData/facitcost_care.csv")
write_csv(x = covid, file = "IntData/covid_care.csv")
