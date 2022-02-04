## Analysis

#### Trends over time

##### COST

###### Patient
ggplot(
  data = cost,
  mapping = aes(
    x = redcap_event_name,
    y = facit_cost_sum_p,
    group = redcap_event_name
  )
) +
  geom_boxplot()


  ### Visualisations


  ggplot(
    data = dat_long,
    aes(x = value)
  ) +
    geom_histogram() +
    facet_wrap(vars(observation))

  ### Individual Vars

  #### Children and childrennum

  children <- dat %>%
    select(children, childrennum) %>%
    group_by(children, childrennum) %>%
    summarise(count = n())

  #### Education

  education <- dat %>%
    select(education) %>%
    mutate(education = as_factor(education)) %>%
    group_by(education) %>%
    count()

  #### Ethnicity

  ethnicity <- dat %>%
    select(ethnicity) %>%
    mutate(ethnicity = as_factor(ethnicity)) %>%
    group_by(ethnicity) %>%
    count()

  #### Gender

  gender <- dat %>%
  select(gender) %>%
  mutate(gender = as_factor(gender)) %>%
  group_by(gender) %>%
  count()

  #### Income

  income <- dat %>%
  select(income) %>%
  mutate(income = as_factor(income)) %>%
  group_by(income) %>%
  count()

  #### Marital

  marital <- dat %>%
  select(marital) %>%
  mutate(marital = as_factor(marital)) %>%
  group_by(marital) %>%
  count()

  #### race

  race <- dat %>%
  select(race) %>%
  mutate(race = as_factor(race)) %>%
  group_by(race) %>%
  count()

  #### Age

  age <- dat %>%
  select(age) %>%
  mutate(age = if_else(age < 30, "<30",
  if_else(age < 40, "30-39",
  if_else(age < 50, "40-49",)))) %>%
  group_by(age) %>%
  count()

  ## Align: This function takes the long dataset and a specific observation as
  ## inputs, checks for partner concordance, and creates concordance. It uses the
  ## max argument to decide if it should set partner values to the max of both. If
  ## max is T, then it will set them both to max. If max is F, then it sets them
  ## both to the minimum
  align <- function(dat, x, max = TRUE) {

 ## Dependencies
   library(dplyr)

 ifelse(max == TRUE,

   ## True
   dat <- dat %>%
   mutate(value_patient = if_else(observation == x,
     if_else(is.na(value_patient) == T, value_caregiver,
       if_else(value_patient >= value_caregiver, value_patient,
         if_else(value_patient < value_caregiver, value_caregiver, value_patient
                 )
               )
             ), value_patient
   )
 ) %>%
   mutate(value_caregiver = if_else(observation == x,
     if_else(is.na(value_caregiver) == T, value_patient,
       if_else(value_caregiver >= value_patient, value_caregiver,
         if_else(value_caregiver < value_patient, value_patient, value_caregiver)
       )
     ), value_caregiver
   )),
 ## False
 dat <- dat %>%
  mutate(value_patient = if_else(observation == x,
    if_else(is.na(value_patient) == T, value_caregiver,
      if_else(value_patient <= value_caregiver, value_patient,
        if_else(value_patient > value_caregiver, value_caregiver, value_patient)
      )
    ), value_patient
  )) %>%
  mutate(value_caregiver = if_else(observation == x,
    if_else(is.na(value_caregiver) == T, value_patient,
      if_else(value_caregiver <= value_patient, value_caregiver,
        if_else(value_caregiver > value_patient, value_patient, value_caregiver)
      )
    ), value_caregiver
  )))

  ## Return
  return(dat)
 }



### Collapse into one column of predictors

pred_dyad <- pred_long %>%
    filter(observation %in% c(
        "location",
        "DX",
        "stage", "income", "marital", "children"
    )) %>%
    select(partid,
        observation,
        "value" = value_patient
    )

pred_patient <- pred_long %>%
    filter(observation %in% c(
        "age", "gender", "ethnicity", "race", "education",
        "comorbid_sum"
    )) %>%
    select(partid,
        observation,
        "value" = value_patient
    ) %>%
    mutate(observation = paste0(observation, "_p"))

pred_care <- pred_long %>%
    filter(observation %in% c(
        "age", "gender", "ethnicity", "race", "education",
        "comorbid_sum"
    )) %>%
    select(partid,
        observation,
        "value" = value_caregiver
    ) %>%
    mutate(observation = paste0(observation, "_c"))

### Rejoin

pred_long <- pred_dyad %>%
    rbind(pred_patient) %>%
    rbind(pred_care)

### Pull out observations to loop over them

pred_names <- unique(pred_long$observation)

### Pivot wider

pred_wide <- pred_long %>%
    pivot_wider(
        names_from = observation,
        values_from = value
    )
for (i in 1:4) {
    df <- cost_list[[i]]

    df <- df %>%
        mutate(across(
            !c(age_p, age_c, comorbid_sum_p, comorbid_sum_c),
            as_factor
        )) %>%
        mutate(across(
            !c(age_p, age_c, comorbid_sum_p, comorbid_sum_c),
            relevel(ref = 0)
        )) %>%
        mutate(across(
            c(age_p, age_c, comorbid_sum_p, comorbid_sum_c),
            as.double
        ))
    levels(df$income) <- c()

    cost_list[[i]] <- df
}
