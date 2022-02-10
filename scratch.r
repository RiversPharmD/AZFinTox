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
            if_else(age < 50, "40-49", )
        )
    )) %>%
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
                        if_else(value_patient < value_caregiver, value_caregiver, value_patient)
                    )
                ), value_patient
            )) %>%
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
            ))
    )

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


## Create list to hold timepoints
biv_list <- list()
for (i in 1:4) {
    ## Create tibble to hold data

    biv_df <- tibble(
        term = character(),
        estimate = numeric(),
        std.error = numeric(),
        statistic = numeric(),
        p.value = numeric(),
        conf.low = numeric(),
        conf.high = numeric()
    )

    ## Build Model
    log_reg_model <-
        logistic_reg() %>%
        set_engine("glm")


    ## Loop over predictors
    for (z in seq_along(pred_names)) {
        x <- pred_names[z]
        ## Fit model
        ## (https://aosmith.rbind.io/2019/06/24/function-for-model-fitting)
        log_reg_fit <-
            log_reg_model %>%
            fit(as.formula(paste("con ~", x)),
                data = cost_list[[i]]
            )
        ## Extract outcomes
        tidy_fit <- tidy(log_reg_fit,
            conf.int = TRUE,
            exponentiate = TRUE
        )
        ##
        ## Attach Outcomes
        biv_df <- biv_df %>%
            rbind(tidy_fit)
    }

    biv_df <- biv_df %>%
        filter(term != "(Intercept)") %>%
        mutate(timepoint = i)

    biv_list[[i]] <- biv_df
}

biv_df_out <- rbind(biv_list[[1]], biv_list[[2]]) %>%
    rbind(biv_list[[3]]) %>%
    rbind(biv_list[[4]])

## Get rid of variables I don't want
tidy_biv_df <- biv_df_out %>%
    select(
        -c(statistic, std.error)
    )

## Cap stuff
## if a value is = 0.0009, it's super small, and if it's 101, its super big
tidy_biv_df <- tidy_biv_df %>%
    mutate(
        estimate = round(case_when(
            estimate < 0.001 ~ 0.001,
            estimate > 100 ~ 101,
            TRUE ~ estimate
        ), digits = 4),
        p.value = round(case_when(
            p.value < 0.001 ~ 0.001,
            p.value > 100 ~ 101,
            TRUE ~ p.value
        ), digits = 4),
        conf.low = round(case_when(
            conf.low < 0.001 ~ 0.001,
            is.na(conf.low) ~ 0.001,
            conf.low > 100 ~ 101,
            TRUE ~ conf.low
        ), digits = 4),
        conf.high = round(case_when(
            conf.high < 0.001 ~ 0.001,
            conf.high > 100 ~ 101,
            is.na(conf.high) ~ 101,
            TRUE ~ conf.high
        ), digits = 4)
    )

## Join confidence intervals

tidy_biv_df <- tidy_biv_df %>%
    mutate(confidence_interval = paste0(
        "(",
        as.character(conf.low),
        "-",
        conf.high,
        ")"
    )) %>%
    select(-c(
        conf.low,
        conf.high
    ))

## Break Term into Three categories

tidy_biv_df <- tidy_biv_df %>%
    mutate(
        observation = case_when(
            str_detect(term, "^age_") == TRUE ~ "Age",
            str_detect(term, "^children") == TRUE ~ "Children",
            str_detect(term, "^comorbid_sum") == TRUE ~ "Comorbidities",
            str_detect(term, "^dx") == TRUE ~ "Diagnosis",
            str_detect(term, "^education_") == TRUE ~ "Education",
            str_detect(term, "^ethnicity_") == TRUE ~ "Ethnicity",
            str_detect(term, "^gender_") == TRUE ~ "Gender",
            str_detect(term, "^income") == TRUE ~ "Income",
            str_detect(term, "^location") == TRUE ~ "Location",
            str_detect(term, "^marital") == TRUE ~ "Marital",
            str_detect(term, "^race_") == TRUE ~ "Race",
            str_detect(term, "^stage") == TRUE ~ "Stage",
            TRUE ~ "ERROR"
        ),
        person = case_when(
            str_detect(term, "_p") == TRUE ~ "Patient",
            str_detect(term, "_c") == TRUE ~ "Caregiver",
            TRUE ~ "Dyad"
        ),
        level = case_when(
            str_detect(term, "Yes") == TRUE ~ "Yes",
            str_detect(term, "Transgender") == TRUE ~ "Transgender",
            str_detect(term, "Colon") == TRUE ~ "Colon",
            str_detect(term, "Lung") == TRUE ~ "Lung",
            str_detect(term, "Rectum") == TRUE ~ "Rectum",
            str_detect(term, "cBA") == TRUE ~ "College Degree",
            str_detect(term, "pBA") == TRUE ~ "College Degree",
            str_detect(term, "HS") == TRUE ~ "High School",
            str_detect(term, "LTBA") == TRUE ~ "Some College",
            str_detect(term, "PostBA") == TRUE ~ "Some Graduate School",
            str_detect(term, "Hispanic") == TRUE ~ "Hispanic",
            str_detect(term, "Female") == TRUE ~ "Female",
            str_detect(term, "100-120k") == TRUE ~ "100-120k",
            str_detect(term, "20-39k") == TRUE ~ "20-39k",
            str_detect(term, "40-59k") == TRUE ~ "40-59k",
            str_detect(term, "60-79k") == TRUE ~ "60-79k",
            str_detect(term, "80-99k") == TRUE ~ "80-99k",
            str_detect(term, "gt120k") == TRUE ~ "Greater than 120k",
            str_detect(term, "SCCA") == TRUE ~ "SCCA",
            str_detect(term, "Partnered") == TRUE ~ "Marital",
            str_detect(term, "Black") == TRUE ~ "Black",
            str_detect(term, "Multi") == TRUE ~ "Multiracial",
            str_detect(term, "Asian") == TRUE ~ "Asian",
            str_detect(term, "AIAN") == TRUE ~ "American Indian or Alaskan Native",
            str_detect(term, "NHPI") == TRUE ~
            "Native Hawaiian or Pacific Islander",
            str_detect(term, "2A") == TRUE ~ "2A",
            str_detect(term, "2B") == TRUE ~ "2B",
            str_detect(term, "2C") == TRUE ~ "2C",
            str_detect(term, "3$") == TRUE ~ "3",
            str_detect(term, "3A") == TRUE ~ "3A",
            str_detect(term, "3B") == TRUE ~ "3B",
            str_detect(term, "3C") == TRUE ~ "3C",
            str_detect(term, "4$") == TRUE ~ "4",
            str_detect(term, "4A") == TRUE ~ "4A",
            str_detect(term, "4B") == TRUE ~ "4B",
            str_detect(term, "^age") == TRUE ~ "1 year",
            str_detect(term, "^comorbid") == TRUE ~ "1 point",
            TRUE ~ "error"
        )
    )
## Rename stuff

tidy_biv_df <- tidy_biv_df %>%
    select(
        "Timepoint" = timepoint,
        "Person" = person,
        "Observation" = observation,
        "Level" = level,
        "Odds Ratio" = estimate,
        "Confidence Interval" = confidence_interval,
        "P Value" = p.value
    ) %>%
    arrange(
        Timepoint,
        Person,
        Observation,
        Level
    )
write_csv(biv_df_out, file = "IntData/bivariate_COST_logistic_regression.csv")
write_csv(tidy_biv_df, file = "OutData/TidyCOSTlogisticregression.csv")
