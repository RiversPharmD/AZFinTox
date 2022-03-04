## Shift Left are scored on a different scale, this shifts them to the
## original scale

shift_left <- function(x) {
    library(dplyr)
    y <- dplyr::if_else(is.na(x) == FALSE, (x - 1), x)
    return(y)
}

##
timepoint <- function(dat, num) {
    dat_new <- dat %>%
        dplyr::filter(redcap_event_name == num)
    return(dat_new)
}

lengthen <- function(dat, x) {
    ## make column name
    value <- stringr::str_c("value_", x)
    ## pivots longer
    dat_new <- dat %>%
        tidyr::pivot_longer(
            cols = !partid:redcap_event_name,
            names_to = "observation",
            values_to = value
        )
    ## Drops variables
    dat_new2 <- dat_new %>%
        dplyr::select(partid, observation, value)
    return(dat_new2)
}

## label_factors: This function takes the wide dataset, and recasts variables as
## factors.

label_factors <- function(dat) {

    #### Location
    dat$location <- factor(dat$location,
        levels = c(0, 1),
        labels = c("Duke", "SCCA")
    )

    #### Gender

    dat$gender <- factor(dat$gender,
        levels = c(0, 1, 2),
        labels = c(
            "Male",
            "Female", "Transgender"
        )
    )

    #### Race

    dat$race <- factor(dat$race,
        levels = c(0:4),
        labels = c(
            "American Indian or Alaskan Native",
            "Asian, Native Hawaiian or Pacific Islander",
            "Black",
            "White",
            "Multiracial"
        )
    )

    dat$race <- relevel(dat$race, ref = "White")
    #### Ethnicity

    dat$ethnicity <- factor(dat$ethnicity,
        levels = c(0, 1),
        labels = c(
            "Non-Hispanic",
            "Hispanic"
        )
    )

    #### Education

    dat$education <- factor(dat$education,
        levels = c(0:2),
        labels = c(
            "Up to High School Diploma",
            "Some College up to a 4 Year Degree",
            "Education beyond 4 Year Degree"
        )
    )

    #### Marital
    dat$marital <- factor(dat$marital,
        levels = c(0, 1),
        labels = c("Married", "Partnered")
    )

    #### Income

    dat$income <- factor(dat$income,
        levels = c(0:2),
        labels = c(
            "Less than 59k",
            "60-120k",
            "Greater than 120k"
        )
    )

    #### Children

    dat$children <- factor(dat$children,
        levels = c(0, 1),
        labels = c("No", "Yes")
    )

    #### Stage

    dat$stage <- factor(dat$stage,
        levels = 0:2,
        labels = c("2", "3", "4")
    )

    #### dx

    dat$dx <- factor(dat$dx,
        levels = 0:3,
        labels = c("Breast", "Colon", "Lung", "Rectum")
    )
    #### Source

    dat$source <- factor(dat$source,
        levels = 0:1,
        labels = c("Patient", "Caregiver")
    )

    #### Age
    dat$age <- as.numeric(dat$age)

    #### comorbid_sum
    dat$comorbid_sum <- as.numeric(dat$comorbid_sum)

    return(dat)
}
