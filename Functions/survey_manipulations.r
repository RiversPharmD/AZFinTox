## discordance: this function takes the long dataframe, selects a specific
## observation type, and builds in a relative discordance check. It returns
## a list with two dataframes, one with the discordance for each row, and a
## summary of each observation.

discordance <- function(dat, x) {

    ## Load Packages
    library(dplyr)


    ## Creates small dataset
    dat <- dat %>%
        dplyr::filter(observation == x)

    ## Builds Discordance measure
    dat <- dat %>%
        mutate(disc = case_when(
            value_patient == value_caregiver ~ 0,
            value_patient < value_caregiver ~ -1,
            value_patient > value_caregiver ~ 1
        ))
    ## Summarize Discordance measure
    dat_sum <- dat %>%
        group_by(disc) %>%
        count()

    ## Prepare return list
    dat_list <- list(dat, dat_sum)

    ## Return list
    return(dat_list)
}

## align_missing: This function takes the long dataframe of dyad values, pulls
## out a given observation, and replaces missingness in one partner with the
## value of the other. THIS SETS BOTH TO MISSING IF BOTH ARE MISSING. It then
## returns the observation to the long dataset.

align_missing <- function(dat, x) {

    ## Dependencies
    library(dplyr)

    ## slices data

    dat_old <- dat %>%
        filter(observation != x)

    dat_update <- dat %>%
        filter(observation == x)

    ## fill in missingness

    dat_update <- dat_update %>%
        mutate(
            value_patient = if_else(is.na(value_patient) == TRUE,
                value_caregiver,
                value_patient
            ),
            value_caregiver = if_else(is.na(value_caregiver) == TRUE,
                value_patient,
                value_caregiver
            )
        )

    ## rejoins slices

    dat_new <- dat_old %>%
        rbind(dat_update)

    ## Check for correct rows
    ifelse(nrow(dat) != nrow(dat_new),
        print("The length of the new data doesn't match old data "),
        ## Return
        return(dat_new)
    )
}
## align_discordant: This function takes a long dataset, an observation of
## interest, and a logical imput. It "checks out" all of the observations of
## interest, and sets both the patient and caregiver values to the maximum of
## the two values if the logical "max" is true, or the minimum if "max" is
## false.

align_discordant <- function(dat, x, max = TRUE) {

    ## Dependencies
    library(dplyr)

    ## Slices data

    dat_old <- dat %>%
        filter(observation != x)

    dat_update <- dat %>%
        filter(observation == x)

    ## Adds placeholder column

    dat_update <- dat_update %>%
        mutate(placeholder = NA)

    ## Checks logical
    if (max == TRUE) {
        ## TRUE
        dat_update <- dat_update %>%
            rowwise() %>%
            mutate(placeholder = max(c_across(value_patient:value_caregiver)))
    } else {
        ## FALSE
        dat_update <- dat_update %>%
            rowwise() %>%
            mutate(placeholder = min(c_across(value_patient:value_caregiver)))
    }

    ## set value_patient and value_caregiver to placeholder

    dat_update <- dat_update %>%
        mutate(
            value_patient = placeholder,
            value_caregiver = placeholder
        )

    ## drop placeholder

    dat_update <- dat_update %>%
        select(-placeholder)

    ## Insert back
    dat_new <- dat_old %>%
        rbind(dat_update)

    ## Check for correct rows
    ifelse(nrow(dat) != nrow(dat_new),
        print("The length of the new data doesn't match old data "),
        ## Return
        return(dat_new)
    )
}
## extract_missing: This function pulls the partids the correspond with missing
## observations in a given column. It takes a long dataframe, scans it for NAs
## in the column named X, and returns a vector of partids.

extract_missing <- function(dat, x) {

    ## Dependencies
    library(dplyr)

    ## quosure magic that i don't really understand
    ## (https://rpubs.com/hadley/dplyr-programming)
    x <- enquo(x)

    ## filters based on X
    dat <- dat %>%
        dplyr::filter(is.na(!!x))

    ## Selects vector of partids
    vec <- dat$partid

    ## returns vector
    return(vec)
}
