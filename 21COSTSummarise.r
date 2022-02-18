################################################################################

## Author: Zach Rivers, @riverspharmd, zrivers@fredhutch.org
## Contributors:

## Purpose: Calculate summary statistics on COST scores across timepoints

## Depends on:
## 00Functions.r
## 10COSTTidyJoin.r
## tidyverse

## Input:
## Output:

################################################################################

# Packages
library(tidyverse)
source("00Functions.r")

# Functions

# Data In

## COST

cost <- list(
    cost_narrow = read_csv(
        file = "IntData/cost.csv",
        col_types = "nfffn"
        # ),
        # cost_full = read_csv(
        #    file = "IntData/cost_full.csv"
    )
)

# Analysis

## Initiate Lists and Tibbles

cost_dat_avail <- list()
dat_list <- list()

df_dat <- tibble()

## Loop over available data

for (i in 1:4) {

    # Read in data to include only those where both dyad members are available
    dat <- import_available_survey_data(
        data = cost$cost_narrow,
        timepoint = i,
        survey = "COST",
        obs = "dyad"
    )

    # Put in list to reuse later
    cost_dat_avail[[i]] <- dat


    # split into numeric, logical, categorical, and factor_concordance

    dat_num <- dat %>%
        filter(observation %in% c("sum_p", "sum_c", "con_raw"))

    dat_lgl <- dat %>%
        filter(observation %in% c(
            "lgl_p", "lgl_c"
        )) %>%
        mutate(score = as_factor(score))
    dat_cat <- dat %>%
        filter(observation %in% c("cat_p", "cat_c")) %>%
        mutate(score = as_factor(score))

    dat_con <- dat %>%
        filter(observation %in% c("con_dich", "con_cat")) %>%
        mutate(score = as_factor(score))
    # Analyze numeric

    ## Create con_abs

    con_abs <- dat_num %>%
        filter(observation == "con_raw") %>%
        mutate(
            observation = "con_abs",
            score = abs(score)
        )

    ## rejoin con_abs
    dat_num <- dat_num %>%
        rbind(con_abs)

    ## Summarise

    dat_num_sum <- dat_num %>%
        group_by(observation) %>%
        summarise(
            mean = mean(score),
            sd = sd(score)
        )

    # Analyze factor

    ## Find number of obs
    num_obs <- length(unique(dat$partid))

    ## Summarise logical
    dat_lgl_sum <- dat_lgl %>%
        group_by(observation) %>%
        count(score) %>%
        mutate(per = round((n * 100) / num_obs, 2))

    ## Summarise categorical

    dat_cat_sum <- dat_cat %>%
        group_by(observation) %>%
        count(score) %>%
        mutate(per = round((n * 100) / num_obs, 2))

    ## Summarise concordance

    dat_con_sum <- dat_con %>%
        group_by(observation) %>%
        count(score) %>%
        mutate(per = round((n * 100) / num_obs, 2))

    ## Bind back together
    dat_fact_sum <- rbind(
        dat_lgl_sum,
        dat_cat_sum,
        dat_con_sum
    )
    # Put into dataframe

    ## numeric

    ### Lengthen

    dat_num_sum_long <- dat_num_sum %>%
        pivot_longer(
            cols = c(mean, sd),
            names_to = "stat",
            values_to = "value"
        )

    ### Widen

    dat_num_sum_wide <- dat_num_sum_long %>%
        pivot_wider(
            names_from = c(observation, stat),
            names_sep = "_",
            values_from = value
        ) %>%
        mutate(tp = i)

    ## Factor

    ### Lengthen

    dat_fact_sum_long <- dat_fact_sum %>%
        pivot_longer(
            cols = c(n, per),
            names_to = "stat",
            values_to = "value"
        )

    ### Widen
    dat_fact_sum_wide <- dat_fact_sum_long %>%
        pivot_wider(
            names_from = c(observation, score, stat),
            names_sep = "_",
            values_from = value
        ) %>%
        mutate(tp = i)
    # Join back together

    dat_wide <- dat_fact_sum_wide %>%
        left_join(dat_num_sum_wide) %>%
        mutate(n_dyads = num_obs)

    # Pivot again!

    dat_long <- dat_wide %>%
        pivot_longer(
            cols = -tp,
            names_to = "observation",
            values_to = paste0("value_", i)
        ) %>%
        select(-tp)

    # attach to external list
    dat_list[[i]] <- dat_long
}


dat <- dat_list[[1]] %>%
    full_join(dat_list[[2]]) %>%
    full_join(dat_list[[3]]) %>%
    full_join(dat_list[[4]]) %>%
    mutate(across(where(is.numeric), ~ round(., digits = 2))) %>%
    mutate(across(where(is.numeric), ~ case_when(
        is.na(.) == TRUE ~ 0,
        is.na(.) == FALSE ~ .
    ))) %>%
    arrange(observation)
# Tidy

## set up data to split it

dat <- dat %>%
    mutate(type = case_when(
        str_detect(
            string = observation,
            pattern = "mean"
        ) == T ~ "cont",
        str_detect(
            string = observation,
            pattern = "sd"
        ) == T ~ "cont",
        str_detect(
            string = observation,
            pattern = "dyads"
        ) ~ "other",
        TRUE ~ "cat"
    ))

## Split data
dat_other <- dat %>%
    filter(type == "other")

dat_cont <- dat %>%
    filter(type == "cont")

dat_cat <- dat %>%
    filter(type == "cat")

## Tidy

## Categorical

#### Pivot longer

dat_cat_long <- dat_cat %>%
    select(-type) %>%
    pivot_longer(
        cols = -observation,
        names_to = "timepoint",
        names_prefix = "value_",
        values_to = "value"
    )

#### Separate Observation

dat_cat_long <- dat_cat_long %>%
    separate(
        col = observation,
        into = c("type", "source", "score", "stat"),
        sep = "\\_"
    )

#### Pivot wider

dat_cat_wide <- dat_cat_long %>%
    pivot_wider(
        names_from = stat,
        values_from = value
    )

#### Staple together

dat_cat_wide <- dat_cat_wide %>%
    mutate(obs = paste0(n, " (", per, ")")) %>%
    select(-c(n, per))

#### Widen by timepoint

dat_cat_wide <- dat_cat_wide %>%
    pivot_wider(
        names_from = timepoint,
        names_prefix = "timepoint_",
        values_from = obs
    )

#### Mutate variable names, add sort for sorting

dat_cat_wide <- dat_cat_wide %>%
    mutate(
        type = if_else(type == "con", source, type),
        type = if_else(type == "dich", "lgl", type),
        source = if_else(source %in% c("cat", "lgl", "dich"), "con", source),
        sort = 3
    )

### Numeric

#### Pivot longer
dat_cont_long <- dat_cont %>%
    select(-type) %>%
    pivot_longer(
        cols = -observation,
        names_to = "timepoint",
        names_prefix = "value_",
        values_to = "value"
    )

#### Separate observation

dat_cont_long <- dat_cont_long %>%
    separate(
        col = observation,
        into = c("type", "source", "stat"),
        sep = "_"
    )

#### Pivot Wider

dat_cont_wide <- dat_cont_long %>%
    pivot_wider(
        names_from = stat,
        values_from = value
    )

#### staple together

dat_cont_wide <- dat_cont_wide %>%
    mutate(obs = paste0(mean, " (", sd, ")")) %>%
    select(-c(mean, sd))

#### Pivot again

dat_cont_wide <- dat_cont_wide %>%
    pivot_wider(
        names_from = timepoint,
        names_prefix = "timepoint_",
        values_from = obs
    )

#### Rename columns, add Score for sorting
dat_cont_wide <- dat_cont_wide %>%
    mutate(
        type = "num",
        source = if_else(source %in% c("abs", "raw"), "con", source),
        score = NA_integer_,
        sort = 2
    )

### Sample size
dat_other_wide <- dat_other %>%
    mutate(
        source = "dyad",
        type = "num",
        score = NA_integer_,
        sort = 1
    ) %>%
    rename(
        "timepoint_1" = value_1,
        "timepoint_2" = value_2,
        "timepoint_3" = value_3,
        "timepoint_4" = value_4
    ) %>%
    select(-observation)
## Staple them all together
tidy_dat_sum <- dat_other_wide %>%
    rbind(dat_cont_wide) %>%
    rbind(dat_cat_wide) %>%
    select(source, score, everything()) %>%
    mutate(source = if_else(source == "con", "z_con", source)) %>%
    arrange(sort, type, source, score)

# Data Out
write_csv(x = tidy_dat_sum,
file = "OutData/TidyCostSummary.csv")
