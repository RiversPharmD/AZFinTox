bounded_style_ratio <- function(x, min = -Inf, max = Inf, ...) {
    purrr::map_chr(
        x,
        function(x) {
            if (isTRUE(x < min)) {
                return(paste0("<", gtsummary::style_ratio(min, ...)))
            }
            if (isTRUE(x > max)) {
                return(paste0(">", gtsummary::style_ratio(max, ...)))
            }
            gtsummary::style_ratio(x, ...)
        }
    )
}

build_outcome <- function(dat) {
    ### Filter for timepoint 1
    cost <- cost %>%
        filter(redcap_event_name == 1)

    ### Filter for survey availability

    cost <- cost %>%
        import_available_survey_data(
            timepoint = 1,
            survey = "COST",
            obs = "dyad"
        )


    ### Build outcomes
    cost_pat_fintox <- cost %>%
        dplyr::filter(observation == "lgl_p") %>%
        select(-c(survey, observation)) %>%
        rename("p_fintox" = score)

    cost_care_fintox <- cost %>%
        dplyr::filter(observation == "lgl_c") %>%
        select(-c(survey, observation)) %>%
        rename("c_fintox" = score)

    cost_convergence <- cost %>%
        dplyr::filter(observation == "con_dich") %>%
        select(-c(survey)) %>%
        rename("con" = score) %>%
        mutate(con = as_factor(if_else(con > 1, 1, 0))) %>%
        select(-observation)

    ### Join outcomes together
    cost_outcomes <- cost_pat_fintox %>%
        left_join(cost_care_fintox) %>%
        left_join(cost_convergence)

    return(cost_outcomes)
}
build_predictors <- function(dat) {
    #### Split out data
    care_wide <- dat %>%
        filter(source == "Caregiver")

    #### Select columns that are caregiver specific
    care_wide <- care_wide %>%
        select(partid,
            "age_c" = age,
            "gender_c" = gender,
            "ethnicity_c" = ethnicity,
            "race_c" = race,
            "education_c" = education,
            "comorbid_sum_c" = comorbid_sum
        )

    ### Patient

    #### Pull out data
    pat_wide <- dat %>%
        filter(source == "Patient")

    #### Rename columns

    pat_wide <- pat_wide %>%
        select(partid,
            "age_p" = age,
            "gender_p" = gender,
            "ethnicity_p" = ethnicity,
            "race_p" = race,
            "education_p" = education,
            "comorbid_sum_p" = comorbid_sum,
            everything()
        ) %>%
        select(-source)

    ### Rejoin predictors

    pred_wide <- pat_wide %>%
        left_join(care_wide) %>%
        mutate(partid = as.character(partid))

    return(pred_wide)
}

join_pred_out <- function(pred_dat,
                          outcome_dat) {
    pred_outcomes <- pred_dat %>%
        inner_join(outcome_dat) %>%
        select(
            age_p, age_c, gender_p, gender_c, ethnicity_p, ethnicity_c,
            race_p, race_c, education_p, education_c, comorbid_sum_p,
            comorbid_sum_c,
            everything()
        )
    return(pred_outcomes)
}

pat_labels <- list(
    age_p = "Patient Age",
    gender_p = "Patient Gender",
    ethnicity_p = "Patient Ethnicity",
    race_p = "Patient Race",
    education_p = "Patient Education",
    comorbid_sum_p = "Patient Comorbidities"
)
dx_labels <- list(
    location = "Study Site",
    stage = "Tumor Stage",
    dx = "Tumor Site"
)
care_labels <- list(
    age_c = "Caregiver Age",
    gender_c = "Caregiver Gender",
    ethnicity_c = "Caregiver Ethnicity",
    race_c = "Caregiver Race",
    education_c = "Caregiver Education",
    comorbid_sum_c = "Caregiver Comorbidities"
)
dyad_labels <- list(
    marital = "Marital Status",
    income = "Household Income",
    children = "Presence of Children Under 18 Years Old"
)
lr_labels <- c(pat_labels, care_labels, dyad_labels, dx_labels)

bv_log_reg <- function(dat, resp, exp = TRUE) {
    dat_out <- tbl_uvregression(
        data = dat,
        y = resp,
        method = glm,
        include = -outcome_exclude,
        method.args = list(family = binomial),
        exponentiate = exp,
        estimate_fun = purrr::partial(bounded_style_ratio,
            min = 0.001, max = 10
        )
    ) %>%
        modify_footnote(c(estimate, ci) ~
        "ORs <0.001 or larger than 10 shown as '<0.001' and '>10.00'") %>%
        bold_p()
}
mv_log_reg <- function(input, resp, dat) {
    mv_fit <- glm(
        reformulate(input, response = resp),
        data = dat,
        family = binomial
    )
    return(mv_fit)
}

tidy_mv_log_reg <- function(mod_fit, exp) {
    out <- tbl_regression(
        x = mod_fit,
        exponentiate = exp,
        estimate_fun = purrr::partial(bounded_style_ratio,
            min = 0.001, max = 10
        )
    ) %>%
        modify_footnote(c(estimate, ci) ~
        "ORs <0.001 or larger than 10 shown as '<0.001' and '>10.00'") %>%
        bold_p()

    return(out)
}
