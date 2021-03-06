split_by_source <- function(dat) {
    pat_wide <- dat %>%
        filter(source == "Patient") %>%
        select(any_of(c(
            "partid", "location",
            "age", "gender", "ethnicity", "race", "education", "comorbid_sum",
            "stage", "dx",
            "source", "cohort"
        )))

    care_wide <- dat %>%
        filter(source == "Caregiver") %>%
        select(any_of(c(
            "partid", "location",
            "age", "gender", "ethnicity", "race", "education", "comorbid_sum",
            "source", "cohort"
        )))

    dyad_wide <- dat %>%
        filter(source == "Patient") %>%
        select(any_of(c(
            "partid", "location",
            "income", "children", "marital", "cohort"
        )))
    list_out <- list(
        care_wide,
        pat_wide,
        dyad_wide
    )
}

label_vars <- function(group_var = "location") {
    if (group_var == "location") {
        label_vars_out <- list(
            care_vars <- c(
                "age", "gender", "ethnicity", "race", "education",
                "comorbid_sum", "location"
            ),
            pat_vars <- c(care_vars, "stage", "dx"),
            dyad_vars <- c("income", "children", "marital", "location")
        )
    } else if (group_var == "cohort") {
        label_vars_out <- list(
            care_vars <- c(
                "age", "gender", "ethnicity", "race", "education",
                "comorbid_sum", "location", "cohort"
            ),
            pat_vars <- c(care_vars, "stage", "dx"),
            dyad_vars <- c(
                "income", "children", "marital", "location",
                "cohort"
            )
        )
    } else {
        stop("Pick either location or cohort")
    }
}


label_output <- function() {
    out_list <- list(
        care_labels <- list(
            location ~ "Study Site",
            age ~ "Age",
            gender ~ "Gender",
            ethnicity ~ "Ethnicity",
            race ~ "Race",
            education ~ "Education",
            comorbid_sum ~ "Number of Comorbidities"
        ),
        pat_labels <- list(
            location ~ "Study Site",
            age ~ "Age",
            gender ~ "Gender",
            ethnicity ~ "Ethnicity",
            race ~ "Race",
            education ~ "Education",
            comorbid_sum ~ "Number of Comorbidities",
            stage ~ "Stage at Diagnosis",
            dx ~ "Disease Site"
        ),
        dyad_labels <- list(
            location ~ "Study Site",
            income ~ "Household Income",
            children ~ "Presence of Children Under 18 Years Old",
            marital ~ "Marital Status"
        )
    )
}
populate_table_one <- function(dat,
                               var,
                               by = "NONE",
                               label) {
    if (by == "NONE") {
        dat_out <- dat %>%
            select(var) %>%
            tbl_summary(
                label = label,
                missing = "ifany"
            )
    } else {
        dat_out <- dat %>%
            select(var) %>%
            tbl_summary(
                by = {{ by }},
                label = label,
                missing = "ifany"
            ) %>%
            add_p() %>%
            add_overall()
    }
}

stack_table_one <- function(list_table_one) {
    table_out <- tbl_stack(
        tbls = list_table_one,
        group_header = c("Caregiver", "Patient", "Dyad")
    )
}

footnote_table_one <- function(gt_in, vec_footnote_position) {
    gt_out <- gt_in %>%
        as_flex_table() %>%
        flextable::footnote(
            i = vec_footnote_position, ## manually added numbers
            j = 2,
            value = flextable::as_paragraph("Comorbidities include:
            Heart Disease, High Blood Pressure, Lung Disease,
            Diabetes, Ulcer or Stomach Disease, Kidney Disease,
            Liver Disease, Anemia or Other Blood Disease, Depression,
            Osteoarthritis/Degenerative Arthritis, Back Pain,
            and Rheumatoid Arthritis"),
            ref_symbols = "3",
            part = "body"
        )
}
