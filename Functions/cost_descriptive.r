sum_observation <- function(dat, source) {
    if (source != "Dyad") {
        dat_out <- dat %>%
            select(-c(partid, src)) %>%
            mutate(
                binary = factor(binary,
                    levels = pc_bin$levels,
                    labels = pc_bin$labels
                ),
                categorical = factor(categorical,
                    levels = pc_cat$levels,
                    labels = pc_cat$labels
                )
            ) %>%
            tbl_summary(
                by = redcap_event_name,
                label = pc_labels,
                statistic = n ~ "{N}"
            ) %>%
            modify_header(all_stat_cols() ~ "**{level}**")
    } else {
        dat_out <- dat %>%
            select(-c(partid, src)) %>%
            mutate(
                binary = factor(binary,
                    levels = dyad_bin$levels,
                    labels = dyad_bin$labels
                ),
                categorical = factor(categorical,
                    levels = dyad_bin$levels,
                    labels = dyad_bin$labels
                )
            ) %>%
            tbl_summary(
                by = redcap_event_name,
                label = dyad_labels,
                statistic = n ~ "{N}"
            ) %>%
            add_p() %>%
            modify_header(all_stat_cols() ~ "**{level}**")
    }
}

sum_timepoint <- function(dat) {
    dat_sum <- dat %>%
        filter(src != "Dyad") %>%
        select(-c(redcap_event_name)) %>%
        mutate(
            binary = factor(binary,
                levels = pc_bin$levels,
                labels = pc_bin$labels
            ),
            categorical = factor(categorical,
                levels = pc_cat$levels,
                labels = pc_cat$labels
            )
        ) %>%
        tbl_summary(
            by = src,
            include = -partid,
            label = pc_labels,
            statistic = n ~ "{N}"
        ) %>%
        add_p(
            test = all_continuous() ~ "paired.t.test",
            group = partid
        ) %>%
        modify_header(all_stat_cols() ~ "**{level}**")
}

merge_list <- function(list_in) {
    tbl_merge(
        tbls = list_in,
        tab_spanner = c(
            "Timepoint 1",
            "Timepoint 2",
            "Timepoint 3",
            "Timepoint 4"
        )
    )
}


stack_list <- function(list_in) {
    tbl_stack(
        tbls = list_in,
        group_header = c("Patient", "Caregiver", "Dyad")
    )
}
