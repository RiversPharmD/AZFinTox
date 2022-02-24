
sum_longitudinal <- function(dat_in, src, tp_vec, var) {
    # populate var list
    if (src != "dyad") {
        label_list <- list(
            continuous ~ "COST Score",
            binary ~ "COST Binary",
            categorical ~ "COST Categorical"
        )
    } else {
        label_list <- list(
            continuous ~ "Difference in COST",
            binary ~ "Difference in Financial Toxicity",
            categorical ~ "Difference in Experience of Financial Toxicity"
        )
    }

    # select var_label from var_list
    if (var == "continuous") {
        label_var <- label_list[[1]]
    } else if (var == "binary") {
        label_var <- label_list[[2]]
    } else if (var == "categorical") {
        label_var <- label_list[[3]]
    } else {
        stop("Var not in Var List for Function")
    }

    # Build summary table
    dat <- dat_in %>%
        filter(src == {{ src }}) %>%
        filter(redcap_event_name %in% tp_vec) %>%
        tbl_summary(
            by = redcap_event_name,
            include = var,
            label = label_var,
            type = list(where(is.logical) ~ "categorical")
        )

    # label p function -> doesn't work right
    if (var == "continuous") {
        dat <- dat %>%
            add_p(test = continuous ~ "paired.t.test", group = partid)
    } else if (var == "binary") {
        dat <- dat %>%
            add_p(test = binary ~ "mcnemar.test", group = partid)
    } else {
        dat <- dat %>%
            add_p(test = categorical ~ "mcnemar.test", group = partid)
    }

    return(dat)
}
