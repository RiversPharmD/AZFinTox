tp_sum <- function(dat, obs, tp) {
    dat_out <- dat %>%
        filter(observation == obs) %>%
        filter(redcap_event_name == tp) %>%
        group_by(survey) %>%
        summarise("tp_{{tp}}" := sum(available))
}
