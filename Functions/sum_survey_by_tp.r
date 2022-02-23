#' Summarise Survey at Timepoint
#'
#' Count the number of observations available for a survey at a given timepoint

#' @param dat a dataframe with survey data
#' @param obs a character descriping the observation to summarise by
#' @param tp a numeric value representing the timepoint of interest


tp_sum <- function(dat, obs, tp) {
    dat_out <- dat %>%
        dplyr::filter(observation == obs) %>%
        dplyr::filter(redcap_event_name == tp) %>%
        dplyr::group_by(survey) %>%
        dplyr::summarise("tp_{{tp}}" := sum(available))
}
