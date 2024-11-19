#' Generate Season Length
#'
#' @param fn022 FR022 FN2 design table
#' @param fn023 FR023 FN2 design table
#' @param fn024 FR023 FN2 design table
#' @param fn025 FR023 FN2 design table
#'
#' @description
#' This function generates the season length give the FN2 design tables
#' correcting for exception days from FN025
#'
#' @return summary table of SSN length
#' @export
#'
#' @examples
#' \dontrun{
#' generate_SSN_LENGTH(SC00$FN022, SC00$FN023, SC00$FN024, SC00$FN025)
#' }
generate_SSN_LENGTH <- function(fn022, fn023, fn024, fn025){

  SSN_DTP <- fn022 %>%
    dplyr::left_join(., fn023, by=c("SSN", "PRJ_CD"))

  SSN_DTP_list <- split(SSN_DTP, list(SSN_DTP$SSN, SSN_DTP$DTP))

  make_calendar <- function(ssn_dtp) {
    all_dates <- seq.Date(ssn_dtp$SSN_DATE0, ssn_dtp$SSN_DATE1, by = "day")
    day_type <- as.numeric(unlist(strsplit(ssn_dtp$DOW_LST, "")))
    ssn_days <- all_dates[lubridate::wday(all_dates) %in% day_type]
    if(length(ssn_days) == 0) {
      invisible(NULL)
    } else {
      df <- data.frame(PRJ_CD = ssn_dtp$PRJ_CD, SSN = ssn_dtp$SSN, DTP = ssn_dtp$DTP, DATE = ssn_days)
      return(df)
    }
  }

  full_calendar <- dplyr::bind_rows(lapply(SSN_DTP_list, make_calendar))

  # precursor to FR712
  SSN_LENGTH <- dplyr::left_join(full_calendar, fn025, by = c("PRJ_CD", "DATE")) %>%
    dplyr::mutate(DTP = ifelse(!is.na(DTP1), DTP1, DTP)) %>%
    dplyr::select(SSN, DTP, DATE) %>%
    dplyr::group_by(SSN, DTP) %>%
    dplyr::summarize(STRAT_DAYS = dplyr::n()) %>%
    dplyr::left_join(fn023, ., by=c("SSN", "DTP")) %>%
    dplyr::mutate(STRAT_DAYS = ifelse(is.na(STRAT_DAYS), 0, STRAT_DAYS)) %>%
    dplyr::left_join(., fn024, by = c('SSN', "DTP", "PRJ_CD")) %>%
    dplyr::select(-F4, -F3)

  return(SSN_LENGTH)
}
