#' make_FR712
#'
#' @description
#' This function takes FN tables as inputs and generates the FR712 table.
#'
#' @param fn022 FN022 design table
#' @param fn023 FN023 design table
#' @param fn024 FN024 design table
#' @param fn026 FN026 design table
#' @param fn028 FN028 design table
#'
#' @return FR712-like table
#' @export
#'
#' @examples
#' \dontrun{
#' FR712 <- make_FR712(SC00$FN022, SC00$FN023, SC00$FN024, SC00$FN025, SC00$FN026, SC00$FN028, SC00$FN111)
#' }


make_FR712 <- function(fn022, fn023, fn024, fn025, fn026, fn028, fn111) {
  # summarize ssn length
  SSN_LENGTH <- generate_SSN_LENGTH(fn022, fn023, fn024, fn025)
  # create all strata
  ALL_STRATA <- make_all_stratum(fn022, fn023, fn024, fn026, fn028)
  # combine tables
  FR712_raw<- left_join(ALL_STRATA_SC17, SSN_LENGTH_SC17) %>%
    select(STRATUM, STRAT_DAYS, PRD_DUR, PRJ_CD)

  # summarize fn111
  fn111_sum <- fn111 %>% group_by(PRJ_CD, STRATUM) %>%
    summarize(SAM_DAYS = n())
  fn111_sum <- parse_STRAT(fn111_sum)

  # combine fn111 and strata duration tables
  FR712_raw <- left_join(FR712_raw, fn111_sum) %>%
    mutate(SAM_DAYS = ifelse(is.na(SAM_DAYS),0, SAM_DAYS),
           STRAT_HRS = STRAT_DAYS*PRD_DUR) %>%
    select(-MODE)

  # summarize SSN_LENGTH
  FR712_SSN <- SSN_LENGTH %>%
    group_by(PRJ_CD, SSN, PRD_DUR) %>%
    summarize(STRAT_DAYS = sum(STRAT_DAYS)) %>%
    mutate(STRATUM = paste0(SSN, "_++_++_++"))

  # summarize SAM_DAYS by SSN
  FR712_SAM <- fn111_sum %>% group_by(SSN, PRJ_CD) %>%
    summarize(SAM_DAYS = sum(SAM_DAYS))

  FR712_SSN<- inner_join(FR712_SSN, FR712_SAM, by = c("PRJ_CD", "SSN"))

  # summarize N strata combined in SSN
  STRAT_NN <- ALL_STRATA_SC17 %>% group_by(SSN) %>%
    summarize(STRAT_NN = n())

  FR712_SSN <- left_join(FR712_SSN, STRAT_NN, by = c("SSN"))

  # append STRATUM raw and SSN Summary as per FR712 format
  FR712 <- bind_rows(FR712_raw, FR712_SSN) %>%
    rename(STRAT = STRATUM) %>%
    ungroup() %>%
    select(STRAT, STRAT_DAYS, SAM_DAYS, STRAT_HRS, PRD_DUR, STRAT_NN) %>%
    mutate(STRAT_NN = ifelse(is.na(STRAT_NN), 1, STRAT_NN))

  return(FR712)

}
