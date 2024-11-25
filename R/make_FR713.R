#' Make FR713
#'
#' @param fn022
#' @param fn023
#' @param fn024
#' @param fn025
#' @param fn026
#' @param fn028
#' @param fn111
#' @param fn112
#' @param fn121
#'
#' @return FR713-like table
#' @export
#'
#' @examples
#' \dontrun{
#' make_FR713(SC00$FN022, SC00$FN023, SC00$FN024, SC00$FN025, SC00$FN026, SC00$FN028, SC00$FN112, SC00$FN121)
#' }

make_FR713 <- function(fn022, fn023, fn024, fn025, fn026, fn028, fn111, fn112, fn121){

  SSN_LENGTH <- generate_SSN_LENGTH(fn022, fn023, fn024, fn025)

  if(nrow(fn112 %>%
          dplyr::filter(!is.na(CHKCNT))>0)){
    fn112 <- fn112 %>%
      dplyr::group_by(PRJ_CD, SAMA, ATYTM0) %>%
      dplyr::summarize(PF = sum(as.numeric(ITVCNT))/sum(as.numeric(CHKCNT)))
  } else {
    fn112$PF <- 1
    fn112$CHKCNT <- 0
    }

  fn112_sum <- fn112 %>%
    dplyr::group_by(PRJ_CD, SAMA) %>%
    dplyr::summarize(ATY_hi = sum(as.numeric(ATYCNT))/dplyr::n()*PF,
              ITVCNT_hi = sum(as.numeric(ITVCNT))/dplyr::n()*PF,
              CHKCNT_hi = sum(as.numeric(CHKCNT)))

  Activity_hi <- dplyr::left_join(fn111, fn112_sum, by=c("PRJ_CD", "SAMA")) %>%
    dplyr::select(-COMMENT1) %>%
    parse_STRAT()

  Activity_h <- Activity_hi %>%
    dplyr::group_by(PRJ_CD, STRATUM) %>%
    dplyr::summarize(
      ATY0_h = mean(ATY_hi),
      ATY2_h = mean(ATY_hi), # may change if ATY not done by boat, i.e. winter creel
      CIF_NN = sum(ITVCNT_hi),
      ITVCNT_h = sum(ITVCNT_hi),
      ATY_NN = dplyr::n(),
      ATY_CNT = sum(ATY_hi),
      CHKCNT_S = sum(CHKCNT_hi),
      ITVCNT_S = sum(ITVCNT_hi)
      ) %>% parse_STRAT()

  Activity_h <- left_join(Activity_h, SSN_LENGTH)

  # summarize CIF
  fn121 <- fn121 %>%
    mutate(
      ANGLERS = as.numeric(ANGLERS),
      RODS = as.numeric(RODS),
      EFFDUR = as.numeric(EFFDUR),
      PERSONS = as.numeric(PERSONS)
    )

  fn121_h <- fn121 %>%
    group_by(PRJ_CD, STRATUM) %>%
    summarize(
      ANGLER_MN = sum(ANGLERS)/n(),
      ROD_MNA = sum(RODS)/sum(ANGLERS),
      ANGLERS_S = sum(ANGLERS),
      RODS_S = sum(RODS),
      PERSONS_S = sum(PERSONS),
      ANGLERS_h = sum(ANGLERS)/n(),
      RODS_h = sum(RODS)/n(),
      PERSONS_h = sum(PERSONS)/n(),
      EFFDUR_h = sum(EFFDUR)/n(),
      EFFRO_S = sum(EFFDUR * RODS),
      EFFAO_S = sum(EFFDUR * ANGLERS)
    ) %>% as.data.frame()

  # combine activity with CIF
  FR713_h <- left_join(fn121_h, Activity_h) %>%
    parse_STRAT() %>%
    left_join(., SSN_LENGTH) %>%
    mutate(
      STRAT_HRS_h = STRAT_DAYS * PRD_DUR # this is same as PRD_DUR_h
    ) %>%
    mutate(
      EFFPE_h = ATY2_h * STRAT_HRS_h
    )

  cat("function is not yet complete")

  return(FR713_h)
}


