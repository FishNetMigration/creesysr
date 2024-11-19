#' Make All Stratum
#'
#' @param fn022 FN022 design table
#' @param fn023 FN023 design table
#' @param fn024 FN024 design table
#' @param fn026 FN026 design table
#' @param fn028 FN028 design table
#'
#' @return dataframe with all possible strata
#' @export
#' @examples
#' \dontrun{
#' make_all_stratum(SC00$FN022, SC00$FN023, SC00$FN024, SC00$FN026, SC00$FN028)
#' }
#'
#'

make_all_stratum <- function(fn022, fn023, fn024, fn026, fn028){
  SSN <- unique(fn022$SSN)
  DTP <- unique(fn023$DTP)
  PRD <- unique(fn024$PRD)
  SPACE <- unique(fn026$SPACE)
  MODE <- unique(fn028$MODE)

  STRATUM <- expand.grid(SSN = SSN, DTP = DTP, PRD = PRD, SPACE = SPACE, MODE = MODE)
  STRATUM <- STRATUM %>%
    mutate(STRATUM = paste0(SSN, "_", DTP, PRD, "_", SPACE, "_", MODE))
  STRATUM <- STRATUM %>% group_by(STRATUM, SSN, DTP, PRD, SPACE, MODE) %>% summarize
  STRATUM
}

