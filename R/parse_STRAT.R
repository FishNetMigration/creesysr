#' parse_STRAT
#' @description
#' STRAT is an important grouping variable used throughout FN2. Often though
#' it is necessary to have the individual components parsed out in to data columns
#' in order to be able to join tables together.
#'
#'
#' @param fndf a FN2 data table that includes STRAT as a column
#'
#' @return return the same FN2 dataframe with SSN, DTP, PRD, SPACE, and MODE columns as factors
#' @export
#'
#' @examples
#' dummydf <- data.frame(STRAT = c("01_13_01_02", "01_11_02_01", "03_22_04_01"))
#' parse_STRAT(dummydf)

parse_STRAT <- function(fndf) {
  # fndf can be any FN2 table as long as it has STRAT
  if(!("STRATUM" %in% names(fndf))) {stop("STRATUM is not a column in this dataframe")}

  # Season
  fndf$SSN <- as.factor(substr(fndf$STRATUM, 1, 2))

  # Day type
  fndf$DTP <- as.factor(substr(fndf$STRATUM, 4, 4))

  # Period
  fndf$PRD <- as.factor(substr(fndf$STRATUM, 5,5))

  # Space
  fndf$SPACE <- as.factor(substr(fndf$STRATUM, 7, 8))

  # Mode
  fndf$MODE <- as.factor(substr(fndf$STRATUM, 10, 11))

  # Return new appended df
  fndf
}
