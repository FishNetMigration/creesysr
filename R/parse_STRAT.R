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
#'
parse_STRAT <- function(fndf) {
  # fndf can be any FN2 table as long as it has STRAT
  if(!("STRAT" %in% names(fndf))) {stop("STRAT is not a column in this dataframe")}

  # Season
  fndf$SSN <- as.factor(substr(fndf$STRAT, 1, 2))

  # Day type
  fndf$DTP <- as.factor(substr(fndf$STRAT, 4, 4))

  # Period
  fndf$PRD <- as.factor(substr(fndf$STRAT, 5,5))

  # Space
  fndf$SPACE <- as.factor(substr(fndf$STRAT, 7, 8))

  # Mode
  fndf$MODE <- as.factor(substr(fndf$STRAT, 10, 11))

  # Return new appended df
  fndf
}
