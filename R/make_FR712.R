#' make_FR712
#'
#' @param fn022
#' @param fn023
#' @param fn025
#'
#' @description
#' This function takes FN tables as inputs and generates the FR712 table.
#'
#' @return FR712 table
#' @export
#'
#' @examples

make_FR712 <- function(fn022 = FN022, fn023 = FN023, fn025 = FN025) {

  #Restrict data to only required fields
  fn022 <- fn022 %>% select(PRJ_CD, SSN, SSN_DATE0, SSN_DATE1)
  fn023 <- fn023 %>% select(PRJ_CD, SSN, DTP, DOW_LST)
  fn025 <- fn025 %>% select(PRJ_CD, DATE, DTP1)

  #Validation to check for gaps in SSN strata
  VAL <- fn022 %>%
    mutate(SSN_DATE0 = as.Date(SSN_DATE0, format = "%Y-%m-%d"))%>%
    mutate(SSN_DATE1 = as.Date(SSN_DATE1, format = "%Y-%m-%d"))%>%
    arrange(SSN_DATE0)%>%
    mutate(SSN_CHECK = ifelse((SSN_DATE1 + 1) != lead(SSN_DATE0), "ISSUE", "OK"))%>%
    mutate(SSN_CHECK = ifelse(is.na(SSN_CHECK), "OK", SSN_CHECK))

  if(any(VAL$SSN_CHECK == "ISSUE")){stop("There is a gap between between the seasonal strata.")}

  #Merge the FN022 and FN023 tables
  SSN_DAYS <- merge(fn022, fn023, all.x = TRUE, all.y = TRUE)

  #Validation to check that FN023 contains all seasons in FN022
  if(any(is.na(SSN_DAYS$DTP))){stop("There are missing SSN values in the FN023 table.")}

  #Merge the new table with the FN025 table
  EXCEPTDAYS <- merge(SSN_DAYS, fn025, all.x = TRUE)

  #No validation was added to check that FN025 contains all seasons in FN022
  #Assumed that we can exclude any FN025 outside seasonal ranges

  #Identify which DTP values need adjustment
  #This information is stored in the m025 field
  EXCEPTDAYS <- EXCEPTDAYS %>%
    mutate(SSN_DATE0 = as.Date(SSN_DATE0, format = "%Y-%m-%d"))%>%
    mutate(SSN_DATE1 = as.Date(SSN_DATE1, format = "%Y-%m-%d"))%>%
    mutate(DATE = as.Date(DATE, format = "%Y-%m-%d"))%>%
    mutate(m025 = ifelse(DATE >= SSN_DATE0 & DATE <= SSN_DATE1, ifelse(DTP1 == DTP, 1, -1), 0))%>%
    mutate(m025 = ifelse(is.na(DATE), 0, m025))%>%
    mutate(DTP1 = ifelse(is.na(DTP1), 2, DTP1))%>%
    group_by(PRJ_CD, SSN, SSN_DATE0, SSN_DATE1, DTP, DTP1, DOW_LST)%>%
    summarise(m025 = sum(m025), .groups = "drop")%>%
    select(PRJ_CD, SSN, SSN_DATE0, SSN_DATE1, DTP, DTP1, DOW_LST, m025)

  #Calculate the number of STRAT_DAYS
  STRATDAYS <- EXCEPTDAYS %>%
    mutate(Days = as.numeric((SSN_DATE1-SSN_DATE0)+1))%>%
    mutate(FWeeks = round(Days/7,0))%>%
    mutate(ExDays = Days-FWeeks*7)%>%
    mutate(FWeek1 = SSN_DATE1-ExDays)%>%
    mutate(DOW0 = as.numeric(format(FWeek1,'%w'))+1)%>%
    mutate(Su = ifelse(grepl("1", DOW_LST, fixed=TRUE),
                       FWeeks+ifelse((DOW0+ExDays)>7,1,0),0))%>%
    mutate(Mo = ifelse(grepl("2", DOW_LST, fixed=TRUE),
                       FWeeks+ifelse(DOW0<2 & (DOW0+ExDays)>=2 | DOW0>2 & (DOW0+ExDays)>=9,1,0),0))%>%
    mutate(Tu = ifelse(grepl("3", DOW_LST, fixed=TRUE),
                       FWeeks+ifelse(DOW0<3 & (DOW0+ExDays)>=3 | DOW0>3 & (DOW0+ExDays)>=10,1,0),0))%>%
    mutate(We = ifelse(grepl("4", DOW_LST, fixed=TRUE),
                       FWeeks+ifelse(DOW0<4 & (DOW0+ExDays)>=4 | DOW0>4 & (DOW0+ExDays)>=11,1,0),0))%>%
    mutate(Th = ifelse(grepl("5", DOW_LST, fixed=TRUE),
                       FWeeks+ifelse(DOW0<5 & (DOW0+ExDays)>=5 | DOW0>5 & (DOW0+ExDays)>=12,1,0),0))%>%
    mutate(Fr = ifelse(grepl("6", DOW_LST, fixed=TRUE),
                       FWeeks+ifelse(DOW0<6 & (DOW0+ExDays)>=6 | DOW0>6 & (DOW0+ExDays)>=13,1,0),0))%>%
    mutate(Sa = ifelse(grepl("7", DOW_LST, fixed=TRUE),
                       FWeeks+ifelse(DOW0<7 & (DOW0+ExDays)>=7,1,0),0))%>%
    mutate(nDAYS = Su+Mo+Tu+We+Th+Fr+Sa) %>%
    mutate(STRAT_DAYS = nDAYS+m025)

  #Standardize the field order for STRATDAYS
  STRATDAYS <- STRATDAYS %>%
    arrange(SSN, DTP)%>%
    rename(sFN025 = m025)%>%
    select(PRJ_CD, SSN, DTP, DOW_LST, SSN_DATE0, FWeek1, SSN_DATE1,
           DOW0, Days, FWeeks, ExDays, Su, Mo, Tu, We, Th, Fr, Sa,
           nDAYS, sFN025, STRAT_DAYS)

  #Return the datasets
  assign("EXCEPTDAYS", EXCEPTDAYS, envir = .GlobalEnv)
  assign("STRATDAYS", STRATDAYS, envir = .GlobalEnv)
  print("The datasets EXCEPTDAYS and STRATDAYS have been created successfully.")
}
