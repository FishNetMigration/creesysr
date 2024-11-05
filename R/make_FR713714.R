#' make_FR713714
#'
#' @param fn011 FN project definition table FN011 which lists project dates, year, lake name and project leader information
#' @param fn111 FN portion of the CLF that outlines the SAMA, Date, DOW, STRATUM,
#' @param fn112 FN portion of the CLF that outlines the activity count, time of the count and number of interviews that occurred. Note: multiple counts could occur per SAMA
#' @param fn121 FN CIF creel interview form
#' @param fn123 FN species records part of the CIF form but nested by SAM to the FN121
#' @param fn022 FN strata definition file for SSN
#' @param fn023 FN strata definition file for DTP i.e. workday and weekend
#' @param fn024 FN stratum definition for period i.e. am or pm
#' @param fn025 FN stratum definition listing the exception days i.e. holidays that need to be re-coded as weekends
#' @param fn026 FN stratum definition for the SPACE
#' @param fn028 FN stratum definition for the MODE
#' @param fileloc FN stratum definition for the MODE
#'
#' @description
#' This function generates the estimates and outputs a table for EFFORT and HARVEST
#'
#'
#' @return
#' @export
#'
#' @examples
#'
make_FR713714<-function(fn011, fn022, fn023, fn024, fn025, fn026, fn028, fn111, fn112, fn121, fn123, fileloc){



  # create daily estimate FR713_daily

  cSPACE<-fn026 %>% group_by(PRJ_CD) %>% dplyr::summarize(cSPACE=n())
  cMODE<-fn028 %>% group_by(PRJ_CD) %>% dplyr::summarize(cMODE=n())


  CreelData111_112<-left_join(fn111, fn112, by=c("SAMA", "PRJ_CD"))




  FN121_mutate<-fn121 %>% data.frame() %>%  mutate(SAMA=as.numeric(SAMA),
                                                   EFFTM1=parse_hm(EFFTM1),
                                                   EFFTM0=parse_hm(EFFTM0),
                                                   ITVTM0= parse_hm(ITVTM0),
                                                   PERSONS_hik=as.numeric(PERSONS),
                                                   ANGLERS_hik=as.numeric(ANGLERS),
                                                   RODS_hik=as.numeric(RODS)) %>% data.frame()

  CreelData111_112<-CreelData111_112 %>% mutate(ATYCNT=as.numeric(ATYCNT))
  CreelData111_112<-CreelData111_112 %>% mutate(ITVCNT=as.numeric(ITVCNT))
  CreelData111_112<-CreelData111_112 %>% mutate(SAMA=as.numeric(SAMA))

  all<-CreelData111_112 %>% dplyr::select(SAMA, STRATUM, PRJ_CD, DATE, AREA, ITVCNT, ATYCNT) %>%
    mutate(ITVCNT_hi=as.numeric(ITVCNT),
           ATYCNT_hi=as.numeric(ATYCNT))


  FN121<- left_join(all, FN121_mutate) %>% # this join needs to be done in case on a certain date there are 0 interviews
    mutate(
      # Victoria test check this
      # EFFORT -----------------------------------------------------------------------
      # the estimate of daily effort, uses information from the CIF such as :
      #- number of anglers (ANGLERS)
      #- number of rods (RODS)

      # a- Fishing start time
      # b- Interview time
      # (where a and b are used to calculate EFFDUR which is the duration of the fishing trip)
      # note effort is estimated in various units (rod-hours, angler-hours and party-hours)

      # EFFDUR=ifelse(EFFCMP==1, difftime(EFFTM1, EFFTM0, units="hours"), difftime(ITVTM0, EFFTM0, units="hours")),
      EFFDUR=as.numeric(EFFDUR),
      EFFDUR=ifelse(is.na(EFFDUR), 0, EFFDUR),
      EFFDUR=ifelse(is.na(EFFDUR), 0, EFFDUR),
      EFFDUR=ifelse(is.na(EFFDUR), 0, EFFDUR),
      RODS_hik=ifelse(is.na(RODS_hik), 0, RODS_hik),
      PERSONS_hik=ifelse(is.na(PERSONS_hik), 0, PERSONS_hik),
      ANGLERS_hik=ifelse(is.na(ANGLERS_hik), 0, ANGLERS_hik),
      angler_hours=ANGLERS_hik*EFFDUR,
      rod_hours=RODS_hik*EFFDUR,

      SSN = substr(STRATUM, 1, 2), # season (usually season 1, 2, 3)
      DTP = substr(STRATUM, 4, 4), # day type (Weekday or Weekend)
      PRD = substr(STRATUM, 5, 5), # period (AM vs PM)
      AREA = substr(STRATUM, 7, 8), # sectors
      MODE = substr(STRATUM, 10, 11) # mode for open water is always just a single mode...
    ) %>%
    ungroup()




  CreelData_FN024<-fn024 %>% mutate(SSN=as.numeric(SSN),
                                    DTP=as.numeric(DTP),
                                    PRD=as.numeric(PRD),
                                    PRD_DUR=abs(as.numeric(PRD_DUR)))


  # the prefix R- indicates "roving"

  R_FR713_Daily<-FN121 %>%
    mutate(SSN=as.numeric(SSN),
           DTP=as.numeric(DTP),
           PRD=as.numeric(PRD)) %>% # daily sum of people and effort
    left_join(., CreelData_FN024, by=c("PRJ_CD", "SSN", "DTP", "PRD")) %>%
    group_by(PRJ_CD, STRATUM, SAMA) %>%
    dplyr::summarize(
      SAMA=first(SAMA),
      STRATUM=first(STRATUM),
      SSN=first(SSN),
      DTP=first(DTP),
      PRD=first(PRD),
      PRD_DUR=first(PRD_DUR),
      PF_hi = 1, # NOTE: revisit this as this can be more complicated PF = what proportion of the recorded activity is due to fishing. The field would need to change to a calculation if the CHKCNT (check count) field was used ... i.e. winter creels - counting huts and doing checks on active or not. If check counts are not done, PF=1 (always summer creels PF=1)
      ITVCNT_hi=first(ITVCNT_hi),
      ATYCNT_hi=first(ATYCNT_hi),

      # note: adjustment is needed if persons does not equal anglers

      ANGLERS_hi=sum(ANGLERS_hik, na.rm=TRUE),
      PERSONS_hi=sum(PERSONS_hik, na.rm=TRUE),
      RODS_hi=sum(RODS_hik, na.rm=TRUE),
      EFFAO_hi=sum(angler_hours, na.rm=TRUE),
      EFFRO_hi=sum(rod_hours, na.rm=TRUE)) %>%

    mutate(ATY2_hi = ATYCNT_hi, # Activity unit =2 when activity counts refer to groups (boat, hut, etc)
           # ATY_hi is MEAN ACTIVITY PER STRATUM SAMPLE DAY
           # ATY2_hi: IIf([PF_hi] Is Null,1,[PF_hi])*IIf([ATYUNIT]="2",[ATYCNT],[ATYCNT]*[ITVCNT]/[PERSONS_hi])

           # if the activity unit is "person" then the formula to convert Person activity (ATY1) to group activity (ATY2) is used:
           # > ATY2_hi = ATY1_hi * CIF_NN_hi / sum(PERSONS_hik)
           # CIF_NN is the number of interviews
           # PERSONS is the number of persons in each interview


           EFFPE_hi = ATY2_hi*PRD_DUR, #EFFPE_hi: [ATY2_hi]*[PRD_DUR]
           # Estimated Party Effort (EFFPE)-------------------------------------------------
           # if the activity unit is "party" (boats, ice-huts), it is estimated as:
           # > EFFPE_hi = ATY2_hi * PRD_DUR_h # this is the estimated party effort (time spent on the lake i.e. the sum of trip duration)


           EFFAE_hi = ifelse(ITVCNT_hi == 0, 0, EFFPE_hi*ANGLERS_hi/ITVCNT_hi),
           # Estimated Angler Effort (EFFAE) above ----------------------------------------
           # the estimate of effort in party-hours is multiplied by the mean anglers per interview to get an estimate of angler-effort (EFFAE)
           # > EFFAE_hi = EFFPE_hi * (sum(ANGLERS_hik)/CIF_NN_hi)

           EFFRE_hi = ifelse(ITVCNT_hi == 0, 0, EFFPE_hi*RODS_hi/ITVCNT_hi))%>%
    ungroup() %>%
    # and Estimated angler-effort in rod-hours (EFFRE):
    # > EFFRE_hi = EFFPE_hi * (sum(RODS_hik)/CIF_NN_hi)
    mutate( SSN = as.numeric(substr(STRATUM, 1, 2)),
            DTP = as.numeric(substr(STRATUM, 4, 4)), # day type (Weekday or Weekend)
            PRD = as.numeric(substr(STRATUM, 5, 5)), # period (AM vs PM)
            AREA = as.numeric(substr(STRATUM, 7, 8)), # sectors
            MODE = as.numeric(substr(STRATUM, 10, 11))
    ) %>%


    arrange(PRJ_CD, STRATUM, SAMA)


  # Get Strata Days function ===================================================

  library(dplyr)
  # library(tidyr)
  get_EXCEPTDAYS_STRATDAYS <- function(fn022 = FN022, fn023 = FN023, fn025 = FN025) {

    #Restrict data to only required fields
    fn022 <- fn022 %>% dplyr::select(PRJ_CD, SSN, SSN_DATE0, SSN_DATE1)
    fn023 <- fn023 %>% dplyr::select(PRJ_CD, SSN, DTP, DOW_LST)
    fn025 <- fn025 %>% dplyr::select(PRJ_CD, DATE, DTP1)

    #Validation to check for gaps in SSN strata
    VAL <- fn022 %>%
      dplyr::mutate(SSN_DATE0 = as.Date(SSN_DATE0, format = "%Y-%m-%d"))%>%
      dplyr::mutate(SSN_DATE1 = as.Date(SSN_DATE1, format = "%Y-%m-%d"))%>%
      dplyr::arrange(SSN_DATE0)%>%
      dplyr::mutate(SSN_CHECK = ifelse((SSN_DATE1 + 1) != lead(SSN_DATE0), "ISSUE", "OK"))%>%
      dplyr::mutate(SSN_CHECK = ifelse(is.na(SSN_CHECK), "OK", SSN_CHECK))

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
      dplyr::mutate(SSN_DATE0 = as.Date(SSN_DATE0, format = "%Y-%m-%d"))%>%
      dplyr::mutate(SSN_DATE1 = as.Date(SSN_DATE1, format = "%Y-%m-%d"))%>%
      dplyr::mutate(DATE = as.Date(DATE, format = "%Y-%m-%d"))%>%
      dplyr::mutate(m025 = ifelse(DATE >= SSN_DATE0 & DATE <= SSN_DATE1, ifelse(DTP1 == DTP, 1, -1), 0))%>%
      dplyr::mutate(m025 = ifelse(is.na(DATE), 0, m025))%>%
      dplyr::mutate(DTP1 = ifelse(is.na(DTP1), 2, DTP1))%>%
      group_by(PRJ_CD, SSN, SSN_DATE0, SSN_DATE1, DTP, DTP1, DOW_LST)%>%
      dplyr::summarise(m025 = sum(m025), .groups = "drop")%>%
      dplyr::select(PRJ_CD, SSN, SSN_DATE0, SSN_DATE1, DTP, DTP1, DOW_LST, m025)

    #Calculate the number of STRAT_DAYS
    STRATDAYS <- EXCEPTDAYS %>%
      dplyr::mutate(Days = as.numeric((SSN_DATE1-SSN_DATE0)+1))%>%
      dplyr::mutate(FWeeks = round(Days/7,0))%>%
      dplyr::mutate(ExDays = Days-FWeeks*7)%>%
      dplyr::mutate(FWeek1 = SSN_DATE1-ExDays)%>%
      dplyr::mutate(DOW0 = as.numeric(format(FWeek1,'%w'))+1)%>%
      dplyr::mutate(Su = ifelse(grepl("1", DOW_LST, fixed=TRUE),
                                FWeeks+ifelse((DOW0+ExDays)>7,1,0),0))%>%
      dplyr::mutate(Mo = ifelse(grepl("2", DOW_LST, fixed=TRUE),
                                FWeeks+ifelse(DOW0<2 & (DOW0+ExDays)>=2 | DOW0>2 & (DOW0+ExDays)>=9,1,0),0))%>%
      dplyr::mutate(Tu = ifelse(grepl("3", DOW_LST, fixed=TRUE),
                                FWeeks+ifelse(DOW0<3 & (DOW0+ExDays)>=3 | DOW0>3 & (DOW0+ExDays)>=10,1,0),0))%>%
      dplyr::mutate(We = ifelse(grepl("4", DOW_LST, fixed=TRUE),
                                FWeeks+ifelse(DOW0<4 & (DOW0+ExDays)>=4 | DOW0>4 & (DOW0+ExDays)>=11,1,0),0))%>%
      dplyr::mutate(Th = ifelse(grepl("5", DOW_LST, fixed=TRUE),
                                FWeeks+ifelse(DOW0<5 & (DOW0+ExDays)>=5 | DOW0>5 & (DOW0+ExDays)>=12,1,0),0))%>%
      dplyr::mutate(Fr = ifelse(grepl("6", DOW_LST, fixed=TRUE),
                                FWeeks+ifelse(DOW0<6 & (DOW0+ExDays)>=6 | DOW0>6 & (DOW0+ExDays)>=13,1,0),0))%>%
      dplyr::mutate(Sa = ifelse(grepl("7", DOW_LST, fixed=TRUE),
                                FWeeks+ifelse(DOW0<7 & (DOW0+ExDays)>=7,1,0),0))%>%
      dplyr::mutate(nDAYS = Su+Mo+Tu+We+Th+Fr+Sa) %>%
      dplyr::mutate(STRAT_DAYS = nDAYS+m025)

    #Standardize the field order for STRATDAYS
    STRATDAYS <- STRATDAYS %>%
      dplyr::arrange(SSN, DTP)%>%
      dplyr::rename(sFN025 = m025)%>%
      dplyr::select(PRJ_CD, SSN, DTP, DOW_LST, SSN_DATE0, FWeek1, SSN_DATE1,
                    DOW0, Days, FWeeks, ExDays, Su, Mo, Tu, We, Th, Fr, Sa,
                    nDAYS, sFN025, STRAT_DAYS)

    #Return the datasets
    assign("EXCEPTDAYS", EXCEPTDAYS, envir = .GlobalEnv)
    assign("STRATDAYS", STRATDAYS, envir = .GlobalEnv)
    print("The datasets EXCEPTDAYS and STRATDAYS have been created successfully.")
  }

  #===============================================================================


  STRATDAYS_1<-data.frame()

  PRJ_CD_list<-unique(fn011$PRJ_CD)

  for (i in PRJ_CD_list) {

    FN022<-fn022 %>% filter(PRJ_CD==i)
    FN023<-fn023 %>% filter(PRJ_CD==i)
    FN025<-fn025 %>% filter(PRJ_CD==i)

    get_EXCEPTDAYS_STRATDAYS(FN022, FN023, FN025)

    STRATDAYS_1<-rbind(STRATDAYS, STRATDAYS_1)

  }



  # STRATUM FR713 ----------------------------------------------------------------
  STRATDAYS_1<-STRATDAYS_1 %>% mutate(SSN=as.numeric(SSN), DTP=as.numeric(DTP)) # eventually change back to factors

  # the means of the daily estimates within each stratum are multiplied by the stratum size (# days)
  R_FR713_Stratum<-R_FR713_Daily %>%
    left_join(., STRATDAYS_1, by=c("SSN", "DTP", "PRJ_CD")) %>%
    mutate(STRAT_HRS_h = PRD_DUR*STRAT_DAYS) %>%
    group_by(PRJ_CD, STRATUM) %>%
    dplyr::summarize(
      STRAT_DAYS_s = first(STRAT_DAYS),
      PRD_DUR = first(PRD_DUR), # length of period (hours)
      SAM_DAYS_h = length(unique(SAMA)), # number of sample days
      ATYCNT_h = sum(ATYCNT_hi, na.rm=TRUE), # total activity (sum)
      ATY_NN_h = n(), # number of activity counts
      ITVCNT_h = sum(ITVCNT_hi, na.rm=TRUE), # total interviews (sum)
      PERSONS_h = sum(PERSONS_hi, na.rm=TRUE),
      ANGLERS_h = sum(ANGLERS_hi, na.rm=TRUE),
      RODS_h = sum(RODS_hi, na.rm=TRUE),
      trip_a = sum(EFFAE_hi), # sum of angler hours per strata
      ATY2_h = mean(ATY2_hi),
      EFFPE_h = mean(EFFPE_hi)*STRAT_DAYS_s,
      EFFAO_h = sum(EFFAO_hi, na.rm=TRUE),
      EFFAE_h = mean(EFFAE_hi)*STRAT_DAYS_s,
      EFFRO_h = sum(EFFRO_hi, na.rm=TRUE),
      EFFRE_h = mean(EFFRE_hi)*STRAT_DAYS_s,
      ATY2_VR_h = var(ATY2_hi)/n_distinct(SAMA),

      EFFPE_VR_h = STRAT_DAYS_s^2*(var(EFFPE_hi)/n_distinct(SAMA)),
      EFFAE_VR_h = STRAT_DAYS_s^2*(var(EFFAE_hi)/n_distinct(SAMA)),
      EFFRE_VR_h = STRAT_DAYS_s^2*(var(EFFRE_hi)/n_distinct(SAMA))) %>%

    mutate(
      STRAT_HRS_h = STRAT_DAYS_s*PRD_DUR,
      ANGLERS_MN_h = ANGLERS_h/ITVCNT_h,
      RODS_MNA_h = RODS_h/ANGLERS_h,
      TRIPNE_h = ifelse(EFFAE_h>0, trip_a*ITVCNT_h/(2*EFFAO_h), NA), #TRIPNE_h: IIf([EFFAO_h]>0,Sum([EFFAE_hi])*[ITVCNT_h]/(2*[EFFAO_h]),Null)
      ATY2_SE_h =  sqrt(ATY2_VR_h),
      EFFPE_SE_h = sqrt(EFFPE_VR_h),
      EFFAE_SE_h = sqrt(EFFAE_VR_h),
      EFFRE_SE_h = sqrt(EFFRE_VR_h)
    )


  R_FR713_ProjPart<-R_FR713_Stratum %>% data.frame() %>%
    mutate(
      SSN = as.numeric(substr(STRATUM, 1, 2)),
      DTP = as.numeric(substr(STRATUM, 4, 4)), # day type (Weekday or Weekend)
      PRD = as.numeric(substr(STRATUM, 5, 5)), # period (AM vs PM)
      AREA = as.numeric(substr(STRATUM, 7, 8)), # sectors
      MODE = as.numeric(substr(STRATUM, 10, 11))
    ) %>%

    left_join(., cSPACE, by=c("PRJ_CD")) %>%
    left_join(., cMODE, by=c("PRJ_CD")) %>%

    group_by(PRJ_CD, AREA) %>%
    dplyr::summarize( SAM_DAYS=sum(SAM_DAYS_h, na.rm=TRUE),
                      STRAT_HRS=sum(as.numeric(STRAT_HRS_h))/first(cSPACE)/first(cMODE),
                      #STRAT_HRS: Sum([STRAT_HRS_h])/IIf([Forms]![R Creel Estimates]![cbo_SPACE] Is Null,First([cSPACE]),1)/IIf([Forms]![R Creel Estimates]![cbo_MODE] Is Null,First([cMODE]),1)
                      ATY_NN=sum(ATY_NN_h, na.rm=TRUE),
                      ATYCNT=sum(ATYCNT_h, na.rm=TRUE),
                      ITVCNT=sum(ITVCNT_h, na.rm=TRUE),
                      PERSONS=sum(PERSONS_h, na.rm=TRUE),
                      ANGLERS=sum(ANGLERS_h, na.rm=TRUE),
                      RODS=sum(RODS_h, na.rm=TRUE),

                      TRIPNE=sum(TRIPNE_h, na.rm=TRUE),
                      EFFPE=sum(EFFPE_h, na.rm=TRUE),
                      EFFAO=sum(EFFAO_h, na.rm=TRUE),
                      EFFAE=sum(EFFAE_h, na.rm=TRUE),
                      EFFRO=sum(EFFRO_h, na.rm=TRUE),

                      EFFRE=sum(EFFRE_h, na.rm=TRUE),
                      EFFPE_VR=sum(EFFPE_VR_h, na.rm=TRUE),
                      EFFAE_VR=sum(EFFAE_VR_h, na.rm=TRUE),
                      EFFRE_VR=sum(EFFRE_VR_h, na.rm=TRUE),
                      ATY2_VR=sum(ATY2_VR_h, na.rm=TRUE)) %>%

    mutate(
      ANGLERS_MN=ANGLERS/ITVCNT,
      RODS_MN=RODS/ANGLERS,
      ATY2=EFFPE/STRAT_HRS,

      EFFPE_SE=sqrt(EFFPE_VR),
      EFFAE_SE=sqrt(EFFAE_VR),
      EFFRE_SE=sqrt(EFFRE_VR),

      EFFRE_SE_percent=EFFRE_SE/EFFRE*100,
      ATY2_SE=sqrt(ATY2_VR),
      ATY2_SE_percent=ATY2_SE/ATY2*100)


  R_FR713_ProjPart<-R_FR713_ProjPart %>% mutate(Year_Sectors=sub(".*SC", "", PRJ_CD)) %>% mutate(Year=sub("_.*", "", Year_Sectors)) %>% mutate(YR=ifelse(Year<70, as.numeric(Year)+2000, as.numeric(Year)+1900))



  R_FR713_ProjPart

  #===================================================================== harvest estimate

  fn121 <-fn121 %>%
    mutate(SAMA=as.numeric(SAMA))

  R_FR714a<-
    left_join(CreelData111_112, fn121, by=c("PRJ_CD", "STRATUM", "SAMA", "DATE", "AREA", "MODE" )) %>%
    mutate(ATYCNT=as.numeric(ATYCNT),
           ATYCNT=ifelse(is.na(ATYCNT), 0, ATYCNT),
           ITVCNT=as.numeric(ITVCNT),
           ITVCNT=ifelse(is.na(ITVCNT), 0, ITVCNT)) %>%
    left_join(., fn123, by=c("PRJ_CD",  "SAM")) # join with the catch info

  R_FR714b<-R_FR714a  %>%
    mutate(
      SAMA=as.numeric(SAMA),
      EFFTM1=parse_hm(EFFTM1),
      EFFTM0=parse_hm(EFFTM0),
      ITVTM0= parse_hm(ITVTM0),

      HVSCNT=as.numeric(HVSCNT),
      HVSCNT=ifelse(is.na(HVSCNT), 0, HVSCNT),

      RLSCNT=as.numeric(RLSCNT),
      RLSCNT=ifelse(is.na(RLSCNT), 0, RLSCNT),

      ANGLERS=as.numeric(ANGLERS),
      PERSONS=as.numeric(PERSONS),
      RODS=as.numeric(RODS),

      # EFFDUR=ifelse(EFFCMP==1, difftime(EFFTM1, EFFTM0, units="hours"), difftime(ITVTM0, EFFTM0, units="hours")),
      EFFDUR=round(as.numeric(EFFDUR), 2), #FN2 already had a EFFDUR and some were EFFDURC =2 so the time was added in manually in FN2 and not calculated
      EFFDUR=ifelse(is.na(EFFDUR), 0, EFFDUR), # do we have to change interviews with 0 effort duration to a value?

      EFFAO1_hik=ifelse(SEK==1, (EFFDUR*ANGLERS), 0), # SOUGHT (YES = 1), assigning 0 if the species was not sought
      EFFRO1_hik=ifelse(SEK==1, (EFFDUR*RODS), 0),

      EFFAO_hik=round(EFFDUR*ANGLERS, 0), # non-target/all
      EFFRO_hik=round(EFFDUR*RODS, 0), # non-target/all

      CATNO_hik= (HVSCNT+RLSCNT), # CATNO_hik # number of fish caught by a party
      HVSNO_hik= HVSCNT, # HVSNO_hik # number of fish harvested

      CATNO1_hik=ifelse(SEK==1, (HVSCNT+RLSCNT), 0),
      HVSNO1_hik=ifelse(SEK==1, (HVSCNT), 0)) %>%


    group_by(PRJ_CD, STRATUM, SAMA , SPC) %>%
    dplyr::summarize(

      CATNO_hi=sum(CATNO_hik, na.rm=TRUE), # all catch (target + plus non targeted)
      HVSNO_hi=sum(HVSNO_hik, na.rm=TRUE), # all harvest(target + plus non targeted)

      CATNO1_hi=sum(CATNO1_hik, na.rm=TRUE), # targeted catch (target)
      HVSNO1_hi=sum(HVSNO1_hik, na.rm=TRUE), # targeted harvest (target)

      EFFAO1_hi=sum(EFFAO1_hik, na.rm=TRUE), # targeted angler hours
      EFFRO1_hi=sum(EFFRO1_hik, na.rm=TRUE) # targeted rod hours


    ) %>%
    left_join (R_FR713_Daily, ., by=c("PRJ_CD", "STRATUM", "SAMA")) %>%

    mutate(EFFAE1_hi=ifelse(EFFAO_hi==0, 0, (EFFAE_hi*EFFAO1_hi/EFFAO_hi)), # angler effort (sought) total times fraction targeted
           EFFRE1_hi=ifelse(EFFRO_hi==0, 0, (EFFRE_hi*EFFRO1_hi/EFFRO_hi)), # rod effort (sought) total times fraction targeted

           CATNE_hi=ifelse(EFFRO_hi==0, 0, (EFFRE_hi*CATNO_hi/EFFRO_hi)), # uses rod hours as a base unit....
           CATNE_hi=round(CATNE_hi, 1),
           CATNE1_hi=ifelse(EFFRO1_hi==0, 0, (EFFRE1_hi*CATNO1_hi/EFFRO1_hi)),
           CATNE1_hi= round(CATNE1_hi, 1),
           # CATNE # stratum (h) sample day (i) estimates of number caught are calculated as a product of effort * CUE
           # this estimate can be in rod-hours, angler-hours or party-hours as the base effort unit
           # the original CREESYS used rod-hours

           # (need to check the project definition file to see what the FR71_Unit is)
           # > CATNE_hi = EFFRE_hi * (CATNO_hi/EFFRO_hi) # ROD-HOURS AS BASE UNIT
           # FR71_UNIT ='0' rod-hours

           # > CATNE_hi = EFFAE_hi * (CATNO_hi/EFFAO_hi) # ANGLER-HOURS AS BASE UNIT
           # FR71_UNIT = '1' angler-hours

           # > CATNE_hi = EFFPE_hi * (CATNO_hi/EFFDUR_hi) # party-HOURS AS BASE UNIT
           # FR71_UNIT = '2' party-hours as the base unit

           HVSNE_hi= ifelse(EFFRO_hi==0, 0, (EFFRE_hi*HVSNO_hi/EFFRO_hi)),
           #HVSNE_hi= round(HVSNE_hi, 0),
           HVSNE1_hi= ifelse(EFFRO1_hi==0, 0, (EFFRE1_hi*HVSNO1_hi/EFFRO1_hi)), # HVSNE_hi: IIf(First([EFFRO_hi])=0,0,First([EFFRE_hi])*[HVSNO_hi]/First([EFFRO_hi]))
           HVSNE1_hi= round(HVSNE1_hi, 0)) %>% filter(!is.na(SPC))

  # >>> REPEAT FOR HARVEST -------------------------------------------------------

  # > HVSNE # daily estimates of number harvested are calculated as a product of effort * CUE
  # this estimate can be in rod-hours, angler-hours or party-hours as the base effort unit
  # CREESYS used rod-hours

  # > HVSNE_hi = EFFRE_hi * (sum(HVSNO_hik)/ sum(EFFRO_hik)) # ROD-HOURS AS BASE UNIT
  # FR71_UNIT ='0' rod-hours

  # > HVSNE_hi = EFFAE_hi * (sum(HVSNO_hik)/ sum(EFFAO_hik)) # ANGLER-HOURS AS BASE UNIT
  # FR71_UNIT = '1' angler-hours

  # > HVSNE_hi = EFFPE_hi * (sum(HVSNO_hik)/ sum(EFFDUR_hik)) # ANGLER-HOURS AS BASE UNIT
  #-------------------------------------------------------------------------------



  # R_FR714 Avgs

  R_FR714_Avgsa<-R_FR714b %>%
    group_by(PRJ_CD, STRATUM, SPC) %>%
    dplyr::summarize(
      EFFRE1_MN_ha = sum(EFFRE1_hi),
      HVSNE_MN_ha= sum(HVSNE_hi),
      CATNE_MN_ha= sum(CATNE_hi))

  R_FR714_Avgs<- left_join(R_FR714_Avgsa, R_FR713_Stratum, by=c("PRJ_CD", "STRATUM")) %>%
    mutate(EFFRE1_MN_h = EFFRE1_MN_ha/SAM_DAYS_h,
           HVSNE_MN_h= HVSNE_MN_ha/SAM_DAYS_h,
           CATNE_MN_h= CATNE_MN_ha/SAM_DAYS_h)

  R_FR714_Avgs<-R_FR714_Avgs %>% dplyr::select(PRJ_CD, STRATUM, SPC, SAM_DAYS_h,  EFFRE1_MN_h, HVSNE_MN_h, CATNE_MN_h)


  # R_FR714 Stratum


  R_FR713_Stratum_a<-R_FR713_Stratum %>% dplyr::select(PRJ_CD, STRATUM, STRAT_DAYS_s,  EFFAO_h, EFFAE_h, EFFRO_h, EFFRE_h)

  R_FR714_Stratum<-left_join(R_FR713_Stratum_a, R_FR714b, by=c("PRJ_CD", "STRATUM")) %>%
    left_join(., R_FR714_Avgs, by=c("PRJ_CD", "STRATUM", "SPC")) %>%
    group_by(PRJ_CD, STRATUM, SPC) %>%
    dplyr::summarize(
      SAM_DAYS_h=first(SAM_DAYS_h),
      STRAT_DAYS=first(STRAT_DAYS_s),
      EFFAO_h=first(EFFAO_h),
      EFFAE_h=first(EFFAE_h),
      EFFRO_h=first(EFFRO_h),
      EFFRE_h=first(EFFRE_h),
      HVSNE_MN_h=first(HVSNE_MN_h),

      CATNO_h=sum(CATNO_hi, na.rm=TRUE),

      CATNE_ha= sum(CATNE_hi, na.rm=TRUE),
      CATNE_ha= round(CATNE_ha, 0),

      HVSNO_h=sum(HVSNO_hi, na.rm=TRUE),
      HVSNE_ha= sum(HVSNE_hi, na.rm=TRUE),
      HVSNE_ha= round(HVSNE_ha, 0),

      # Variance------------------------------------------------------------------
      HVSNE_VR_h= ifelse(SAM_DAYS_h<2, NA, (STRAT_DAYS^2))*(sum((HVSNE_hi-HVSNE_MN_h)^2)+(SAM_DAYS_h-n())*first(HVSNE_MN_h^2))/(SAM_DAYS_h-1)/SAM_DAYS_h,
      # IIf([SAM_DAYS_h]<2,Null,[STRAT_DAYS]^2*(Sum(([HVSNE_hi]-[HVSNE_MN_h])^2)+([SAM_DAYS_h]-Count([R FR714].[Year]))*First([HVSNE_MN_h])^2)/([SAM_DAYS_h]-1)/[SAM_DAYS_h]) AS HVSNE_VR_h,


      CATNO1_h= sum(CATNO1_hi, na.rm=TRUE),
      CATNE1_ha= sum(CATNE1_hi, na.rm=TRUE),
      CATNE1_ha= round(CATNE1_ha, 0),

      # Variance -------------------------------------------------------------------
      CATNE_VR_h= ifelse(SAM_DAYS_h<2, NA, (STRAT_DAYS^2))*(sum((CATNE_hi-CATNE_MN_h)^2)+(SAM_DAYS_h-n())*first(CATNE_MN_h^2))/(SAM_DAYS_h-1)/SAM_DAYS_h,

      # IIf([SAM_DAYS_h]<2,Null,[STRAT_DAYS]^2*(Sum(([CATNE_hi]-[CATNE_MN_h])^2)+([SAM_DAYS_h]-Count([R FR714].[Year]))*First([CATNE_MN_h])^2)/([SAM_DAYS_h]-1)/[SAM_DAYS_h]) AS CATNE_VR_h,

      EFFAO1_h= sum(EFFAO1_hi, na.rm=TRUE),
      EFFAE1_ha= sum(EFFAE1_hi, na.rm=TRUE),  #(sum(EFFAE1_hi)/SAM_DAYS_h*STRAT_DAYS),
      EFFAE1_ha= round(EFFAE1_ha, 0),

      EFFRO1_h= sum(EFFRO1_hi, na.rm=TRUE),
      EFFRE1_ha= sum(EFFRE1_hi, na.rm=TRUE), #(sum(EFFRE1_hi)/SAM_DAYS_h*STRAT_DAYS) ,
      EFFRE1_ha= round(EFFRE1_ha, 0),

      # Variance -----------------------------------------------------------------
      EFFRE1_VR_h=ifelse(SAM_DAYS_h<2, NA, (STRAT_DAYS^2))*(sum((EFFRE1_hi-EFFRE1_MN_h)^2)+(SAM_DAYS_h-n())*first(EFFRE1_MN_h^2))/(SAM_DAYS_h-1)/SAM_DAYS_h

      # IIf([SAM_DAYS_h]<2,Null,[STRAT_DAYS]^2*(Sum(([EFFRE1_hi]-[EFFRE1_MN_h])^2)+([SAM_DAYS_h]-Count([R FR714].[Year]))*First([EFFRE1_MN_h])^2)/([SAM_DAYS_h]-1)/[SAM_DAYS_h]) AS EFFRE1_VR_h,
    ) %>%
    mutate(
      CATNE_h= round(CATNE_ha/SAM_DAYS_h*STRAT_DAYS, 0),
      HVSNE_h= round(HVSNE_ha/SAM_DAYS_h*STRAT_DAYS, 0),
      CATNE1_h= round(CATNE1_ha/SAM_DAYS_h*STRAT_DAYS, 0),

      CATNE1_PC_h= ifelse(CATNE_h>0, ((CATNE1_h/CATNE_h)*100), NA),

      HVSCAT_PC_h= ifelse(CATNE_h>0, ((HVSNE_h/CATNE_h)*100), NA) ,

      EFFAE1_h= round(EFFAE1_ha/SAM_DAYS_h*STRAT_DAYS, 0),
      EFFRE1_h= round(EFFRE1_ha/SAM_DAYS_h*STRAT_DAYS, 0),

      EFFRE1_PC_h=(EFFRE1_h/EFFRE_h*100),
      CUENAO_h= (CATNO_h/EFFRO_h),
      CUENAO1_h= ifelse(EFFRO1_h>0, CATNO1_h/EFFRO1_h, 0), # has to have some targeted effort h
      CUENAE_h= CATNE_h/EFFRE_h,
      CUENAE1_h=  ifelse(EFFRE1_h>0, CATNE1_h/EFFRE1_h, 0),

      SSN = substr(STRATUM, 1, 2),
      DTP = substr(STRATUM, 4, 4),
      PRD = substr(STRATUM, 5, 5),
      AREA = as.numeric(substr(STRATUM, 7, 8)),
      MODE = substr(STRATUM, 10, 11)
    )


  R_FR714_ProjPart <- R_FR714_Stratum %>%
    group_by(PRJ_CD, AREA, SPC) %>%
    dplyr::summarise(
      EFFRO1=sum(EFFRO1_h, na.rm=TRUE), # observed targeted effort as rod-hours
      EFFAE1=sum(EFFAE1_h, na.rm=TRUE), # estimated targeted effort as angler-hours
      EFFRE1=sum(EFFRE1_h, na.rm=TRUE), # estimated targeted effort as rod-hours
      HVSNE=sum(HVSNE_h, na.rm=TRUE), # estimated harvest (sum)
      EFFRE1_VR=sum(EFFRE1_VR_h, na.rm=TRUE), # Effort variance
      HVSNE_VR = sum(HVSNE_VR_h, na.rm=TRUE),  # Harvest variance
      CATNO=sum(CATNO_h, na.rm=TRUE), # observed catch
      CATNE=sum(CATNE_h, na.rm=TRUE), # estimated catch
      CATNE_VR = sum(CATNE_VR_h, na.rm=TRUE),  # Catch variance
      CATNO1=sum(CATNO1_h, na.rm=TRUE), # observed targeted catch
      CATNE1=sum(CATNE1_h, na.rm=TRUE) # estimated targeted catch
    ) %>%
    mutate(
      EFFRE1_SE_percent=ifelse(EFFRE1>0, ((EFFRE1_VR)^0.5)/EFFRE1*100, NA),
      HVSNE_SE_Percent=ifelse(HVSNE>0, ((HVSNE_VR)^0.5)/HVSNE*100, NA),  # Percent RSE for harvest estimate
      CATNE_SE_Percent=ifelse(CATNE>0, ((CATNE_VR)^0.5)/CATNE*100, NA), # Percent RSE for catch estimate
      EFFRE1_SE_percent=ifelse(EFFRE1>0, ((EFFRE1_VR)^0.5)/EFFRE1*100, NA),
      CATNE1_PC=ifelse(CATNE>0, (CATNE1/CATNE)*100, NA), # Percent (%) of catch that was from targeted effort
      HVSCAT_PC=ifelse(CATNE1>0, (HVSNE/CATNE)*100, NA) # Percent (%) of the targeted catch that was kept
    ) %>%

    left_join(., R_FR713_ProjPart, by=c("PRJ_CD", "AREA")) %>%

    mutate(
      EFFRE1_PC=(EFFRE1/EFFRE)*100, # Percent (%) Targeted (Target/total) effort
      CUENAO=CATNO/EFFRO, # observed CUE all (rod-hours base effort)
      CUENAO1=ifelse(EFFRO1>0, CATNO1/EFFRO1, NA), # observed CUE targeted (rod-hours base effort)
      CUENAE=CATNE/EFFRE, # estimates CUE all
      CUENAE1=ifelse(EFFRE1>0, CATNE1/EFFRE1, NA) # targeted CUE targeted
    ) %>% ungroup()

R_FR714_ProjPart

################################################## tables

# insert the output of the make_FR713

RFR713_KblTable<- R_FR713_ProjPart %>% dplyr::select(
  AREA,
  PRJ_CD,
  # Estimated Effort----------------------------------
  EFFRE,
  EFFRE_SE_percent,
  # Estimated Activity -------------------------------
  ATY2,    #  --- Party
  ATY2_SE_percent,    # --- % RSE party
  TRIPNE,    # --- Number of trips

  # Details-------------------------------------------
  ATY_NN,
  ITVCNT, #---------Interview Forms
  EFFRO,  #---------Observed rod-hours
  ANGLERS_MN, #-----Anglers per party
  RODS_MN #-----Rods per party
)


RFR713_KblTable<-RFR713_KblTable %>%
  mutate(EFFRE=round(EFFRE, 0),
         EFFRE_SE_percent=round(EFFRE_SE_percent, 1),
         ATY2=round(ATY2, 2),
         ATY2_SE_percent=round(ATY2_SE_percent, 2),
         TRIPNE=round(TRIPNE, 0),
         EFFRO=round(EFFRO, 1),
         ANGLERS_MN=round(ANGLERS_MN, 2),
         RODS_MN=round(RODS_MN, 2)
  )

# kbl table for FR713 that matches Creesys 4.1 table
FR713_table<-kbl(format="html", RFR713_KblTable, caption = "Total Effort FR713 Summary",
                 col.names = c("AREA",
                               "Project:",

                               # Estimated Effort
                               "Rd-Hr",
                               "% RSE",
                               # Estimated Activity
                               "Party",
                               "% RSE",
                               "Trips (n)",
                               # Details
                               "Counts (n)",
                               "Interviews (n)",
                               "Observed rd-hrs",
                               "Anglers/Party",
                               "Rods/Angler"
                 ),

                 format.args = list(big.mark = ",")) %>%
  kable_classic(full_width = T, html_font = "Cambria") %>%
  add_header_above(c("Project & Area" = 2,
                     "Estimated Effort" = 2,
                     "Estimated Activity" = 3,
                     "Details" = 5))



#------------------------------ harvest FR714


FR714_table<-R_FR714_ProjPart %>%
  dplyr::select(
    SPC,
    EFFRE1,
    EFFRE1_SE_percent,
    EFFRE1_PC,

    HVSNE,
    HVSNE_SE_Percent,

    CATNO,
    CATNE,
    CATNE_SE_Percent,
    CATNE1_PC,
    HVSCAT_PC,

    CUENAO,
    CUENAO1,
    CUENAE,
    CUENAE1
  ) %>%
  mutate(
    EFFRE1_SE_percent=round(EFFRE1_SE_percent, 1),
    EFFRE1_PC=round(EFFRE1_PC, 0),

    HVSNE=round(HVSNE,0),
    HVSNE_SE_Percent=round(HVSNE_SE_Percent,1),

    CATNE=round(CATNE,0),
    CATNE_SE_Percent=round(CATNE_SE_Percent,1),
    CATNE1_PC=round(CATNE1_PC,0),
    HVSCAT_PC=round(HVSCAT_PC, 0),

    CUENAO=round(CUENAO,3),
    CUENAO1=round(CUENAO1, 3),
    CUENAE=round(CUENAE,3),
    CUENAE1=round(CUENAE1,3)
  ) %>%
  kbl(format="html", caption = "\n Targeted FR714 Summary",
      col.names = c("Species Code",
                    "Targeted (rd-hr)",
                    "% RSE",
                    "% Target",

                    "Harvest (n)",
                    "% RSE",

                    "Obs",
                    "Est",
                    "% RSE",
                    "% Target",
                    "% Kept",

                    "All",
                    "Targeted",
                    "All",
                    "Targeted"),
      format.args = list(big.mark = ",")) %>%
  kable_classic(full_width = T, html_font = "Cambria") %>%
  add_header_above(c(" " = 1, "Estimated Effort" = 3, "Harvest Count" = 2, "Catch Count" = 5, "Observed CUE"=2, "Estimated CUE"=2))


#---------------------------------------

require(styler)
library(reprex)
library(dplyr)
library(kableExtra)
library(knitr)
library(magick)


save_kable(FR713_table, file = "FR713_table.png")

par(mar=c(0,0,0,0))
png("space.png", width = 200, height = 200, units = "px")
plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
text(x = 0.5, y = 0.5, paste(' '), cex = 2, col = "black", family="serif", font=2, adj=0.5)
dev.off()


save_kable(FR714_table, file = "FR714_table.png")

image01 <- image_read("FR713_table.png")
space <-image_read("space.png")
space<-image_scale(space, "x50") # height: 50px
image02 <- image_read("FR714_table.png")

image <- c(image01,space, image02)
image_info(image)

image_stack <- image_append(image, stack = T)

image_write(image_stack, fileloc, format = "png")


}
