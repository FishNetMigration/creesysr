#' make_FR713_table
#'
#' @param fr713 the output of the make_FR713 function
#' @param fr714 the output of the make_FR714 function
#' @param Table the effort table will print if yes
#' @param EffortHarvest both the effort and harvest table will print if yes
#'
#' @description
#' This is the creel estimate table for EFFORT
#'
#'
#' @return a table of the creel estimates
#' @export
#'
#' @examples
#'
make_FR713_table<-function(fr713){

  # insert the output of the make_FR713

  RFR713_KblTable<- fr713 %>% dplyr::select(
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
  FR713_table<-kbl(RFR713_KblTable, caption = "Total Effort FR713 Summary",
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
    kable_classic(full_width = F, html_font = "Cambria") %>%
    add_header_above(c("Project & Area" = 2,
                       "Estimated Effort" = 2,
                       "Estimated Activity" = 3,
                       "Details" = 5))


  FR713_table


  }
