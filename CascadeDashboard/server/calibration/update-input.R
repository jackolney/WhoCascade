# Update MasterData upon hitting 'NEXT'
# Update bug happens if no value is already there to REPLACE.
# Need to run a check... that will rbind() if nothing there.

observeEvent(input$PREV_plhiv, {
    if (input$uPLHIV != 0 & !is.na(input$uPLHIV) & input$uPLHIV_source != "Please select source...") {
        if (isEmpty(MasterData[["calib"]][MasterData[["calib"]]$year == input$uPLHIV_year & MasterData[["calib"]]$indicator == "PLHIV", "value"])) {
            country <- input$selectCountry
            indicator <- "PLHIV"
            year <- input$uPLHIV_year
            value <- input$uPLHIV
            weight <- as.vector(SourceList$weight[which(SourceList == input$uPLHIV_source)])
            newData <- data.frame(country, indicator, year, value, weight)
            MasterData[["calib"]] <<- rbind(MasterData[["calib"]], newData)
            message(paste("MasterData, row added for uPLHIV =", value, "in", year, "with weight", weight))
            print(MasterData[["calib"]][MasterData[["calib"]]$year == input$uPLHIV_year & MasterData[["calib"]]$indicator == "PLHIV",])
        } else {
            MasterData[["calib"]][MasterData[["calib"]]$year == input$uPLHIV_year & MasterData[["calib"]]$indicator == "PLHIV", "value"] <<- input$uPLHIV
            MasterData[["calib"]][MasterData[["calib"]]$year == input$uPLHIV_year & MasterData[["calib"]]$indicator == "PLHIV", "weight"] <<- as.vector(SourceList$weight[which(SourceList == input$uPLHIV_source)])
            message(paste("MasterData updated with uPLHIV =", input$uPLHIV, "in", input$uPLHIV_year, "with weight", input$uPLHIV_source))
            print(MasterData[["calib"]][MasterData[["calib"]]$year == input$uPLHIV_year & MasterData[["calib"]]$indicator == "PLHIV",])
        }
    }
})

observeEvent(input$NEXT_plhiv, {
    if (input$uPLHIV != 0 & !is.na(input$uPLHIV) & input$uPLHIV_source != "Please select source...") {
        if (isEmpty(MasterData[["calib"]][MasterData[["calib"]]$year == input$uPLHIV_year & MasterData[["calib"]]$indicator == "PLHIV", "value"])) {
            country <- input$selectCountry
            indicator <- "PLHIV"
            year <- input$uPLHIV_year
            value <- input$uPLHIV
            weight <- as.vector(SourceList$weight[which(SourceList == input$uPLHIV_source)])
            newData <- data.frame(country, indicator, year, value, weight)
            MasterData[["calib"]] <<- rbind(MasterData[["calib"]], newData)
            message(paste("MasterData, row added for uPLHIV =", value, "in", year, "with weight", weight))
            print(MasterData[["calib"]][MasterData[["calib"]]$year == input$uPLHIV_year & MasterData[["calib"]]$indicator == "PLHIV",])
        } else {
            MasterData[["calib"]][MasterData[["calib"]]$year == input$uPLHIV_year & MasterData[["calib"]]$indicator == "PLHIV", "value"] <<- input$uPLHIV
            MasterData[["calib"]][MasterData[["calib"]]$year == input$uPLHIV_year & MasterData[["calib"]]$indicator == "PLHIV", "weight"] <<- as.vector(SourceList$weight[which(SourceList == input$uPLHIV_source)])
            message(paste("MasterData updated with uPLHIV =", input$uPLHIV, "in", input$uPLHIV_year, "with weight", input$uPLHIV_source))
            print(MasterData[["calib"]][MasterData[["calib"]]$year == input$uPLHIV_year & MasterData[["calib"]]$indicator == "PLHIV",])
        }
    }
})

observeEvent(input$NEXT_diag, {
    if (input$uDIAG != 0 & !is.na(input$uDIAG) & input$uDIAG_source != "Please select source...") {
        if (isEmpty(MasterData[["calib"]][MasterData[["calib"]]$year == input$uDIAG_year & MasterData[["calib"]]$indicator == "PLHIV Diagnosed", "value"])) {
            country <- input$selectCountry
            indicator <- "PLHIV Diagnosed"
            year <- input$uDIAG_year
            value <- input$uDIAG
            weight <- as.vector(SourceList$weight[which(SourceList == input$uDIAG_source)])
            newData <- data.frame(country, indicator, year, value, weight)
            MasterData[["calib"]] <<- rbind(MasterData[["calib"]], newData)
            message(paste("MasterData, row added for uDIAG =", value, "in", year, "with weight", weight))
            print(MasterData[["calib"]][MasterData[["calib"]]$year == input$uDIAG_year & MasterData[["calib"]]$indicator == "PLHIV Diagnosed",])
        } else {
            MasterData[["calib"]][MasterData[["calib"]]$year == input$uDIAG_year & MasterData[["calib"]]$indicator == "PLHIV Diagnosed", "value"] <<- input$uDIAG
            MasterData[["calib"]][MasterData[["calib"]]$year == input$uDIAG_year & MasterData[["calib"]]$indicator == "PLHIV Diagnosed", "weight"] <<- as.vector(SourceList$weight[which(SourceList == input$uDIAG_source)])
            message(paste("MasterData updated with uDIAG =", input$uDIAG, "in", input$uDIAG_year, "with weight", input$uDIAG_source))
            print(MasterData[["calib"]][MasterData[["calib"]]$year == input$uDIAG_year & MasterData[["calib"]]$indicator == "PLHIV Diagnosed",])
        }
    }
})

observeEvent(input$NEXT_care, {
    if (input$uCARE != 0 & !is.na(input$uCARE) & input$uCARE_source != "Please select source...") {
        if (isEmpty(MasterData[["calib"]][MasterData[["calib"]]$year == input$uCARE_year & MasterData[["calib"]]$indicator == "PLHIV in Care", "value"])) {
            country <- input$selectCountry
            indicator <- "PLHIV in Care"
            year <- input$uCARE_year
            value <- input$uCARE
            weight <- as.vector(SourceList$weight[which(SourceList == input$uCARE_source)])
            newData <- data.frame(country, indicator, year, value, weight)
            MasterData[["calib"]] <<- rbind(MasterData[["calib"]], newData)
            message(paste("MasterData, row added for uCARE =", value, "in", year, "with weight", weight))
            print(MasterData[["calib"]][MasterData[["calib"]]$year == input$uCARE_year & MasterData[["calib"]]$indicator == "PLHIV in Care",])
        } else {
            MasterData[["calib"]][MasterData[["calib"]]$year == input$uCARE_year & MasterData[["calib"]]$indicator == "PLHIV in Care", "value"] <<- input$uCARE
            MasterData[["calib"]][MasterData[["calib"]]$year == input$uCARE_year & MasterData[["calib"]]$indicator == "PLHIV in Care", "weight"] <<- as.vector(SourceList$weight[which(SourceList == input$uCARE_source)])
            message(paste("MasterData updated with uCARE =", input$uCARE, "in", input$uCARE_year, "with weight", input$uCARE_source))
            print(MasterData[["calib"]][MasterData[["calib"]]$year == input$uCARE_year & MasterData[["calib"]]$indicator == "PLHIV in Care",])
        }
    }
})

observeEvent(input$NEXT_art, {
    if (input$uART != 0 & !is.na(input$uART) & input$uART_source != "Please select source...") {
        if (isEmpty(MasterData[["calib"]][MasterData[["calib"]]$year == input$uART_year & MasterData[["calib"]]$indicator == "PLHIV on ART", "value"])) {
            country <- input$selectCountry
            indicator <- "PLHIV on ART"
            year <- input$uART_year
            value <- input$uART
            weight <- as.vector(SourceList$weight[which(SourceList == input$uART_source)])
            newData <- data.frame(country, indicator, year, value, weight)
            MasterData[["calib"]] <<- rbind(MasterData[["calib"]], newData)
            message(paste("MasterData, row added for uART =", value, "in", year, "with weight", weight))
            print(MasterData[["calib"]][MasterData[["calib"]]$year == input$uART_year & MasterData[["calib"]]$indicator == "PLHIV on ART",])
        } else {
            MasterData[["calib"]][MasterData[["calib"]]$year == input$uART_year & MasterData[["calib"]]$indicator == "PLHIV on ART", "value"] <<- input$uART
            MasterData[["calib"]][MasterData[["calib"]]$year == input$uART_year & MasterData[["calib"]]$indicator == "PLHIV on ART", "weight"] <<- as.vector(SourceList$weight[which(SourceList == input$uART_source)])
            message(paste("MasterData updated with uART =", input$uART, "in", input$uART_year, "with weight", input$uART_source))
            print(MasterData[["calib"]][MasterData[["calib"]]$year == input$uART_year & MasterData[["calib"]]$indicator == "PLHIV on ART",])
        }
    }
})

observeEvent(input$NEXT_viral, {
    if (input$uVIRAL != 0 & !is.na(input$uVIRAL) & input$uVIRAL_source != "Please select source...") {
        if (isEmpty(MasterData[["calib"]][MasterData[["calib"]]$year == input$uVIRAL_year & MasterData[["calib"]]$indicator == "PLHIV Suppressed", "value"])) {
            country <- input$selectCountry
            indicator <- "PLHIV Suppressed"
            year <- input$uVIRAL_year
            value <- input$uVIRAL
            weight <- as.vector(SourceList$weight[which(SourceList == input$uVIRAL_source)])
            newData <- data.frame(country, indicator, year, value, weight)
            MasterData[["calib"]] <<- rbind(MasterData[["calib"]], newData)
            message(paste("MasterData, row added for uVIRAL =", value, "in", year, "with weight", weight))
            print(MasterData[["calib"]][MasterData[["calib"]]$year == input$uVIRAL_year & MasterData[["calib"]]$indicator == "PLHIV Suppressed",])
        } else {
            MasterData[["calib"]][MasterData[["calib"]]$year == input$uVIRAL_year & MasterData[["calib"]]$indicator == "PLHIV Suppressed", "value"] <<- input$uVIRAL
            MasterData[["calib"]][MasterData[["calib"]]$year == input$uVIRAL_year & MasterData[["calib"]]$indicator == "PLHIV Suppressed", "weight"] <<- as.vector(SourceList$weight[which(SourceList == input$uVIRAL_source)])
            message(paste("MasterData updated with uVIRAL =", input$uVIRAL, "in", input$uVIRAL_year, "with weight", input$uVIRAL_source))
            print(MasterData[["calib"]][MasterData[["calib"]]$year == input$uVIRAL_year & MasterData[["calib"]]$indicator == "PLHIV Suppressed",])
        }
    }
})

####################
# RESET function calls

observeEvent(input$resetPLHIV, { shinyjs::reset("plhiv_panel") })
observeEvent(input$resetDIAG,  { shinyjs::reset("diag_panel")  })
observeEvent(input$resetCARE,  { shinyjs::reset("care_panel")  })
observeEvent(input$resetART,   { shinyjs::reset("art_panel")   })
observeEvent(input$resetVIRAL, { shinyjs::reset("viral_panel") })

####################
# Update parRange for calibration

observeEvent(input$uCalib_rho,     { userParRange$rho     <<- input$uCalib_rho })
observeEvent(input$uCalib_epsilon, { userParRange$epsilon <<- input$uCalib_epsilon })
observeEvent(input$uCalib_kappa,   { userParRange$kappa   <<- input$uCalib_kappa })
observeEvent(input$uCalib_gamma,   { userParRange$gamma   <<- input$uCalib_gamma })
observeEvent(input$uCalib_theta,   { userParRange$theta   <<- input$uCalib_theta })
observeEvent(input$uCalib_omega,   { userParRange$omega   <<- input$uCalib_omega })
observeEvent(input$uCalib_p,       { userParRange$p       <<- input$uCalib_p })
observeEvent(input$uCalib_q,       { userParRange$q       <<- input$uCalib_q })