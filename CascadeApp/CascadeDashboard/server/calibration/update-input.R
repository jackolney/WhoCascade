# Update MasterData upon hitting 'NEXT'

observeEvent(input$NEXT_plhiv, {
    print("MasterData updated.")
    MasterData[["calib"]][MasterData[["calib"]]$year == input$uPLHIV_year & MasterData[["calib"]]$indicator == "PLHIV", "value"] <<- input$uPLHIV
    if (input$uPLHIV_source != "Please select source...") {
        MasterData[["calib"]][MasterData[["calib"]]$year == input$uPLHIV_year & MasterData[["calib"]]$indicator == "PLHIV", "weight"] <<- as.vector(SourceList$weight[which(SourceList == input$uPLHIV_source)])
    }
    print(MasterData)
})

observeEvent(input$NEXT_diag, {
    print("MasterData updated.")
    MasterData[["calib"]][MasterData[["calib"]]$year == input$uDIAG_year & MasterData[["calib"]]$indicator == "PLHIV Diagnosed", "value"] <<- input$uDIAG
    if (input$uDIAG_source != "Please select source...") {
        MasterData[["calib"]][MasterData[["calib"]]$year == input$uDIAG_year & MasterData[["calib"]]$indicator == "PLHIV Diagnosed", "weight"] <<- as.vector(SourceList$weight[which(SourceList == input$uDIAG_source)])
    }
    print(MasterData)
})

observeEvent(input$NEXT_care, {
    print("MasterData updated.")
    MasterData[["calib"]][MasterData[["calib"]]$year == input$uCARE_year & MasterData[["calib"]]$indicator == "PLHIV in Care", "value"] <<- input$uCARE
    if (input$uCARE_source != "Please select source...") {
        MasterData[["calib"]][MasterData[["calib"]]$year == input$uCARE_year & MasterData[["calib"]]$indicator == "PLHIV in Care", "weight"] <<- as.vector(SourceList$weight[which(SourceList == input$uCARE_source)])
    }
    print(MasterData)
})

observeEvent(input$NEXT_art, {
    print("MasterData updated.")
    MasterData[["calib"]][MasterData[["calib"]]$year == input$uART_year & MasterData[["calib"]]$indicator == "PLHIV on ART", "value"] <<- input$uART
    if (input$uART_source != "Please select source...") {
        MasterData[["calib"]][MasterData[["calib"]]$year == input$uART_year & MasterData[["calib"]]$indicator == "PLHIV on ART", "weight"] <<- as.vector(SourceList$weight[which(SourceList == input$uART_source)])
    }
    print(MasterData)
})

observeEvent(input$NEXT_viral, {
    print("MasterData updated.")
    MasterData[["calib"]][MasterData[["calib"]]$year == input$uVIRAL_year & MasterData[["calib"]]$indicator == "PLHIV Suppressed", "value"] <<- input$uVIRAL
    if (input$uVIRAL_source != "Please select source...") {
        MasterData[["calib"]][MasterData[["calib"]]$year == input$uVIRAL_year & MasterData[["calib"]]$indicator == "PLHIV Suppressed", "weight"] <<- as.vector(SourceList$weight[which(SourceList == input$uVIRAL_source)])
    }
    print(MasterData)
})

####################
# 'Traffic Light' Buttons

observeEvent(input$uPLHIV_source, {
    if (input$uPLHIV_source != "Please select source...") {
        if (SourceList$weight[which(SourceList == input$uPLHIV_source)] == "green") {
            # Close any open alerts
            shinyBS::closeAlert(session, alertId = "alertId_uPLHIV_amber")
            shinyBS::closeAlert(session, alertId = "alertId_uPLHIV_red")
            # Create new alert
            shinyBS::createAlert(session,
                anchorId = "uPLHIV_ALERT_green",
                alertId = "alertId_uPLHIV_green",
                title = paste(icon("thumbs-up", class = "fa-lg fa-fw", lib = "font-awesome"), "GOOD"),
                content = "Data source is of reliable quality.",
                style = "success",
                dismiss = TRUE,
                append = TRUE)
        } else if (SourceList$weight[which(SourceList == input$uPLHIV_source)] == "amber") {
            # Close any open alerts
            shinyBS::closeAlert(session, alertId = "alertId_uPLHIV_green")
            shinyBS::closeAlert(session, alertId = "alertId_uPLHIV_red")
            # Create new alert
            shinyBS::createAlert(session,
                anchorId = "uPLHIV_ALERT_amber",
                alertId = "alertId_uPLHIV_amber",
                title = paste(icon("hand-stop-o", class = "fa-lg fa-fw", lib = "font-awesome"), "USE WITH CAUTION"),
                content = "Data source is of questionable quality.",
                style = "warning",
                dismiss = TRUE,
                append = TRUE)
        } else if (SourceList$weight[which(SourceList == input$uPLHIV_source)] == "red") {
            # Close any open alerts
            shinyBS::closeAlert(session, alertId = "alertId_uPLHIV_green")
            shinyBS::closeAlert(session, alertId = "alertId_uPLHIV_amber")
            # Create new alert
            shinyBS::createAlert(session,
                anchorId = "uPLHIV_ALERT_red",
                alertId = "alertId_uPLHIV_red",
                title = paste(icon("warning", class = "fa-lg fa-fw", lib = "font-awesome"), "DANGER"),
                content = "Data source is of unreliable quality, use with extreme caution.",
                style = "danger",
                dismiss = TRUE,
                append = TRUE)
        }
    } else {
        return()
    }
})

observeEvent(input$uDIAG_source, {
    if (input$uDIAG_source != "Please select source...") {
        if (SourceList$weight[which(SourceList == input$uDIAG_source)] == "green") {
            # Close any open alerts
            shinyBS::closeAlert(session, alertId = "alertId_uDIAG_amber")
            shinyBS::closeAlert(session, alertId = "alertId_uDIAG_red")
            # Create new alert
            shinyBS::createAlert(session,
                anchorId = "uDIAG_ALERT_green",
                alertId = "alertId_uDIAG_green",
                title = paste(icon("thumbs-up", class = "fa-lg fa-fw", lib = "font-awesome"), "GOOD"),
                content = "Data source is of reliable quality.",
                style = "success",
                dismiss = TRUE,
                append = TRUE)
        } else if (SourceList$weight[which(SourceList == input$uDIAG_source)] == "amber") {
            # Close any open alerts
            shinyBS::closeAlert(session, alertId = "alertId_uDIAG_green")
            shinyBS::closeAlert(session, alertId = "alertId_uDIAG_red")
            # Create new alert
            shinyBS::createAlert(session,
                anchorId = "uDIAG_ALERT_amber",
                alertId = "alertId_uDIAG_amber",
                title = paste(icon("hand-stop-o", class = "fa-lg fa-fw", lib = "font-awesome"), "USE WITH CAUTION"),
                content = "Data source is of questionable quality.",
                style = "warning",
                dismiss = TRUE,
                append = TRUE)
        } else if (SourceList$weight[which(SourceList == input$uDIAG_source)] == "red") {
            # Close any open alerts
            shinyBS::closeAlert(session, alertId = "alertId_uDIAG_green")
            shinyBS::closeAlert(session, alertId = "alertId_uDIAG_amber")
            # Create new alert
            shinyBS::createAlert(session,
                anchorId = "uDIAG_ALERT_red",
                alertId = "alertId_uDIAG_red",
                title = paste(icon("warning", class = "fa-lg fa-fw", lib = "font-awesome"), "DANGER"),
                content = "Data source is of unreliable quality, use with extreme caution.",
                style = "danger",
                dismiss = TRUE,
                append = TRUE)
        }
    } else {
        return()
    }
})

observeEvent(input$uCARE_source, {
    if (input$uCARE_source != "Please select source...") {
        if (SourceList$weight[which(SourceList == input$uCARE_source)] == "green") {
            # Close any open alerts
            shinyBS::closeAlert(session, alertId = "alertId_uCARE_amber")
            shinyBS::closeAlert(session, alertId = "alertId_uCARE_red")
            # Create new alert
            shinyBS::createAlert(session,
                anchorId = "uCARE_ALERT_green",
                alertId = "alertId_uCARE_green",
                title = paste(icon("thumbs-up", class = "fa-lg fa-fw", lib = "font-awesome"), "GOOD"),
                content = "Data source is of reliable quality.",
                style = "success",
                dismiss = TRUE,
                append = TRUE)
        } else if (SourceList$weight[which(SourceList == input$uCARE_source)] == "amber") {
            # Close any open alerts
            shinyBS::closeAlert(session, alertId = "alertId_uCARE_green")
            shinyBS::closeAlert(session, alertId = "alertId_uCARE_red")
            # Create new alert
            shinyBS::createAlert(session,
                anchorId = "uCARE_ALERT_amber",
                alertId = "alertId_uCARE_amber",
                title = paste(icon("hand-stop-o", class = "fa-lg fa-fw", lib = "font-awesome"), "USE WITH CAUTION"),
                content = "Data source is of questionable quality.",
                style = "warning",
                dismiss = TRUE,
                append = TRUE)
        } else if (SourceList$weight[which(SourceList == input$uCARE_source)] == "red") {
            # Close any open alerts
            shinyBS::closeAlert(session, alertId = "alertId_uCARE_green")
            shinyBS::closeAlert(session, alertId = "alertId_uCARE_amber")
            # Create new alert
            shinyBS::createAlert(session,
                anchorId = "uCARE_ALERT_red",
                alertId = "alertId_uCARE_red",
                title = paste(icon("warning", class = "fa-lg fa-fw", lib = "font-awesome"), "DANGER"),
                content = "Data source is of unreliable quality, use with extreme caution.",
                style = "danger",
                dismiss = TRUE,
                append = TRUE)
        }
    } else {
        return()
    }
})

observeEvent(input$uART_source, {
    if (input$uART_source != "Please select source...") {
        if (SourceList$weight[which(SourceList == input$uART_source)] == "green") {
            # Close any open alerts
            shinyBS::closeAlert(session, alertId = "alertId_uART_amber")
            shinyBS::closeAlert(session, alertId = "alertId_uART_red")
            # Create new alert
            shinyBS::createAlert(session,
                anchorId = "uART_ALERT_green",
                alertId = "alertId_uART_green",
                title = paste(icon("thumbs-up", class = "fa-lg fa-fw", lib = "font-awesome"), "GOOD"),
                content = "Data source is of reliable quality.",
                style = "success",
                dismiss = TRUE,
                append = TRUE)
        } else if (SourceList$weight[which(SourceList == input$uART_source)] == "amber") {
            # Close any open alerts
            shinyBS::closeAlert(session, alertId = "alertId_uART_green")
            shinyBS::closeAlert(session, alertId = "alertId_uART_red")
            # Create new alert
            shinyBS::createAlert(session,
                anchorId = "uART_ALERT_amber",
                alertId = "alertId_uART_amber",
                title = paste(icon("hand-stop-o", class = "fa-lg fa-fw", lib = "font-awesome"), "USE WITH CAUTION"),
                content = "Data source is of questionable quality.",
                style = "warning",
                dismiss = TRUE,
                append = TRUE)
        } else if (SourceList$weight[which(SourceList == input$uART_source)] == "red") {
            # Close any open alerts
            shinyBS::closeAlert(session, alertId = "alertId_uART_green")
            shinyBS::closeAlert(session, alertId = "alertId_uART_amber")
            # Create new alert
            shinyBS::createAlert(session,
                anchorId = "uART_ALERT_red",
                alertId = "alertId_uART_red",
                title = paste(icon("warning", class = "fa-lg fa-fw", lib = "font-awesome"), "DANGER"),
                content = "Data source is of unreliable quality, use with extreme caution.",
                style = "danger",
                dismiss = TRUE,
                append = TRUE)
        }
    } else {
        return()
    }
})

observeEvent(input$uVIRAL_source, {
    if (input$uVIRAL_source != "Please select source...") {
        if (SourceList$weight[which(SourceList == input$uVIRAL_source)] == "green") {
            # Close any open alerts
            shinyBS::closeAlert(session, alertId = "alertId_uVIRAL_amber")
            shinyBS::closeAlert(session, alertId = "alertId_uVIRAL_red")
            # Create new alert
            shinyBS::createAlert(session,
                anchorId = "uVIRAL_ALERT_green",
                alertId = "alertId_uVIRAL_green",
                title = paste(icon("thumbs-up", class = "fa-lg fa-fw", lib = "font-awesome"), "GOOD"),
                content = "Data source is of reliable quality.",
                style = "success",
                dismiss = TRUE,
                append = TRUE)
        } else if (SourceList$weight[which(SourceList == input$uVIRAL_source)] == "amber") {
            # Close any open alerts
            shinyBS::closeAlert(session, alertId = "alertId_uVIRAL_green")
            shinyBS::closeAlert(session, alertId = "alertId_uVIRAL_red")
            # Create new alert
            shinyBS::createAlert(session,
                anchorId = "uVIRAL_ALERT_amber",
                alertId = "alertId_uVIRAL_amber",
                title = paste(icon("hand-stop-o", class = "fa-lg fa-fw", lib = "font-awesome"), "USE WITH CAUTION"),
                content = "Data source is of questionable quality.",
                style = "warning",
                dismiss = TRUE,
                append = TRUE)
        } else if (SourceList$weight[which(SourceList == input$uVIRAL_source)] == "red") {
            # Close any open alerts
            shinyBS::closeAlert(session, alertId = "alertId_uVIRAL_green")
            shinyBS::closeAlert(session, alertId = "alertId_uVIRAL_amber")
            # Create new alert
            shinyBS::createAlert(session,
                anchorId = "uVIRAL_ALERT_red",
                alertId = "alertId_uVIRAL_red",
                title = paste(icon("warning", class = "fa-lg fa-fw", lib = "font-awesome"), "DANGER"),
                content = "Data source is of unreliable quality, use with extreme caution.",
                style = "danger",
                dismiss = TRUE,
                append = TRUE)
        }
    } else {
        return()
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
observeEvent(input$uCalib_mu,      { userParRange$mu      <<- input$uCalib_mu })
observeEvent(input$uCalib_p,       { userParRange$p       <<- input$uCalib_p })
observeEvent(input$uCalib_q,       { userParRange$q       <<- input$uCalib_q })
