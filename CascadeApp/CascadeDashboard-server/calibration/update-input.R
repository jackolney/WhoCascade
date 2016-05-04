# Update MasterData upon hitting 'NEXT'

observeEvent(input$NEXT_plhiv, {
    print("MasterData updated.")
    MasterData[["calib"]][MasterData[["calib"]]$year == input$uPLHIV_year & MasterData[["calib"]]$indicator == "PLHIV", "value"] <<- input$uPLHIV
    if(input$uPLHIV_source != "Please select source...") {
        MasterData[["calib"]][MasterData[["calib"]]$year == input$uPLHIV_year & MasterData[["calib"]]$indicator == "PLHIV", "weight"] <<- as.vector(SourceList$weight[which(SourceList == input$uPLHIV_source)])
    }
    print(MasterData)
})

observeEvent(input$NEXT_diag, {
    print("MasterData updated.")
    MasterData[["calib"]][MasterData[["calib"]]$year == input$uDIAG_year & MasterData[["calib"]]$indicator == "PLHIV Diagnosed", "value"] <<- input$uDIAG
    if(input$uDIAG_source != "Please select source...") {
        MasterData[["calib"]][MasterData[["calib"]]$year == input$uDIAG_year & MasterData[["calib"]]$indicator == "PLHIV Diagnosed", "weight"] <<- as.vector(SourceList$weight[which(SourceList == input$uDIAG_source)])
    }
    print(MasterData)
})

observeEvent(input$NEXT_care, {
    print("MasterData updated.")
    MasterData[["calib"]][MasterData[["calib"]]$year == input$uCARE_year & MasterData[["calib"]]$indicator == "PLHIV in Care", "value"] <<- input$uCARE
    if(input$uCARE_source != "Please select source...") {
        MasterData[["calib"]][MasterData[["calib"]]$year == input$uCARE_year & MasterData[["calib"]]$indicator == "PLHIV in Care", "weight"] <<- as.vector(SourceList$weight[which(SourceList == input$uCARE_source)])
    }
    print(MasterData)
})

observeEvent(input$NEXT_art, {
    print("MasterData updated.")
    MasterData[["calib"]][MasterData[["calib"]]$year == input$uART_year & MasterData[["calib"]]$indicator == "PLHIV on ART", "value"] <<- input$uART
    if(input$uART_source != "Please select source...") {
        MasterData[["calib"]][MasterData[["calib"]]$year == input$uART_year & MasterData[["calib"]]$indicator == "PLHIV on ART", "weight"] <<- as.vector(SourceList$weight[which(SourceList == input$uART_source)])
    }
    print(MasterData)
})

observeEvent(input$NEXT_viral, {
    print("MasterData updated.")
    MasterData[["calib"]][MasterData[["calib"]]$year == input$uVIRAL_year & MasterData[["calib"]]$indicator == "PLHIV Suppressed", "value"] <<- input$uVIRAL
    if(input$uVIRAL_source != "Please select source...") {
        MasterData[["calib"]][MasterData[["calib"]]$year == input$uVIRAL_year & MasterData[["calib"]]$indicator == "PLHIV Suppressed", "weight"] <<- as.vector(SourceList$weight[which(SourceList == input$uVIRAL_source)])
    }
    print(MasterData)
})

####################
# 'Traffic Light' Buttons

observeEvent(input$uPLHIV_source, {
    if(input$uPLHIV_source != "Please select source...") {
        if(SourceList$weight[which(SourceList == input$uPLHIV_source)] == "green") {
            output$uPLHIV_quality <- renderUI({
                bsButton(inputId = "_uPLHIV_quality_", label = "GOOD", style = "success", size = "large", block = TRUE, disabled = TRUE, icon = icon("thumbs-up", class = "fa-lg fa-fw", lib = "font-awesome"))
            })
        } else if(SourceList$weight[which(SourceList == input$uPLHIV_source)] == "amber") {
            output$uPLHIV_quality <- renderUI({
                bsButton(inputId = "_uPLHIV_quality_", label = "AVERAGE", style = "warning", size = "large", block = TRUE, disabled = TRUE)
            })
        } else if(SourceList$weight[which(SourceList == input$uPLHIV_source)] == "red") {
            output$uPLHIV_quality <- renderUI({
                bsButton(inputId = "_uPLHIV_quality_", label = "POOR", style = "danger", size = "large", block = TRUE, disabled = TRUE, icon = icon("thumbs-down", class = "fa-lg fa-fw", lib = "font-awesome"))
            })
        }
    } else {
        output$uPLHIV_quality <- renderUI({})
    }
})

observeEvent(input$uDIAG_source, {
    if(input$uDIAG_source != "Please select source...") {
        if(SourceList$weight[which(SourceList == input$uDIAG_source)] == "green") {
            output$uDIAG_quality <- renderUI({
                bsButton(inputId = "_uDIAG_quality_", label = "GOOD", style = "success", size = "large", block = TRUE, disabled = TRUE, icon = icon("thumbs-up", class = "fa-lg fa-fw", lib = "font-awesome"))
            })
        } else if(SourceList$weight[which(SourceList == input$uDIAG_source)] == "amber") {
            output$uDIAG_quality <- renderUI({
                bsButton(inputId = "_uDIAG_quality_", label = "AVERAGE", style = "warning", size = "large", block = TRUE, disabled = TRUE)
            })
        } else if(SourceList$weight[which(SourceList == input$uDIAG_source)] == "red") {
            output$uDIAG_quality <- renderUI({
                bsButton(inputId = "_uDIAG_quality_", label = "POOR", style = "danger", size = "large", block = TRUE, disabled = TRUE, icon = icon("thumbs-down", class = "fa-lg fa-fw", lib = "font-awesome"))
            })
        }
    } else {
        output$uDIAG_quality <- renderUI({})
    }
})

observeEvent(input$uCARE_source, {
    if(input$uCARE_source != "Please select source...") {
        if(SourceList$weight[which(SourceList == input$uCARE_source)] == "green") {
            output$uCARE_quality <- renderUI({
                bsButton(inputId = "_uCARE_quality_", label = "GOOD", style = "success", size = "large", block = TRUE, disabled = TRUE, icon = icon("thumbs-up", class = "fa-lg fa-fw", lib = "font-awesome"))
            })
        } else if(SourceList$weight[which(SourceList == input$uCARE_source)] == "amber") {
            output$uCARE_quality <- renderUI({
                bsButton(inputId = "_uCARE_quality_", label = "AVERAGE", style = "warning", size = "large", block = TRUE, disabled = TRUE)
            })
        } else if(SourceList$weight[which(SourceList == input$uCARE_source)] == "red") {
            output$uCARE_quality <- renderUI({
                bsButton(inputId = "_uCARE_quality_", label = "POOR", style = "danger", size = "large", block = TRUE, disabled = TRUE, icon = icon("thumbs-down", class = "fa-lg fa-fw", lib = "font-awesome"))
            })
        }
    } else {
        output$uCARE_quality <- renderUI({})
    }
})

observeEvent(input$uART_source, {
    if(input$uART_source != "Please select source...") {
        if(SourceList$weight[which(SourceList == input$uART_source)] == "green") {
            output$uART_quality <- renderUI({
                bsButton(inputId = "_uART_quality_", label = "GOOD", style = "success", size = "large", block = TRUE, disabled = TRUE, icon = icon("thumbs-up", class = "fa-lg fa-fw", lib = "font-awesome"))
            })
        } else if(SourceList$weight[which(SourceList == input$uART_source)] == "amber") {
            output$uART_quality <- renderUI({
                bsButton(inputId = "_uART_quality_", label = "AVERAGE", style = "warning", size = "large", block = TRUE, disabled = TRUE)
            })
        } else if(SourceList$weight[which(SourceList == input$uART_source)] == "red") {
            output$uART_quality <- renderUI({
                bsButton(inputId = "_uART_quality_", label = "POOR", style = "danger", size = "large", block = TRUE, disabled = TRUE, icon = icon("thumbs-down", class = "fa-lg fa-fw", lib = "font-awesome"))
            })
        }
    } else {
        output$uART_quality <- renderUI({})
    }
})

observeEvent(input$uVIRAL_source, {
    if(input$uVIRAL_source != "Please select source...") {
        if(SourceList$weight[which(SourceList == input$uVIRAL_source)] == "green") {
            output$uVIRAL_quality <- renderUI({
                bsButton(inputId = "_uVIRAL_quality_", label = "GOOD", style = "success", size = "large", block = TRUE, disabled = TRUE, icon = icon("thumbs-up", class = "fa-lg fa-fw", lib = "font-awesome"))
            })
        } else if(SourceList$weight[which(SourceList == input$uVIRAL_source)] == "amber") {
            output$uVIRAL_quality <- renderUI({
                bsButton(inputId = "_uVIRAL_quality_", label = "AVERAGE", style = "warning", size = "large", block = TRUE, disabled = TRUE)
            })
        } else if(SourceList$weight[which(SourceList == input$uVIRAL_source)] == "red") {
            output$uVIRAL_quality <- renderUI({
                bsButton(inputId = "_uVIRAL_quality_", label = "POOR", style = "danger", size = "large", block = TRUE, disabled = TRUE, icon = icon("thumbs-down", class = "fa-lg fa-fw", lib = "font-awesome"))
            })
        }
    } else {
        output$uVIRAL_quality <- renderUI({})
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
