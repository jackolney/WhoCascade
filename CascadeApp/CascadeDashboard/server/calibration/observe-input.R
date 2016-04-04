## uPLHIV ##

output$UI_uPLHIV <- renderUI({
    numericInput("uPLHIV", "Number of people living with HIV:",
        value = MasterData[["calib"]][MasterData[["calib"]]$year == 2015 & MasterData[["calib"]]$indicator == "PLHIV", "value"],
        min = 0,
        width = '100%')
})

output$UI_uPLHIV_source <- renderUI({
    selectInput("uPLHIV_source", "Source of Data:", SourceList, selected = "Please select source...")
})

output$UI_uPLHIV_year <- renderUI({
    selectInput("uPLHIV_year", "Year data represents:", YearList, selected = 2015)
})

observeEvent(input$uPLHIV_source, {
    if(input$uPLHIV_source != "Please select source...") {
        if(input$uPLHIV_source %in% c("Mathematical Model", "Nationally Representative Study")) {
            output$uPLHIV_quality <- renderUI({
                bsButton(inputId = "_uPLHIV_quality_", label = "GOOD", style = "success", size = "large", block = TRUE, disabled = TRUE, icon = icon("thumbs-up", class = "fa-lg fa-fw", lib = "font-awesome"))
            })
        } else if(input$uPLHIV_source == "Peer-reviewed Study") {
            output$uPLHIV_quality <- renderUI({
                bsButton(inputId = "_uPLHIV_quality_", label = "AVERAGE", style = "warning", size = "large", block = TRUE, disabled = TRUE)
            })
        } else if(input$uPLHIV_source == "Estimate") {
            output$uPLHIV_quality <- renderUI({
                bsButton(inputId = "_uPLHIV_quality_", label = "POOR", style = "danger", size = "large", block = TRUE, disabled = TRUE, icon = icon("thumbs-down", class = "fa-lg fa-fw", lib = "font-awesome"))
            })
        }
    } else {
        output$uPLHIV_quality <- renderUI({})
    }
})

## uDIAG ##

output$UI_uDIAG <- renderUI({
    numericInput("uDIAG", "Number of people diagnosed with HIV:",
        value = MasterData[["calib"]][MasterData[["calib"]]$year == 2015 & MasterData[["calib"]]$indicator == "PLHIV Diagnosed", "value"],
        min = 0,
        width = '100%')
})

output$UI_uDIAG_source <- renderUI({
    selectInput("uDIAG_source", "Source of Data:", SourceList, selected = "Please select source...")
})

output$UI_uDIAG_year <- renderUI({
    selectInput("uDIAG_year", "Year data represents:", YearList, selected = 2015)
})

observeEvent(input$uDIAG_source, {
    if(input$uDIAG_source != "Please select source...") {
        if(input$uDIAG_source %in% c("Mathematical Model", "Nationally Representative Study")) {
            output$uDIAG_quality <- renderUI({
                bsButton(inputId = "_uDIAG_quality_", label = "GOOD", style = "success", size = "large", block = TRUE, disabled = TRUE, icon = icon("thumbs-up", class = "fa-lg fa-fw", lib = "font-awesome"))
            })
        } else if(input$uDIAG_source == "Peer-reviewed Study") {
            output$uDIAG_quality <- renderUI({
                bsButton(inputId = "_uDIAG_quality_", label = "AVERAGE", style = "warning", size = "large", block = TRUE, disabled = TRUE)
            })
        } else if(input$uDIAG_source == "Estimate") {
            output$uDIAG_quality <- renderUI({
                bsButton(inputId = "_uDIAG_quality_", label = "POOR", style = "danger", size = "large", block = TRUE, disabled = TRUE, icon = icon("thumbs-down", class = "fa-lg fa-fw", lib = "font-awesome"))
            })
        }
    } else {
        output$uDIAG_quality <- renderUI({})
    }
})

## uCARE ##

output$UI_uCARE <- renderUI({
    numericInput("uCARE", "Number of people in HIV care:",
        value = MasterData[["calib"]][MasterData[["calib"]]$year == 2015 & MasterData[["calib"]]$indicator == "PLHIV in Care", "value"],
        min = 0,
        width = '100%')
})

output$UI_uCARE_source <- renderUI({
    selectInput("uCARE_source", "Source of Data:", SourceList, selected = "Please select source...")
})

output$UI_uCARE_year <- renderUI({
    selectInput("uCARE_year", "Year data represents:", YearList, selected = 2015)
})

observeEvent(input$uCARE_source, {
    if(input$uCARE_source != "Please select source...") {
        if(input$uCARE_source %in% c("Mathematical Model", "Nationally Representative Study")) {
            output$uCARE_quality <- renderUI({
                bsButton(inputId = "_uCARE_quality_", label = "GOOD", style = "success", size = "large", block = TRUE, disabled = TRUE, icon = icon("thumbs-up", class = "fa-lg fa-fw", lib = "font-awesome"))
            })
        } else if(input$uCARE_source == "Peer-reviewed Study") {
            output$uCARE_quality <- renderUI({
                bsButton(inputId = "_uCARE_quality_", label = "AVERAGE", style = "warning", size = "large", block = TRUE, disabled = TRUE)
            })
        } else if(input$uCARE_source == "Estimate") {
            output$uCARE_quality <- renderUI({
                bsButton(inputId = "_uCARE_quality_", label = "POOR", style = "danger", size = "large", block = TRUE, disabled = TRUE, icon = icon("thumbs-down", class = "fa-lg fa-fw", lib = "font-awesome"))
            })
        }
    } else {
        output$uCARE_quality <- renderUI({})
    }
})

## uART ##

output$UI_uART <- renderUI({
    numericInput("uART", "Number of people on ART:",
        value = MasterData[["calib"]][MasterData[["calib"]]$year == 2015 & MasterData[["calib"]]$indicator == "PLHIV on ART", "value"],
        min = 0,
        width = '100%')
})

output$UI_uART_source <- renderUI({
    selectInput("uART_source", "Source of Data:", SourceList, selected = "Please select source...")
})

output$UI_uART_year <- renderUI({
    selectInput("uART_year", "Year data represents:", YearList, selected = 2015)
})

observeEvent(input$uART_source, {
    if(input$uART_source != "Please select source...") {
        if(input$uART_source %in% c("Mathematical Model", "Nationally Representative Study")) {
            output$uART_quality <- renderUI({
                bsButton(inputId = "_uART_quality_", label = "GOOD", style = "success", size = "large", block = TRUE, disabled = TRUE, icon = icon("thumbs-up", class = "fa-lg fa-fw", lib = "font-awesome"))
            })
        } else if(input$uART_source == "Peer-reviewed Study") {
            output$uART_quality <- renderUI({
                bsButton(inputId = "_uART_quality_", label = "AVERAGE", style = "warning", size = "large", block = TRUE, disabled = TRUE)
            })
        } else if(input$uART_source == "Estimate") {
            output$uART_quality <- renderUI({
                bsButton(inputId = "_uART_quality_", label = "POOR", style = "danger", size = "large", block = TRUE, disabled = TRUE, icon = icon("thumbs-down", class = "fa-lg fa-fw", lib = "font-awesome"))
            })
        }
    } else {
        output$uART_quality <- renderUI({})
    }
})

## uVIRAL ##

output$UI_uVIRAL <- renderUI({
    numericInput("uVIRAL", "Number of people on ART and Virally Suppressed:",
        value = MasterData[["calib"]][MasterData[["calib"]]$year == 2015 & MasterData[["calib"]]$indicator == "PLHIV Suppressed", "value"],
        min = 0,
        width = '100%')
})

output$UI_uVIRAL_source <- renderUI({
    selectInput("uVIRAL_source", "Source of Data:", SourceList, selected = "Please select source...")
})

output$UI_uVIRAL_year <- renderUI({
    selectInput("uVIRAL_year", "Year data represents:", YearList, selected = 2015)
})

observeEvent(input$uVIRAL_source, {
    if(input$uVIRAL_source != "Please select source...") {
        if(input$uVIRAL_source %in% c("Mathematical Model", "Nationally Representative Study")) {
            output$uVIRAL_quality <- renderUI({
                bsButton(inputId = "_uVIRAL_quality_", label = "GOOD", style = "success", size = "large", block = TRUE, disabled = TRUE, icon = icon("thumbs-up", class = "fa-lg fa-fw", lib = "font-awesome"))
            })
        } else if(input$uVIRAL_source == "Peer-reviewed Study") {
            output$uVIRAL_quality <- renderUI({
                bsButton(inputId = "_uVIRAL_quality_", label = "AVERAGE", style = "warning", size = "large", block = TRUE, disabled = TRUE)
            })
        } else if(input$uVIRAL_source == "Estimate") {
            output$uVIRAL_quality <- renderUI({
                bsButton(inputId = "_uVIRAL_quality_", label = "POOR", style = "danger", size = "large", block = TRUE, disabled = TRUE, icon = icon("thumbs-down", class = "fa-lg fa-fw", lib = "font-awesome"))
            })
        }
    } else {
        output$uVIRAL_quality <- renderUI({})
    }
})
