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
