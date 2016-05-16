# StartUp Caution Alert
shinyBS::createAlert(session,
    anchorId = "startAlert",
    alertId = NULL,
    title = paste(icon("exclamation-triangle", class = "fa-lg fa-fw", lib = "font-awesome"), "WARNING"),
    content = "This interactive web-based model is still under development.
    Any data entered into the model is done so at the users own risk.
    Clicking 'save' in any tab saves the current inputs to a centrally accessible spreadsheet hosted by Google.
    Results produced by this model are not finalised.
    Use with caution!",
    style = "danger",
    dismiss = TRUE,
    append = TRUE)

# PopOver
shinyBS::addPopover(session, id = "plotOpt909090",
    title = "Info",
    content = "Vertical line represents 73% viral suppression (end goal of 90-90-90 targets, 0.9^3 = 0.729).",
    placement = "bottom",
    trigger = "hover",
    options = NULL)

## Setup Alerts
# Designed to catch users trying to enter non-sensical data

observeEvent(input$uDIAG, {
    if(input$uDIAG > input$uPLHIV) {
        # shinyBS alert
        shinyBS::createAlert(session,
            anchorId = "uDIAG_ALERT",
            alertId = "alertId_uDIAG",
            title = paste(icon("exclamation-triangle", class = "fa-lg fa-fw", lib = "font-awesome"), "WARNING"),
            content = "Value entered for 'PLHIV Diagnosed' exceeds the value of 'PLHIV' in this year.",
            style = "danger",
            dismiss = TRUE,
            append = TRUE)
        # JS alert & disable
        shinyjs::info(text = "Computer says no.")
        shinyjs::disable(id = "NEXT_diag")
    } else {
        shinyjs::enable(id = "NEXT_diag")
        shinyBS::closeAlert(session, alertId = "alertId_uDIAG")
    }
})

observeEvent(input$uCARE, {
    if(input$uCARE > input$uDIAG) {
        # shinyBS alert
        shinyBS::createAlert(session,
            anchorId = "uCARE_ALERT",
            alertId = "alertId_uCARE",
            title = paste(icon("exclamation-triangle", class = "fa-lg fa-fw", lib = "font-awesome"), "WARNING"),
            content = "Value entered for 'PLHIV in Care' exceeds the value of 'PLHIV Diagnosed' in this year.",
            style = "danger",
            dismiss = TRUE,
            append = TRUE)
        # JS alert & disable
        shinyjs::info(text = "Computer says no.")
        shinyjs::disable(id = "NEXT_care")
    } else {
        shinyjs::enable(id = "NEXT_care")
        shinyBS::closeAlert(session, alertId = "alertId_uCARE")
    }
})

observeEvent(input$uART, {
    if(input$uART > input$uCARE) {
        # shinyBS alert
        shinyBS::createAlert(session,
            anchorId = "uART_ALERT",
            alertId = "alertId_uART",
            title = paste(icon("exclamation-triangle", class = "fa-lg fa-fw", lib = "font-awesome"), "WARNING"),
            content = "Value entered for 'PLHIV on ART' exceeds the value of 'PLHIV in Care' in this year.",
            style = "danger",
            dismiss = TRUE,
            append = TRUE)
        # JS alert & disable
        shinyjs::info(text = "Computer says no.")
        shinyjs::disable(id = "NEXT_art")
    } else {
        shinyjs::enable(id = "NEXT_art")
        shinyBS::closeAlert(session, alertId = "alertId_uART")
    }
})

observeEvent(input$uVIRAL, {
    if(input$uVIRAL > input$uART) {
        # shinyBS alert
        shinyBS::createAlert(session,
            anchorId = "uVIRAL_ALERT",
            alertId = "alertId_uVIRAL",
            title = paste(icon("exclamation-triangle", class = "fa-lg fa-fw", lib = "font-awesome"), "WARNING"),
            content = "Value entered for 'PLHIV Virally Suppressed' exceeds the value of 'PLHIV on ART' in this year.",
            style = "danger",
            dismiss = TRUE,
            append = TRUE)
        # JS alert & disable
        shinyjs::info(text = "Computer says no.")
        shinyjs::disable(id = "NEXT_viral")
    } else {
        shinyjs::enable(id = "NEXT_viral")
        shinyBS::closeAlert(session, alertId = "alertId_uVIRAL")
    }
})
