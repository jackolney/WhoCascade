# StartUp Caution Alert
createAlert(session,
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
addPopover(session, id = "plotOpt909090",
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
        createAlert(session,
            anchorId = "uDIAG_ALERT",
            alertId = NULL,
            title = paste(icon("exclamation-triangle", class = "fa-lg fa-fw", lib = "font-awesome"), "WARNING"),
            content = "Value entered for 'PLHIV Diagnosed' exceeds the value of 'PLHIV' in this year.",
            style = "danger",
            dismiss = TRUE,
            append = TRUE)
        # JS alert
        shinyjs::info("Computer says no.")
    } else return()
})

observeEvent(input$uCARE, {
    if(input$uCARE > input$uDIAG) {
        # shinyBS alert
        createAlert(session,
            anchorId = "uCARE_ALERT",
            alertId = NULL,
            title = paste(icon("exclamation-triangle", class = "fa-lg fa-fw", lib = "font-awesome"), "WARNING"),
            content = "Value entered for 'PLHIV in Care' exceeds the value of 'PLHIV Diagnosed' in this year.",
            style = "danger",
            dismiss = TRUE,
            append = TRUE)
        # JS alert
        shinyjs::info("Computer says no.")
    } else return()
})

observeEvent(input$uART, {
    if(input$uART > input$uCARE) {
        # shinyBS alert
        createAlert(session,
            anchorId = "uART_ALERT",
            alertId = NULL,
            title = paste(icon("exclamation-triangle", class = "fa-lg fa-fw", lib = "font-awesome"), "WARNING"),
            content = "Value entered for 'PLHIV on ART' exceeds the value of 'PLHIV in Care' in this year.",
            style = "danger",
            dismiss = TRUE,
            append = TRUE)
        # JS alert
        shinyjs::info("Computer says no.")
    } else return()
})

observeEvent(input$uVIRAL, {
    if(input$uVIRAL > input$uART) {
        # shinyBS alert
        createAlert(session,
            anchorId = "uVIRAL_ALERT",
            alertId = NULL,
            title = paste(icon("exclamation-triangle", class = "fa-lg fa-fw", lib = "font-awesome"), "WARNING"),
            content = "Value entered for 'PLHIV Virally Suppressed' exceeds the value of 'PLHIV on ART' in this year.",
            style = "danger",
            dismiss = TRUE,
            append = TRUE)
        # JS alert
        shinyjs::info("Computer says no.")
    } else return()
})
