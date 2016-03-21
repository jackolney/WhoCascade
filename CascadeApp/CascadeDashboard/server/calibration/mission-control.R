# Observe Functions for Mission Control
observeEvent(input$selectCountry, {

    if(CheckCSV_Incidence(input$selectCountry)) {
        updateButton(session, inputId = "_Incidence_FLAG_",  style = "success", icon = icon("check", class = "fa-lg fa-fw", lib = "font-awesome"))
    } else {
        updateButton(session, inputId = "_Incidence_FLAG_",  style = "danger",  icon = icon("times", class = "fa-lg fa-fw", lib = "font-awesome"))
    }

    if(CheckCSV_CD4(input$selectCountry)) {
        updateButton(session, inputId = "_CD4_FLAG_",        style = "success", icon = icon("check", class = "fa-lg fa-fw", lib = "font-awesome"))
    } else {
        updateButton(session, inputId = "_CD4_FLAG_",        style = "danger",  icon = icon("times", class = "fa-lg fa-fw", lib = "font-awesome"))
    }

    if(CheckCSV_Treatment(input$selectCountry)) {
        updateButton(session, inputId = "_Treatment_FLAG_",  style = "success", icon = icon("check", class = "fa-lg fa-fw", lib = "font-awesome"))
    } else {
        updateButton(session, inputId = "_Treatment_FLAG_",  style = "danger",  icon = icon("times", class = "fa-lg fa-fw", lib = "font-awesome"))
    }

    if(CheckCSV_PLHIV(input$selectCountry)) {
        updateButton(session, inputId = "_PLHIV_FLAG_",      style = "success", icon = icon("check", class = "fa-lg fa-fw", lib = "font-awesome"))
    } else {
        updateButton(session, inputId = "_PLHIV_FLAG_",      style = "danger",  icon = icon("times", class = "fa-lg fa-fw", lib = "font-awesome"))
    }

    if(CheckCSV_ART(input$selectCountry)) {
        updateButton(session, inputId = "_ART_FLAG_",        style = "success", icon = icon("check", class = "fa-lg fa-fw", lib = "font-awesome"))
    } else {
        updateButton(session, inputId = "_ART_FLAG_",        style = "danger",  icon = icon("times", class = "fa-lg fa-fw", lib = "font-awesome"))
    }

    if(CheckCSV_Additional(input$selectCountry)) {
        updateButton(session, inputId = "_Additional_FLAG_", style = "success", icon = icon("check", class = "fa-lg fa-fw", lib = "font-awesome"))
    } else {
        updateButton(session, inputId = "_Additional_FLAG_", style = "danger",  icon = icon("times", class = "fa-lg fa-fw", lib = "font-awesome"))
    }

    if(CheckCSV_Rate(input$selectCountry)) {
        updateButton(session, inputId = "_Rates_FLAG_",      style = "success", icon = icon("check", class = "fa-lg fa-fw", lib = "font-awesome"))
    } else {
        updateButton(session, inputId = "_Rates_FLAG_",      style = "danger",  icon = icon("times", class = "fa-lg fa-fw", lib = "font-awesome"))
    }

    # This will need to call the GetMasterDataSet function() at this point.
    # But After, calibration checks are all made.
    # Using correct Country List

})
