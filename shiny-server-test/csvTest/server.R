if (version$os == "darwin13.4.0") {
    loc <- "/Library/Frameworks/R.framework/Versions/3.3/Resources/library"
} else if (version$os == "linux-gnu") {
    loc <- "/home/DIDE/jjo11/R/x86_64-pc-linux-gnu-library/3.2/"
}

library(shiny,          lib.loc = loc)
library(shinydashboard, lib.loc = loc)
library(readr,          lib.loc = loc)
library(shinyBS,        lib.loc = loc)

shinyServer(function(input, output, session){
    output$progressBox <- renderValueBox({
        valueBox(
            paste0(25 + input$count, "%"), "Progress", icon = icon("medkit", lib = "font-awesome"),
            color = "purple"
        )
    })

    output$approvalBox <- renderValueBox({
        valueBox(
            "80%", "Approval", icon = icon("thumbs-up", lib = "glyphicon"),
            color = "yellow"
        )
    })

    CheckCSV_Incidence <- function() {
        # data <- readr::read_csv("incident-infections.csv", col_names = TRUE, skip = 1)
        data <- read.csv("incident-infections.csv",  skip = 1)
        if(is.null(data)) {
            return(FALSE)
        } else {
            return(TRUE)
        }
    }

    observeEvent(input$check_csv, {
        if(CheckCSV_Incidence()) {
            updateButton(session, inputId = "read_csv",  style = "success", icon = icon("check", class = "fa-lg fa-fw", lib = "font-awesome"))
        } else {
            updateButton(session, inputId = "read_csv",  style = "danger",  icon = icon("times", class = "fa-lg fa-fw", lib = "font-awesome"))
        }
    })

})

