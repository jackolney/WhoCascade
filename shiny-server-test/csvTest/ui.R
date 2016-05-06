if (version$os == "darwin13.4.0") {
    loc <- "/Library/Frameworks/R.framework/Versions/3.3/Resources/library"
} else if (version$os == "linux-gnu") {
    loc <- "/home/DIDE/jjo11/R/x86_64-pc-linux-gnu-library/3.2/"
}

library(shinydashboard, lib.loc = loc)
library(shinyBS,        lib.loc = loc)

dashboardPage(
    dashboardHeader(title = "Value boxes"),
    dashboardSidebar(),
    dashboardBody(
        fluidRow(
            # A static valueBox
            valueBox(10 * 2, "New Orders", icon = icon("credit-card")),

            # Dynamic valueBoxes
            valueBoxOutput("progressBox"),

            valueBoxOutput("approvalBox")
        ),
        fluidRow(
            # Clicking this will increment the progress amount
            box(width = 4,
                actionButton("count", "Increment progress")
            )
        ),
        fluidRow(
            box(width = 4,
                bsButton("check_csv", label = "Check .csv", style = "success", size = "large", block = TRUE),
                bsButton("read_csv",  label = "",           style = "warning", size = "large", block = TRUE)
            )
        )
    )
)

