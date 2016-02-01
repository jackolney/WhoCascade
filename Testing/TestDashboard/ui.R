library(shiny)
library(shinydashboard)

ui <- dashboardPage(
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
      box(width = 4, actionButton("count", "Increment progress"))
    ),
    fluidRow(
      box(width = NULL,
        sliderInput('rho','Diagnosis rate (diagnoses/py) (rho):',
                min = 0,
                max = 5,
                value = 0.205,
                step = 0.001,
                width = 1000),
        tags$div(
          class = "slider.slider-horizontal",
          class = "slider-track",
          `data-slider-min` = "-200",
          `data-slider-max` = "200",
          `data-slider-step` = "5",
          `data-slider-value` = "100",
          `data-slider-orientation` = "horizontal",
          `data-slider-selection` = "before",
          `data-slider-tooltip` = "show",
          href = "slider.css"
        )
      )
    )
  )
)
