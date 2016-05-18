tabItem(tabName = "data-review",
    column(width = 8,
        shinyjs::useShinyjs(),
        box(width = NULL,
            status = "primary",
            solidHeader = TRUE,
            title = "Calibration Data Review",
            collapsible = TRUE,
            collapsed = FALSE,
            "This page will have a plot (here) that illustrates the currently loaded data in the model,
            from (MasterData). The user can review it and pull down a modal with all the values printed.
            The user will then be able to click either 'ENTER DATA' or 'CALIBRATE' and be taken to the
            relevant pages.",
            bsModal(id = "seeDataTable_DATA", title = "Data Table", trigger = "viewData_DATA", size = "large",
                DT::dataTableOutput('dataTable_DATA', width = "100%")
            ),
            plotOutput('plotData', height = 'auto', width = 'auto')
        )
    ),
    column(width = 4,
        box(width = NULL,
            status = "warning",
            solidHeader = TRUE,
            title = "Help Panel",
            helpText("Please fill in the boxes with details regarding each of the strategic information indicators,
                then select the source of the data from the drop-down menu below.
                Once entered hit 'Next' to proceed. For further details please see:"),
            bsButton(inputId = "viewData_DATA",   label = "VIEW DATA", style = "primary", size = "default", block = TRUE)
        ),
        fluidRow(
            column(width = 6,
                bsButton(inputId = "NEXT_data", label = "Enter Data", style = "danger",  size = "large", block = TRUE, icon = icon("database",  class = "fa-lg fa-fw", lib = "font-awesome"))
            ),
            column(width = 6,
                bsButton(inputId = "CALIB_data", label = "Calibrate", style = "success",  size = "large", block = TRUE, icon = icon("check",  class = "fa-lg fa-fw", lib = "font-awesome"))
            )
        )
    )
)
