tabItem(tabName = "calibration",
    column(width = 8,
        box(width = NULL,
            status = "primary",
            solidHeader = TRUE,
            title = "Calibration Result",
            collapsible = TRUE,
            collapsed = FALSE,
            plotOutput('plotCalibration', height = 'auto', width = 'auto')
        ),
        fluidRow(
            box(width = 6,
                solidheader = TRUE,
                status = "primary",
                background = "light-blue",
                title = "Diagnosis Rate (py^-1)",
                collapsible = TRUE,
                collapsed = FALSE,
                numericInput("test_DiagRate_U","Upper:", value = 0, min = 0, max = 100, step = 1e-6, width = '100%'),
                numericInput("test_DiagRate_L","Lower:", value = 0, min = 0, max = 100, step = 1e-6, width = '100%')
            ),
            box(width = 6,
                solidheader = TRUE,
                status = "primary",
                background = "light-blue",
                title = "Linkage Rate (py^-1)",
                collapsible = TRUE,
                collapsed = FALSE,
                numericInput("test_LinkRate_U","Upper:", value = 0, min = 0, max = 100, step = 1e-6, width = '100%'),
                numericInput("test_LinkRate_L","Lower:", value = 0, min = 0, max = 100, step = 1e-6, width = '100%')
            )
        )
    ),
    column(width = 4,
        box(width = NULL,
            status = "warning",
            solidHeader = TRUE,
            collapsible = FALSE,
            collapsed = FALSE,
            title = "Calibration Progress",
            uiOutput(outputId = "progressOne"),
            uiOutput(outputId = "progressTwo"),
            uiOutput(outputId = "progressThree"),
            uiOutput(outputId = "progressFour")
        ),
        box(width = NULL,
            background = "yellow",
            solidHeader = TRUE,
            title = "Confirm Calibration",
            "Hit 'Accept' if you are content with the calibration figure and 'ghost-values' presented. If you are not happy with either, then please edit the ghost-values and hit 'Repeat'",
            bsButton(inputId = "calib_accept", label = "Accept", style = "success", size = "large", block = TRUE, icon = icon("check", class = "fa-lg fa-fw", lib = "font-awesome")),
            bsButton(inputId = "calib_repeat", label = "Repeat", style = "danger", size = "large", block = TRUE, icon = icon("repeat", class = "fa-lg fa-fw", lib = "font-awesome"))
        )
    )
)
