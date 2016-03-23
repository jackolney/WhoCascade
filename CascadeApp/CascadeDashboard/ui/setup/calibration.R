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
        # Row ONE
        fluidRow(
            box(width = 6,
                solidheader = TRUE,
                status = "primary",
                background = "light-blue",
                title = "Diagnosis Rate",
                collapsible = TRUE,
                collapsed = TRUE,
                "Rate at which undiagnosed individuals become diagnosed (py^-1).",
                numericInput("test_DiagRate_U","Upper:", value = 0, min = 0, max = 100, step = 1e-6, width = '100%'),
                numericInput("test_DiagRate_L","Lower:", value = 0, min = 0, max = 100, step = 1e-6, width = '100%')
            ),
            box(width = 6,
                solidheader = TRUE,
                status = "primary",
                background = "light-blue",
                title = "Proportion Linking to Care",
                collapsible = TRUE,
                collapsed = TRUE,
                "Proportion of diagnosed individuals that link to care immediately.",
                numericInput("test_LinkRate_U","Upper:", value = 0, min = 0, max = 100, step = 1e-6, width = '100%'),
                numericInput("test_LinkRate_L","Lower:", value = 0, min = 0, max = 100, step = 1e-6, width = '100%')
            )
        ),
        # Row TWO
        fluidRow(
            box(width = 6,
                solidheader = TRUE,
                status = "primary",
                background = "light-blue",
                title = "ART Initiation Rate (In Care)",
                collapsible = TRUE,
                collapsed = TRUE,
                "ART initiation rate for diagnosed individuals engaged in pre-ART care (py^-1).",
                numericInput("test_DiagRate_U","Upper:", value = 0, min = 0, max = 100, step = 1e-6, width = '100%'),
                numericInput("test_DiagRate_L","Lower:", value = 0, min = 0, max = 100, step = 1e-6, width = '100%')
            ),
            box(width = 6,
                solidheader = TRUE,
                status = "primary",
                background = "light-blue",
                title = "ART Initiation Rate (Not In Care)",
                collapsible = TRUE,
                collapsed = TRUE,
                "ART initiation rate individuals not currently engaged in pre-ART care. This is also a function of current CD4 count (py^-1).",
                numericInput("test_LinkRate_U","Upper:", value = 0, min = 0, max = 100, step = 1e-6, width = '100%'),
                numericInput("test_LinkRate_L","Lower:", value = 0, min = 0, max = 100, step = 1e-6, width = '100%')
            )
        ),
        # Row THREE
        fluidRow(
            box(width = 6,
                solidheader = TRUE,
                status = "primary",
                background = "light-blue",
                title = "Pre-ART Dropout Rate",
                collapsible = TRUE,
                collapsed = TRUE,
                "Rate at which diagnosed individuals disengage from pre-ART care (py^-1).",
                numericInput("test_DiagRate_U","Upper:", value = 0, min = 0, max = 100, step = 1e-6, width = '100%'),
                numericInput("test_DiagRate_L","Lower:", value = 0, min = 0, max = 100, step = 1e-6, width = '100%')
            ),
            box(width = 6,
                solidheader = TRUE,
                status = "primary",
                background = "light-blue",
                title = "ART Disengagement Rate",
                collapsible = TRUE,
                collapsed = TRUE,
                "Rate at which individuals on treatment disengage from ART care (py^-1).",
                numericInput("test_LinkRate_U","Upper:", value = 0, min = 0, max = 100, step = 1e-6, width = '100%'),
                numericInput("test_LinkRate_L","Lower:", value = 0, min = 0, max = 100, step = 1e-6, width = '100%')
            )
        ),
        # Row FOUR
        fluidRow(
            box(width = 6,
                solidheader = TRUE,
                status = "primary",
                background = "light-blue",
                title = "Natural Mortality Rate",
                collapsible = TRUE,
                collapsed = TRUE,
                "Non-HIV mortality rate applied to all individuals (py^-1).",
                numericInput("test_DiagRate_U","Upper:", value = 0, min = 0, max = 100, step = 1e-6, width = '100%'),
                numericInput("test_DiagRate_L","Lower:", value = 0, min = 0, max = 100, step = 1e-6, width = '100%')
            ),
            box(width = 6,
                solidheader = TRUE,
                status = "primary",
                background = "light-blue",
                title = "Proportion Adhering to Treatment",
                collapsible = TRUE,
                collapsed = TRUE,
                "Proportion of individuals initiating ART that adhere to treatment and become virally suppressed.",
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
            p(""),
            bsButton(inputId = "calib_accept", label = "Accept", style = "success", size = "large", block = TRUE, icon = icon("check",  class = "fa-lg fa-fw", lib = "font-awesome")),
            bsButton(inputId = "calib_repeat", label = "Repeat", style = "danger",  size = "large", block = TRUE, icon = icon("repeat", class = "fa-lg fa-fw", lib = "font-awesome"))
        )
    )
)
