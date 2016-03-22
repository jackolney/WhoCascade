tabItem(tabName = "calibration",
    column(width = 8,
        box(width = NULL,
            status = "primary",
            solidHeader = TRUE,
            title = "Calibration Result",
            collapsible = TRUE,
            collapsed = FALSE
            # Here will be the output plot from the calibration
        )
        # PROGRESS BARS HERE.
    ),
    column(width = 4,
        box(width = NULL,
            background = "yellow",
            solidHeader = TRUE,
            title = "Confirm?",
            bsButton(inputId = "calib_accept", label = "Accept", style = "success", size = "large", block = TRUE, icon = icon("check", class = "fa-lg fa-fw", lib = "font-awesome")),
            bsButton(inputId = "calib_reject", label = "Reject", style = "danger", size = "large", block = TRUE, icon = icon("times", class = "fa-lg fa-fw", lib = "font-awesome"))
        )
    )
)
