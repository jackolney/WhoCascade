tabItem(tabName = "retention",
    column(width = 8,
        box(width = NULL,
            status = "primary",
            solidHeader = TRUE,
            title = "People Retained on ART",
            collapsible = TRUE,
            collapsed = FALSE,
            helpText("Hello.")
        )
    ),
    column(width = 4,
        box(width = NULL,
            status = "warning",
            solidHeader = TRUE,
            title = "Help Panel",
            helpText("Please fill in all boxes with relevant data, then hit 'SAVE' and wait for the confirmation below.
                Hit 'RESET' to reset all values to zero, and hit 'DEMO' for a random set of values to be generated.
                Unchecking the 'HIV incidence' checkbox prevents any new infections occurring in the model.")
        ),
        bsButton(inputId = "wizardCalibration", label = "Next", style = "success", size = "large", block = TRUE, icon = icon("arrow-right", class = "fa-lg fa-fw", lib = "font-awesome"))
    )
)
