tabItem(tabName = "country",
    column(width = 8,
        box(width = NULL,
            status = "primary",
            solidHeader = TRUE,
            title = "Country",
            collapsible = TRUE,
            collapsed = FALSE,
            shinyjs::useShinyjs(),
            id = "country-panel",
            helpText("Please select country from map or from dropdown menu.")
            # Here we insert the leaflet();
        ),
        box(width = NULL
            # Here we have the checkInput stuff.
        )
    ),
    column(width = 4,
        box(width = NULL,
            status = "warning",
            solidHeader = TRUE,
            title = "Help Panel",
            helpText("Just pick a country."),
            bsButton("resetInput", label = "RESET MAP", style = "danger", block = TRUE, size = "default")
        ),
        bsButton(inputId = "_BLANK_", label = "Next", style = "success", size = "large", block = TRUE, icon = icon("arrow-right", class = "fa-lg fa-fw", lib = "font-awesome"))
        # Perhaps a collapsible 'master check' warning panel, like at mission control. Call it "mission control".
        # It will contain a narrow and tall list of all the imported csv files, and will have deactivated buttons that will change colour depending on what gets loaded.
        # SUPER AWESOME.
    )
)
