tabItem(tabName = "suppression",
    column(width = 8,
        box(width = NULL,
            status = "primary",
            solidHeader = TRUE,
            title = "People Virally Suppressed on ART",
            collapsible = TRUE,
            collapsed = FALSE,
            # background = "blue",
            div(img(src = "si-indicators/suppression.png", height = '30%', width = '30%'), style="text-align: center;"),
            br(),
            numericInput("uVIRAL", "Number of people on ART and Virally Suppressed:", value = 0, min = 0, width = '100%'),
            selectInput("uVIRAL_source", "Source of Data:", SourceList, selected = "Please select source..."),
            selectInput("uVIRAL_year", "Year data represents:", YearList, selected = 2015)
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
            a(href = "http://who.int/hiv/pub/guidelines/strategic-information-guidelines/en/", "WHO - Consolidated Strategic Information Guidelines for HIV in the Health Sector."),
            br(),
            uiOutput(outputId = "uVIRAL_quality")
        ),
        bsButton(inputId = "NEXT_viral", label = "Next", style = "success", size = "large", block = TRUE, icon = icon("arrow-right", class = "fa-lg fa-fw", lib = "font-awesome"))
    )
)
