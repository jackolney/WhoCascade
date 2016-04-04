tabItem(tabName = "diagnosis",
    column(width = 8,
        box(width = NULL,
            status = "primary",
            solidHeader = TRUE,
            title = "People Diagnosed with HIV",
            collapsible = TRUE,
            collapsed = FALSE,
            # background = "blue",
            div(img(src = "si-indicators/diagnosed.png", height = '30%', width = '30%'), style="text-align: center;"),
            br(),
            uiOutput(outputId = "UI_uDIAG"),
            uiOutput(outputId = "UI_uDIAG_source"),
            uiOutput(outputId = "UI_uDIAG_year")
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
            uiOutput(outputId = "uDIAG_quality")
        ),
        bsButton(inputId = "NEXT_diag", label = "Next", style = "success", size = "large", block = TRUE, icon = icon("arrow-right", class = "fa-lg fa-fw", lib = "font-awesome"))
    )
)
