tabItem(tabName = "linkage",
    column(width = 8,
        shinyjs::useShinyjs(),
        box(width = NULL,
            status = "primary",
            solidHeader = TRUE,
            title = "People in HIV Care",
            collapsible = TRUE,
            collapsed = FALSE,
            # background = "blue",
            div(img(src = "si-indicators/linkage.png", height = '30%', width = '30%'), style="text-align: center;"),
            br(),
            id = 'care_panel',
            uiOutput(outputId = "UI_uCARE"),
            uiOutput(outputId = "UI_uCARE_source"),
            uiOutput(outputId = "UI_uCARE_year"),
            uiOutput(outputId = "uCARE_quality")
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
            a(href = "http://who.int/hiv/pub/guidelines/strategic-information-guidelines/en/", "WHO - Consolidated Strategic Information Guidelines for HIV in the Health Sector.", target = "_blank"),
            p(""),
            bsButton(inputId = "resetCARE", label = "RESET", style = "danger", size = "default", block = TRUE)
        ),
        bsButton(inputId = "NEXT_care", label = "Next", style = "success", size = "large", block = TRUE, icon = icon("arrow-right", class = "fa-lg fa-fw", lib = "font-awesome"))
    )
)
