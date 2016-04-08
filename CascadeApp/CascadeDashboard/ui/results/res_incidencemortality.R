tabItem(tabName = "incidence_mortality",
    column(width = 8,
        tabBox(
            width = NULL,
            height = "600px",
            title = "Results",
            tabPanel("Incidence",
                plotOutput('plotNewInf', height = "500px")
            ),
            tabPanel("AIDS Deaths",
                plotOutput('plotAidsDeaths', height = "500px")
            )
        )
    ),
    column(width = 4,
        box(width = NULL,
            background = "yellow",
            solidHeader = TRUE,
            title = "New Infections & Mortality",
            h4("New Infections"),
            "Predictions of incident infections between 2015 and 2020, illustrated as a proportion of the total HIV-positive population.",
            br(),
            h4("AIDS Deaths"),
            "Predictions of AIDS deaths between 2015 and 2020, illustrated as a proportion of the total HIV-positive population."
        ),
        bsButton(inputId = "NEXT_incMort", label = "Next", style = "success", size = "large", block = TRUE, icon = icon("arrow-right", class = "fa-lg fa-fw", lib = "font-awesome"))
    )
)
