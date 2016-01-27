tabItem(tabName = "incidence_mortality",
    column(width = 4,
        box(width = NULL,
            status = "warning",
            solidHeader = FALSE,
            h4("New Infections"),
            p("Predictions of incident infections between 2015 and 2020, illustrated as a proportion of the total HIV-positive population."),
            p(""),
            h4("AIDS Deaths"),
            p("Predictions of AIDS deaths between 2015 and 2020, illustrated as a proportion of the total HIV-positive population.")
        ),
        bsButton(inputId = "wizardOpt_1", label = "Next", style = "success", size = "large", block = TRUE, icon = icon("arrow-right", class = "fa-lg fa-fw", lib = "font-awesome"))
    ),
    column(width = 8,
        tabBox(
            width = NULL,
            height = "600px",
            title = "Results",
            tabPanel("Incidence",
                plotOutput('plotNewInf')
            ),
            tabPanel("AIDS Deaths",
                plotOutput('plotAidsDeaths')
            )
        )
    )
)
