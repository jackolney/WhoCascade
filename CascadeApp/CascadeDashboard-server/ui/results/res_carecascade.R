tabItem(tabName = "care_cascade",
    column(width = 8,
        box(width = NULL,
            status = "primary",
            solidHeader = TRUE,
            title = "Care Cascade",
            plotOutput('plotCascade')
        )
    ),
    column(width = 4,
        box(width = NULL,
            background = "yellow",
            solidHeader = TRUE,
            title = "The distribution of care between 2015 and 2020",
            "Note, the denominator in all these calculations is # of PLHIV.",
            "These figures illustrate the 'Care Cascade' in 2015 (at baseline), and the projection after 5 years (in 2020)."
        ),
        bsButton(inputId = "NEXT_careCascade", label = "Next", style = "success", size = "large", block = TRUE, icon = icon("arrow-right", class = "fa-lg fa-fw", lib = "font-awesome"))
    )
)
