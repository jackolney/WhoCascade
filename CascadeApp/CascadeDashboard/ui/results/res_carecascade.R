tabItem(tabName = "care_cascade",
    column(width = 12,
        box(width = NULL,
            status = "primary",
            plotOutput('plotCascade')
        ),
        box(width = NULL,
            status = "primary",
            solidHeader = TRUE,
            title = "The distribution of care between 2015 and 2020",
            helpText("Note, the denominator in all these calculations is # of PLHIV."),
            p("These figures illustrate the 'Care Cascade' in 2015 (at baseline), and the projection after 5 years (in 2020)."),
            tags$ol(
                tags$li("% diagnosed = # persons diagnosed / PLHIV"),
                tags$li("% in care = # persons in care (including on ART & virally suppressed) / PLHIV"),
                tags$li("% on treatment = # persons on ART (including those virally suppressed) / PLHIV"),
                tags$li("% virally suppressed = # persons on ART and virally suppressed / PLHIV"),
                tags$li("% LTFU = # persons lost from ART care / PLHIV.")
            )
        )
    ),
    bsButton(inputId = "wizardResults_3", label = "Next", style = "success", size = "large", block = TRUE, icon = icon("arrow-right", class = "fa-lg fa-fw", lib = "font-awesome"))
)
