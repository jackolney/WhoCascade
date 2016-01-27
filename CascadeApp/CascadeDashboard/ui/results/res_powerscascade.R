tabItem(tabName = "powers_cascade",
    fluidRow(
        box(width = NULL,
            status = "primary",
            plotOutput('plotPowersCascade')
        )
    ),
    box(width = NULL,
        status = "info",
        h4("The distribution of care between 2015 and 2020."),
        helpText("Note, the denominator in all these calculations is # of PLHIV."),
        p("These figures illustrate the 'Care Cascade' in 2015 (at baseline), and the projection after 5 years (in 2020)."),
        helpText("Figures are based on those found in Powers et al. (2015)."),
        tags$ol(
            tags$li("% Undiagnosed = # persons undiagnosed / PLHIV"),
            tags$li("% Diagnosed = # persons diagnosed and not in care / PLHIV"),
            tags$li("% In Care = # persons diagnosed, in care, not on ART / PLHIV"),
            tags$li("% On Treatment (non-adherent) = # persons diagnosed, in care, on ART, but not adhering and not virally suppressed / PLHIV"),
            tags$li("% On Treatment (adherent) = # persons diagnosed, in care, on ART, adhering, but not virally suppressed / PLHIV"),
            tags$li("% Virally Suppressed = # persons diagnosed, in care, on ART, virally suppressed / PLHIV"),
            tags$li("% LTFU = # persons diagnosed, not in care, dropped out of ART / PLHIV.")
        )
    ),
    bsButton(inputId = "wizardResults_4", label = "Next", style = "success", size = "large", block = TRUE, icon = icon("arrow-right", class = "fa-lg fa-fw", lib = "font-awesome"))
)
