Tab_YourCascade <- tabItem(tabName = "your_cascade",
    h1(textOutput('CountryName')),
    fluidRow(
        box(width = 6,
            status = "primary",
            tags$b("Number of PLHIV:"), 
            p(""),
            verbatimTextOutput("outPLHIV"),
            tags$b("Percentage of total PLHIV:"), 
            p(""),
            verbatimTextOutput("outPLHIV_perc"),
            p(""),
            plotOutput('plotValidation_PLHIV',
                height = 'auto',
                width = 'auto')
        ),
        box(width = 6,
            status = "primary",
            tags$b("Number of PLHIV who have been diagnosed:"),
            p(""),
            verbatimTextOutput("outDIAG"),
            tags$b("Percentage of PLHIV who have been diagnosed:"), 
            p(""),
            verbatimTextOutput("outDIAG_perc"),
            p(""),
            plotOutput('plotValidation_DIAG',
                height = 'auto',
                width = 'auto')
        )
    ),

    fluidRow(
        box(width = 6,
            status = "primary",
            tags$b("Number of PLHIV in HIV care (including ART):"),
            p(""),
            verbatimTextOutput("outCARE"),
            tags$b("Percentage of PLHIV in HIV care (including ART):"),
            p(""),
            verbatimTextOutput("outCARE_perc"),
            p(""),
            plotOutput('plotValidation_CARE',
                height = 'auto',
                width = 'auto')
        ),
        box(width = 6,
            status = "primary",
            tags$b("Number of PLHIV in HIV care and on ART:"),
            p(""),
            verbatimTextOutput("outART"),
            tags$b("Percentage of PLHIV in HIV care and on ART:"),
            p(""),
            verbatimTextOutput("outART_perc"),
            p(""),
            plotOutput('plotValidation_ART',
                height = 'auto',
                width = 'auto')
        )
    ),

    fluidRow(
        box(width = 6,
            status = "primary",
            tags$b("Number of PLHIV in HIV care, on ART and virally suppressed:"),
            p(""),
            verbatimTextOutput("outSUPP"),
            tags$b("Percentage of PLHIV in HIV care, on ART and virally suppressed:"),
            p(""),
            verbatimTextOutput("outSUPP_perc"),
            p(""),
            plotOutput('plotValidation_SUPP',
                height = 'auto',
                width = 'auto')
        ),
        box(width = 6,
            status = "primary",
            tags$b("Number of PLHIV who dropped out of ART care:"),
            p(""),
            verbatimTextOutput("outLTFU"),
            tags$b("Percentage of PLHIV who dropped out of ART care:"),
            p(""),
            verbatimTextOutput("outLTFU_perc"),
            p(""),
            plotOutput('plotValidation_LTFU',
                height = 'auto',
                width = 'auto')
        )
    )
)

Tab_CareCascade <- tabItem(tabName = "care_cascade",
    fluidRow(
        box(width = NULL,
            status = "primary",
            plotOutput('plotCascade')
        )
    ),
    box(width = NULL,
        status = "info",
        h4("The distribution of care between 2015 and 2020."),
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
)

Tab_PowersCascade <- tabItem(tabName = "powers_cascade",
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
    )
)