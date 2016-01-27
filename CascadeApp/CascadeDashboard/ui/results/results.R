tabItem(tabName = "your_cascade",
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
    ),
    bsButton(inputId = "wizardResults_2", label = "Next", style = "success", size = "large", block = TRUE, icon = icon("arrow-right", class = "fa-lg fa-fw", lib = "font-awesome"))
)

tabItem(tabName = "care_cascade",
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
    ),
    bsButton(inputId = "wizardResults_3", label = "Next", style = "success", size = "large", block = TRUE, icon = icon("arrow-right", class = "fa-lg fa-fw", lib = "font-awesome"))
)

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

tabItem(tabName = "_909090",
    column(width = 4,
        box(width = NULL,
            status = "warning",
            solidHeader = FALSE,
            h4("UNAIDS 90-90-90"),
            p("By 2020, this is what the model predicts will be achieved in comparison to the UNAIDS goals of 90% diagnosed,
                90% on treatment and 90% virally suppressed. If you would like to see what changes can be made to resolve any
                inefficiencies in care, then click on the 'Optimisation' tab.")
        ),
        bsButton(inputId = "wizardResults_5", label = "Next", style = "success", size = "large", block = TRUE, icon = icon("arrow-right", class = "fa-lg fa-fw", lib = "font-awesome"))
    ),
    column(width = 8,
        box(width = NULL,
            status = "primary",
            plotOutput('plot909090')
        ),
        valueBoxOutput("vb_90"),
        valueBoxOutput("vb_9090"),
        valueBoxOutput("vb_909090")
    )
)

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
