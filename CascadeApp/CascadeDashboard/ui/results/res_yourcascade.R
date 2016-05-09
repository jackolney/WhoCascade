tabItem(tabName = "your_cascade",
    column(width = 8,
        # h1(textOutput('CountryName')),
        tabBox(
            width = NULL,
            height = "600px",
            title = icon("bar-chart", class = "fa-lg fa-fw", lib = "font-awesome"),
            selected = "PLHIV",
            side = "left",

            tabPanel(title = "PLHIV",
                tags$b("Mean number of PLHIV:"),
                uiOutput("outPLHIV"),
                p(""),
                tags$b("Range of values:"),
                uiOutput("outPLHIV_perc"),
                p(""),
                plotOutput('plotValidation_PLHIV',
                    height = 'auto',
                    width = 'auto')
            ),

            tabPanel(title = "Diagnosed",
                tags$b("Mean of PLHIV who have been diagnosed:"),
                uiOutput("outDIAG"),
                p(""),
                tags$b("Range of values:"),
                uiOutput("outDIAG_perc"),
                p(""),
                plotOutput('plotValidation_DIAG',
                    height = 'auto',
                    width = 'auto')
            ),

            tabPanel(title = "Care",
                tags$b("Mean of PLHIV in HIV care (including ART):"),
                uiOutput("outCARE"),
                p(""),
                tags$b("Range of values:"),
                uiOutput("outCARE_perc"),
                p(""),
                plotOutput('plotValidation_CARE',
                    height = 'auto',
                    width = 'auto')
            ),

            tabPanel(title = "Treatment",
                tags$b("Mean of PLHIV in HIV care and on ART:"),
                uiOutput("outART"),
                p(""),
                tags$b("Range of values:"),
                uiOutput("outART_perc"),
                p(""),
                plotOutput('plotValidation_ART',
                    height = 'auto',
                    width = 'auto')
            ),

            tabPanel(title = "Suppression",
                tags$b("Mean of PLHIV in HIV care, on ART and virally suppressed:"),
                uiOutput("outSUPP"),
                p(""),
                tags$b("Range of values:"),
                uiOutput("outSUPP_perc"),
                p(""),
                plotOutput('plotValidation_SUPP',
                    height = 'auto',
                    width = 'auto')
            )
        )
    ),
    column(width = 4,
        box(width = NULL,
            # status = "warning",
            background = "yellow",
            solidHeader = TRUE,
            title = "Help Panel",
            "The breakdown of the cascade in 2015 is shown on on this page. Click 'Next' to advance to the next page."
        ),
        bsButton(inputId = "NEXT_yourCascade", label = "Next", style = "success", size = "large", block = TRUE, icon = icon("arrow-right", class = "fa-lg fa-fw", lib = "font-awesome"))
    )
)
