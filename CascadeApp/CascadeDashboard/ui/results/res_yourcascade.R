tabItem(tabName = "your_cascade",
    column(width = 8,
        # h1(textOutput('CountryName')),
        tabBox(
            width = NULL,
            height = "600px",
            title = "Cascade Overview",
            selected = "PLHIV",
            side = "left",

            tabPanel(title = "PLHIV",
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

            tabPanel(title = "Diagnosed",
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
            ),

            tabPanel(title = "Care",
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

            tabPanel(title = "Treatment",
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
            ),

            tabPanel(title = "Suppression",
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
        bsButton(inputId = "wizardResults_2", label = "Next", style = "success", size = "large", block = TRUE, icon = icon("arrow-right", class = "fa-lg fa-fw", lib = "font-awesome"))
    )
)
