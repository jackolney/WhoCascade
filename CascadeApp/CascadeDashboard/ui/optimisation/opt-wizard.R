tabItem(tabName = "opt-wizard",
    column(width = 8,
        box(width = NULL,
            status = "primary",
            # background = "yellow",
            solidHeader = TRUE,
            title = "Results",
            height = "500px",
            plotOutput('plotCascade_wizard')
        ),
        fluidRow(
            box(
                title = "Incidence",
                height = "300px",
                width = 6,
                status = "warning",
                solidHeader = TRUE,
                plotOutput('plotNewInf_wizard')
            ),
            box(
                title = "AIDS Deaths",
                height = "300px",
                width = 6,
                status = "warning",
                solidHeader = TRUE,
                plotOutput('plotAidsDeaths_wizard')
            )
        ),
        valueBoxOutput("vb_90_wizard"),
        valueBoxOutput("vb_9090_wizard"),
        valueBoxOutput("vb_909090_wizard")
    ),
    column(width = 4,
        bsButton(inputId = "testStart", label = "Start", style = "success", size = "large", block = TRUE, icon = icon("play", class = "fa-lg fa-fw", lib = "font-awesome")),
        bsButton(inputId = "testStop",  label = "Stop",  style = "danger",  size = "large", block = TRUE, icon = icon("stop", class = "fa-lg fa-fw", lib = "font-awesome")),
        br(),
        box(width = NULL,
            status = "warning",
            solidHeader = TRUE,
            title = "Testing",
            sliderInput('rho', 'Diagnosis rate (diagnoses/py) (rho):', min = 0, max = 5, value = 0.205, step = 0.001, width = 1000)
            ),
        box(width = NULL,
            status = "warning",
            solidHeader = TRUE,
            title = "Care Seeking",
            sliderInput('epsilon', 'Care seeking rate (persons seeking care/py) (epsilon):', min = 0, max = 20, value = 16.949, step = 0.001, width = 1000)
            ),
        box(width = NULL,
            status = "warning",
            solidHeader = TRUE,
            title = "ART Initiation",
            sliderInput('gamma', 'ART initiation rate (ART initiations/py) (gamma):', min = 0, max = 5, value = 2.556, step = 0.001, width = 1000)
            ),
        box(width = NULL,
            status = "warning",
            solidHeader = TRUE,
            title = "Retention",
            sliderInput('omega', 'ART dropout rate (ART dropout/py) (omega):', min = 0, max = 5, value = 0.033, step = 0.001, width = 1000)
            ),
        bsButton(inputId = "NEXT_optWizard", label = "Next", style = "success", size = "large", block = TRUE, icon = icon("arrow-right", class = "fa-lg fa-fw", lib = "font-awesome"))
    )
)
