tabItem(tabName = "opt-intro",
    column(width = 4,
        box(width = NULL,
            status = "primary",
            solidHeader = TRUE,
            title = "Cost",
            p("Review or edit the unit costs in each box."),
            helpText("Click the 'optimisation' drop down menu and select 'results' to begin running the optimisation algorithm."),
            bsButton("resetCost", label = "RESET COST", block = TRUE, style = "danger"),
            p(" "),
            tableOutput("unitCostTable")
        ),
        bsButton(inputId = "wizardOpt_2", label = "Next", style = "success", size = "large", block = TRUE, icon = icon("arrow-right", class = "fa-lg fa-fw", lib = "font-awesome"))
    ),
    column(width = 8,
        shinyjs::useShinyjs(),
        id = "cost-panel",
        box(width = NULL,
            height = '100%',
            status = "warning",
            solidHeader = TRUE,
            sliderInput('userDxUnitCost','Unit cost of diagnosing a patient (USD):', min = 0, max = 100, value = 10, step = 1)
        ),
        box(width = NULL,
            height = '100%',
            status = "warning",
            solidHeader = TRUE,
            sliderInput('userLinkageUnitCost','Unit cost of linking a patient to care (USD):', min = 0, max = 100, value = 40, step = 1)
        ),
        box(width = NULL,
            height = '100%',
            status = "warning",
            solidHeader = TRUE,
            sliderInput('userAnnualCareUnit','Annual cost of keeping a patient in pre-ART care (USD):', min = 0, max = 100, value = 40, step = 1)
        ),
        box(width = NULL,
            height = '100%',
            status = "warning",
            solidHeader = TRUE,
            sliderInput('userAnnualARTUnitCost','Annual cost of ART (USD):', min = 0, max = 500, value = 367, step = 1)
        )
    )
)
