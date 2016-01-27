Tab_Opt_Cost <- tabItem(tabName = "opt_cost",
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
        wellPanel(
            sliderInput('userDxUnitCost','Unit cost of diagnosing a patient (USD):',min=0,max=100,value=10,step=1)
            ),
        wellPanel(
            sliderInput('userLinkageUnitCost','Unit cost of linking a patient to care (USD):',min=0,max=100,value=40,step=1)
            ),
        wellPanel(
            sliderInput('userAnnualCareUnit','Annual cost of keeping a patient in pre-ART care (USD):',min=0,max=100,value=40,step=1)
            ),
        wellPanel(
            sliderInput('userAnnualARTUnitCost','Annual cost of ART (USD):',min=0,max=500,value=367,step=1)
            )
    )
)

Tab_Opt_Parameter <- tabItem(tabName = "opt_parameter",
    column(width = 4,
        box(width = NULL,
            status = "primary",
            solidHeader = TRUE,
            title = "Parameter Selection",
            p("The optimisation algorithm takes six model parameters representing six hypothetical interventions and simulates the cost and impact of all permutations."),
            helpText("For each intervention, select the number of parameter values to simulate and set the range of rates to sample from (rates are uniformally distributed within this range).
                The parameter values for each intervention are then displayed in the corresponding tables."),
            bsButton("resetSliders", label = "RESET SLIDERS", block = TRUE, style = "danger"),
            p(" "),
            helpText("Below is the number of iterations the model will simulate along with the estimated time to completion. Hit the 'OPTIMISE' button to begin the simulation. Note the progress bar
                at the top of the screen, and the run number and elapsed time on the top right. Please wait until the optimisation algorithm has completed the below bar has turned green before proceeding to the results tab."),
            bsButton("optimiseInput", label = "OPTIMISE", block = TRUE, size = "large", style = "primary"),
            bsTooltip(id = "optimiseInput", title = "Wait for progress bar to complete before proceeding.", placement = "right", trigger = "hover"),
            p(" "),
            tableOutput("optIterationTable"),
            bsButton("optFinished", label = "OPTIMISATION NOT RUN", style = "danger", block = TRUE, icon = icon("ban", class = "fa-lg fa-fw", lib = "font-awesome"), disabled = TRUE)
        ),
        bsButton(inputId = "wizardOpt_3", label = "Next", style = "success", size = "large", block = TRUE, icon = icon("arrow-right", class = "fa-lg fa-fw", lib = "font-awesome"))
    ),
    column(width = 8,
        shinyjs::useShinyjs(),
        id = "optimisation-panel",
        wellPanel(
            h4("HIV-Testing (rho)"),
            helpText("by varying diagnosis rate, rho"),
            sliderInput('userOptRho_LengthOf','Length of parameter range:',min=0,max=10,value=1,step=1),
            sliderInput('userOptRho_Range','Range of values (diagnoses/py):',min=0,max=50,value=c(0.205,20),step=0.001),
            tableOutput("optParTable_Rho")
            ),
        wellPanel(
            h4("Linkage (epsilon)"),
            helpText("by varying care seeking rate, epsilon"),
            sliderInput('userOptEpsilon_LengthOf','Length of parameter range:',min=0,max=10,value=1,step=1),
            sliderInput('userOptEpsilon_Range','Range of values (persons seeking care/py):',min=0,max=50,value=c(16.949,30),step=0.001),
            tableOutput("optParTable_Epsilon")
            ),
        wellPanel(
            h4("Pre-ART Retention (kappa)"),
            helpText("by varying pre-ART dropout rate, kappa"),
            sliderInput('userOptKappa_LengthOf','Length of parameter range:',min=0,max=10,value=1,step=1),
            sliderInput('userOptKappa_Range','Range of values (person lost from pre-ART care/py):',min=0,max=2,value=c(0.01,1.079),step=0.001),
            tableOutput("optParTable_Kappa")
            ),
        wellPanel(
            h4("Treatment Initiation (gamma)"),
            helpText("by varying ART initiation rate, gamma"),
            sliderInput('userOptGamma_LengthOf','Length of parameter range:',min=0,max=10,value=1,step=1),
            sliderInput('userOptGamma_Range','Range of values (ART initiations/py):',min=0,max=50,value=c(2.556,20),step=0.001),
            tableOutput("optParTable_Gamma")
            ),
        wellPanel(
            h4("Adherence (sigma)"),
            helpText("by varying rate at which patients not adhering to treatment start to adhere, sigma"),
            sliderInput('userOptSigma_LengthOf','Length of parameter range:',min=0,max=10,value=4,step=1),
            sliderInput('userOptSigma_Range','Range of values (persons transitioning from non-adherent to adherent/py):',min=0,max=1,value=c(0,1),step=0.001),
            tableOutput("optParTable_Sigma")
            ),
        wellPanel(
            h4("ART Retention (omega)"),
            helpText("by varying rate at which patients are lost from ART care, omega"),
            sliderInput('userOptOmega_LengthOf','Length of parameter range:',min=0,max=10,value=4,step=1),
            sliderInput('userOptOmega_Range','Range of values (ART dropout/py):',min=0,max=0.1,value=c(0.005,0.033),step=0.001),
            tableOutput("optParTable_Omega")
            )
    )
)

InterventionList <- c("Rho","Epsilon","Kappa","Gamma","Sigma","Omega")
Tab_Opt_Results <- tabItem(tabName = "opt_results",
    column(width = 4,
        box(width = NULL,
            status = "primary",
            solidHeader = TRUE,
            title = "Results",
            helpText("This section is still under active development."),
            p("The results of the optimisation simulation are shown in the plot to the right. Hit 'Show Result Table' to view all data points and corresponding parameter values.
                Zoom in on data points by drawing a box on the plot with the mouse and double clicking. To view the details of a specific point, draw a box with the mouse over the point and
                hit 'Show Selected Result Table'"),
            p(" "),
            selectInput("userStratPoint","Select parameter to stratify results by:", InterventionList, selected = "Rho"),
            p(" "),
            tags$b("Viral suppression against cost:"),
            p(" "),
            bsButton("showOpt909090Plot", label = "", style = "success", block = TRUE, size = "large", icon = icon("bar-chart", class = "fa-2x fa-fw", lib = "font-awesome")),
            p(" "),
            bsButton("showOpt909090Table", label = "Show Result Table", style = "primary", block = TRUE),
            p(" "),
            bsButton("showOpt909090BrushedTable", label = "Show Selected Result Table", style = "primary", block = TRUE),
            p(" "), br(),
            tags$b("DALYs averted against cost:"),
            p(" "),
            bsButton("showOptDALYsPlot", label = "", style = "danger", block = TRUE, size = "large", icon = icon("bar-chart", class = "fa-2x fa-fw", lib = "font-awesome")),
            p(" "),
            bsButton("showOptDALYsTable", label = "Show Result Table", style = "primary", block = TRUE),
            p(" "),
            bsButton("showOptDALYsBrushedTable", label = "Show Selected Result Table", style = "primary", block = TRUE),
            p(" "), br(),
            tags$b("DALYs averted against cost (for subset achieving 90-90-90):"),
            p(" "),
            bsButton("showOptDALYs909090Plot", label = "", style = "info", block = TRUE, size = "large", icon = icon("bar-chart", class = "fa-2x fa-fw", lib = "font-awesome")),
            p(" "),
            bsButton("showOptDALYs909090Table", label = "Show Result Table", style = "primary", block = TRUE),
            p(" "),
            bsButton("showOptDALYs909090BrushedTable", label = "Show Selected Result Table", style = "primary", block = TRUE)
        ),
        bsButton(inputId = "wizardOpt_4", label = "Next", style = "success", size = "large", block = TRUE, icon = icon("arrow-right", class = "fa-lg fa-fw", lib = "font-awesome"))
    ),
    column(width = 8,
        bsModal(id = "opt909090TableModal",title = "Result Table (showing 90-90-90 targets)",trigger = "showOpt909090Table",size = "large",
            DT::dataTableOutput('opt909090Table', width = "100%")
        ),
        bsModal(id = "opt909090TableBrushedModal",title = "Selected Result Table (showing 90-90-90 targets)",trigger = "showOpt909090BrushedTable",size = "large",
            DT::dataTableOutput('opt909090TableBrushed', width = "100%")
        ),
        bsModal(id = "optDALYsTableModal",title = "Result Table",trigger = "showOptDALYsTable",size = "large",
            DT::dataTableOutput('optDALYsTable', width = "100%")
        ),
        bsModal(id = "optDALYsTableBrushedModal",title = "Selected Result Table",trigger = "showOptDALYsBrushedTable",size = "large",
            DT::dataTableOutput('optDALYsTableBrushed', width = "100%")
        ),
        bsModal(id = "optDALYs909090TableModal",title = "Result Table (results achieving 90-90-90 targets)",trigger = "showOptDALYs909090Table",size = "large",
            DT::dataTableOutput('optDALYs909090Table', width = "100%")
        ),
        bsModal(id = "optDALYs909090TableBrushedModal",title = "Selected Result Table (results achieving 90-90-90 targets)",trigger = "showOptDALYs909090BrushedTable",size = "large",
            DT::dataTableOutput('optDALYs909090TableBrushed', width = "100%")
        ),
        bsCollapse(id = 'optCollapse', open = NULL,
            bsCollapsePanel("Plot 90-90-90",
                plotOutput('plotOpt909090',
                    dblclick = "plotOpt909090_dblclick",
                    brush = brushOpts(
                        id = "plotOpt909090_brush",
                        clip = TRUE,
                        resetOnNew = TRUE
                        )
                    ),
                style = "success"
            ),
            bsCollapsePanel("Plot DALYs",
                plotOutput('plotOptDALYs',
                    dblclick = "plotOptDALYs_dblclick",
                    brush = brushOpts(
                        id = "plotOptDALYs_brush",
                        clip = TRUE,
                        resetOnNew = TRUE
                        )
                    ),
                style = "danger"
            ),
            bsCollapsePanel("Plot DALYs (90-90-90)",
                plotOutput('plotOptDALYs909090',
                    dblclick = "plotOptDALYs909090_dblclick",
                    brush = brushOpts(
                        id = "plotOptDALYs909090_brush",
                        clip = TRUE,
                        resetOnNew = TRUE
                        )
                    ),
                style = "info"
            )
        )
    )
)

Tab_Opt_Budget <- tabItem(tabName = "opt_budget",
    column(width = 4,
        box(width = NULL,
            status = "primary",
            solidHeader = TRUE,
            title = "Budget",
            helpText("Please enter a health care budget value in the box below to see a subset of results that do not exceed that value."),
            numericInput("userBudget","Enter budget to subset results (2013 USD):", value = 0, min = 0, step = 1e+04),
            tags$b("Select output as 90-90-90 indicators or DALYs:"),
            p(" "),
            bsButton("showBudget909090", label = "90-90-90", style = "success", block = TRUE, size = "large"),
            p(" "),
            bsButton("showBudgetDALYs", label = "DALYs", style = "danger", block = TRUE, size = "large")
        )
    ),
    column(width = 8,
        box(width = NULL,
            status = "primary",
            DT::dataTableOutput('optBudgetTable', width = "100%")
        )
    )
)
