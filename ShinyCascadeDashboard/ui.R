library(shiny)
library(ggplot2)
library(shinythemes)
library(DT)
library(shinyjs)
library(shinyBS)
library(shinydashboard)
# devtools::install_github("shinyTable", "trestletech")
# library(shinyTable)

VariableNames <- c(
    "UnDx_500",
    "UnDx_350500",
    "UnDx_250350",
    "UnDx_200250",
    "UnDx_100200",
    "UnDx_50100",
    "UnDx_50",
    "Dx_500",
    "Dx_350500",
    "Dx_250350",
    "Dx_200250",
    "Dx_100200",
    "Dx_50100",
    "Dx_50",
    "Care_500",
    "Care_350500",
    "Care_250350",
    "Care_200250",
    "Care_100200",
    "Care_50100",
    "Care_50",
    "PreLtfu_500",
    "PreLtfu_350500",
    "PreLtfu_250350",
    "PreLtfu_200250",
    "PreLtfu_100200",
    "PreLtfu_50100",
    "PreLtfu_50",
    "Tx_Na_500",
    "Tx_Na_350500",
    "Tx_Na_250350",
    "Tx_Na_200250",
    "Tx_Na_100200",
    "Tx_Na_50100",
    "Tx_Na_50",
    "Tx_A_500",
    "Tx_A_350500",
    "Tx_A_250350",
    "Tx_A_200250",
    "Tx_A_100200",
    "Tx_A_50100",
    "Tx_A_50",
    "Ltfu_500",
    "Ltfu_350500",
    "Ltfu_250350",
    "Ltfu_200250",
    "Ltfu_100200",
    "Ltfu_50100",
    "Ltfu_50",
    "NewInf",
    "HivMortality",
    "NaturalMortality",
    "N",
    "ART",
    "UnDx",
    "Dx",
    "Care",
    "Tx",
    "Vs",
    "Ltfu",
    "NaturalMortalityProp",
    "HivMortalityProp",
    "NewInfProp",
    "Dx_Cost",
    "Linkage_Cost",
    "Annual_Care_Cost",
    "Annual_ART_Cost",
    "TotalCost",
    "cd4_500",
    "cd4_350500",
    "cd4_250350",
    "cd4_200250",
    "cd4_100200",
    "cd4_50100",
    "cd4_50"
    )

CountryList <- c(
    "Brazil",
    "Cambodia",
    "Cameroon",
    "China",
    "Cote d'Ivoire",
    "DRC",
    "Ethiopia",
    "Haiti",
    "India",
    "Indonesia",
    "Jamaica",
    "Kenya",
    "Malawi",
    "Mozambique",
    "Myanmar",
    'Nigeria',
    "Pakistan",
    "Philippines",
    "South Africa",
    "South Sudan",
    "Tanzania",
    "Thailand",
    "Uganda",
    "Ukraine",
    "Vietnam",
    "Zambia",
    "Zimbabwe"
    )

InterventionList <- c("Rho","Epsilon","Kappa","Gamma","Sigma","Omega")

source("content/introduction.R")
source("content/more.R")
source("content/setup.R")
source("content/parameters.R")

dashboardPage(
    skin = "blue",
    dashboardHeader(title = "Cascade App"),
    dashboardSidebar(
            sidebarMenu(
                menuItem("Introduction", tabName = "introduction", icon = icon("home", lib = "font-awesome")),
                menuItem("Setup", tabName = "setup", icon = icon("cogs", lib = "font-awesome")),
                menuItem("Parameters", tabName = "parameters", icon = icon("cog", lib = "font-awesome")),
                menuItem("Results", icon = icon("line-chart", lib = "font-awesome"),
                    menuSubItem("Your Cascade", tabName = "subitem1"),
                    menuSubItem("The Care Cascade", tabName = "subitem2"),
                    menuSubItem("The Power's Cascade", tabName = "subitem3"),
                    menuSubItem("90-90-90", tabName = "subitem3"),
                    menuSubItem("Incidence", tabName = "subitem4"),
                    menuSubItem("AIDS Deaths", tabName = "subitem5")
                    ),
                
                menuItem("Optimisation", tabName = "optimisation", icon = icon("pie-chart", lib = "font-awesome"),
                    menuSubItem("Cost", tabName = "subitem1"),
                    menuSubItem("Parameter Selection", tabName = "subitem2"),
                    menuSubItem("Results", tabName = "subitem3"),
                    menuSubItem("Budget", tabName = "subitem4")
                    ),
                
                menuItem("More", icon = icon("question", lib = "font-awesome"),
                    menuSubItem("Model Document", tabName = "model_document"),
                    menuSubItem("Single Plot", tabName = "single_plot"),
                    menuSubItem("All Plots", tabName = "all_plots")
                    )
            )
        ),
    dashboardBody(
        # This contains tabItems(tabItem(tabName = "blah"))
        tabItems(
            Tab_Introduction,
            Tab_Setup,
            Tab_Parameters,
            Tab_ModelDocument,
            Tab_SinglePlot,
            Tab_AllPlots
            )
        )
    )


#     navbarMenu("Results",
#         tabPanel("Your Cascade",
#             h1(textOutput('CountryName')),
#             sidebarPanel(
#                 tags$b("Number of PLHIV:"), 
#                 p(""),
#                 verbatimTextOutput("outPLHIV"),
#                 tags$b("Percentage of total PLHIV:"), 
#                 p(""),
#                 verbatimTextOutput("outPLHIV_perc"),
#                 p(""),
#                 plotOutput('plotValidation_PLHIV',
#                     height = 'auto',
#                     width = 'auto'),
#                 width = 6),

#             sidebarPanel(
#                 tags$b("Number of PLHIV who have been diagnosed:"),
#                 p(""),
#                 verbatimTextOutput("outDIAG"),
#                 tags$b("Percentage of PLHIV who have been diagnosed:"), 
#                 p(""),
#                 verbatimTextOutput("outDIAG_perc"),
#                 p(""),
#                 plotOutput('plotValidation_DIAG',
#                     height = 'auto',
#                     width = 'auto'),
#                 width = 6),

#             sidebarPanel(
#                 tags$b("Number of PLHIV in HIV care (including ART):"),
#                 p(""),
#                 verbatimTextOutput("outCARE"),
#                 tags$b("Percentage of PLHIV in HIV care (including ART):"),
#                 p(""),
#                 verbatimTextOutput("outCARE_perc"),
#                 p(""),
#                 plotOutput('plotValidation_CARE',
#                     height = 'auto',
#                     width = 'auto'),
#                 width = 6),

#             sidebarPanel(
#                 tags$b("Number of PLHIV in HIV care and on ART:"),
#                 p(""),
#                 verbatimTextOutput("outART"),
#                 tags$b("Percentage of PLHIV in HIV care and on ART:"),
#                 p(""),
#                 verbatimTextOutput("outART_perc"),
#                 p(""),
#                 plotOutput('plotValidation_ART',
#                     height = 'auto',
#                     width = 'auto'),
#                 width = 6),

#             sidebarPanel(
#                 tags$b("Number of PLHIV in HIV care, on ART and virally suppressed:"),
#                 p(""),
#                 verbatimTextOutput("outSUPP"),
#                 tags$b("Percentage of PLHIV in HIV care, on ART and virally suppressed:"),
#                 p(""),
#                 verbatimTextOutput("outSUPP_perc"),
#                 p(""),
#                 plotOutput('plotValidation_SUPP',
#                     height = 'auto',
#                     width = 'auto'),
#                 width = 6),

#             sidebarPanel(
#                 tags$b("Number of PLHIV who dropped out of ART care:"),
#                 p(""),
#                 verbatimTextOutput("outLTFU"),
#                 tags$b("Percentage of PLHIV who dropped out of ART care:"),
#                 p(""),
#                 verbatimTextOutput("outLTFU_perc"),
#                 p(""),
#                 plotOutput('plotValidation_LTFU',
#                     height = 'auto',
#                     width = 'auto'),
#                 width = 6)
#             ),
#         tabPanel("The Care Cascade",
#             fluidRow(
#                 plotOutput('plotCascade')
#                 ),
#             fluidRow(
#                     br(),
#                     wellPanel(
#                         h4("The distribution of care between 2015 and 2020."),
#                         helpText("Note, the denominator in all these calculations is # of PLHIV."),
#                         p("These figures illustrate the 'Care Cascade' in 2015 (at baseline), and the projection after 5 years (in 2020)."),
#                         tags$ol(
#                             tags$li("% diagnosed = # persons diagnosed / PLHIV"),
#                             tags$li("% in care = # persons in care (including on ART & virally suppressed) / PLHIV"),
#                             tags$li("% on treatment = # persons on ART (including those virally suppressed) / PLHIV"),
#                             tags$li("% virally suppressed = # persons on ART and virally suppressed / PLHIV"),
#                             tags$li("% LTFU = # persons lost from ART care / PLHIV.")
#                         )
#                     )
#                 )
#             ),
#         tabPanel("The Power's Cascade",
#             fluidRow(
#                 plotOutput('plotPowersCascade')
#                 ),
#             fluidRow(
#                     br(),
#                     wellPanel(
#                         h4("The distribution of care between 2015 and 2020."),
#                         helpText("Note, the denominator in all these calculations is # of PLHIV."),
#                         p("These figures illustrate the 'Care Cascade' in 2015 (at baseline), and the projection after 5 years (in 2020)."),
#                         helpText("Figures are based on those found in Powers et al. (2015)."),
#                         tags$ol(
#                             tags$li("% Undiagnosed = # persons undiagnosed / PLHIV"),                            
#                             tags$li("% Diagnosed = # persons diagnosed and not in care / PLHIV"),
#                             tags$li("% In Care = # persons diagnosed, in care, not on ART / PLHIV"),
#                             tags$li("% On Treatment (non-adherent) = # persons diagnosed, in care, on ART, but not adhering and not virally suppressed / PLHIV"),
#                             tags$li("% On Treatment (adherent) = # persons diagnosed, in care, on ART, adhering, but not virally suppressed / PLHIV"),
#                             tags$li("% Virally Suppressed = # persons diagnosed, in care, on ART, virally suppressed / PLHIV"),
#                             tags$li("% LTFU = # persons diagnosed, not in care, dropped out of ART / PLHIV.")
#                         )
#                     )
#                 )
#             ),
#         tabPanel("90-90-90",
#             sidebarPanel(
#                 h4("UNAIDS 90-90-90"),
#                 p("By 2020, this is what the model predicts will be achieved in comparison to the UNAIDS goals of 90% diagnosed, 
#                     90% on treatment and 90% virally suppressed. If you would like to see what changes can be made to resolve any 
#                     inefficiencies in care, then click on the 'Optimisation' tab."),
#                 tableOutput("table909090")
#                 ),
#             mainPanel(
#                 plotOutput('plot909090')
#                 )
#             ),
#         tabPanel("New Infections",
#             sidebarPanel(
#                 h4("New Infections"),
#                 p("Predictions of incident infections between 2015 and 2020, illustrated as a proportion of the total HIV-positive population.")
#                 ),
#             mainPanel(
#                 plotOutput('plotNewInf')
#                 )
#             ),
#         tabPanel("AIDS Deaths",
#             sidebarPanel(
#                 h4("AIDS Deaths"),
#                 p("Predictions of AIDS deaths between 2015 and 2020, illustrated as a proportion of the total HIV-positive population.")
#                 ),
#             mainPanel(
#                 plotOutput('plotAidsDeaths')
#                 )
#             )
#         ),
#     navbarMenu("Optimisation",
#         tabPanel("Cost",
#             sidebarPanel(
#                 h4("Cost"),
#                 p("Review or edit the unit costs in each box."),
#                 helpText("Click the 'optimisation' drop down menu and select 'results' to begin running the optimisation algorithm."),
#                 bsButton("resetCost",label="RESET COST",style="danger"),
#                 p(" "),
#                 h4("Unit cost table"),
#                 tableOutput("unitCostTable")
#                 ),
#             mainPanel(
#                 shinyjs::useShinyjs(),
#                 id = "cost-panel",
#                 wellPanel(
#                     sliderInput('userDxUnitCost','Unit cost of diagnosing a patient (USD):',min=0,max=100,value=10,step=1)
#                     ),
#                 wellPanel(
#                     sliderInput('userLinkageUnitCost','Unit cost of linking a patient to care (USD):',min=0,max=100,value=40,step=1)
#                     ),
#                 wellPanel(
#                     sliderInput('userAnnualCareUnit','Annual cost of keeping a patient in pre-ART care (USD):',min=0,max=100,value=40,step=1)
#                     ),
#                 wellPanel(
#                     sliderInput('userAnnualARTUnitCost','Annual cost of ART (USD):',min=0,max=500,value=367,step=1)
#                     )
#                 )
#             ),
#         tabPanel("Parameter Selection",
#             sidebarPanel(
#                 h4("Parameter Selection"),
#                 p("The optimisation algorithm takes six model parameters representing six hypothetical interventions and simulates the cost and impact of all permutations."),
#                 helpText("For each intervention, select the number of parameter values to simulate and set the range of rates to sample from (rates are uniformally distributed within this range).
#                     The parameter values for each intervention are then displayed in the corresponding tables."),
#                 bsButton("resetSliders",label="RESET SLIDERS",style="danger"),
#                 p(" "),
#                 helpText("Below is the number of iterations the model will simulate along with the estimated time to completion. Hit the 'OPTIMISE' button to begin the simulation. Note the progress bar 
#                     at the top of the screen, and the run number and elapsed time on the top right. Please wait until the optimisation algorithm has completed the below bar has turned green before proceeding to the results tab."),
#                 bsButton("optimiseInput",label="OPTIMISE",style="info"),
#                 bsTooltip(id = "optimiseInput", title = "Wait for progress bar to complete before proceeding.", placement = "right", trigger = "hover"),
#                 p(" "),
#                 tableOutput("optIterationTable"),
#                 bsButton("optFinished",label="OPTIMISATION NOT RUN",style="danger",icon = icon("ban"),disabled = TRUE)
#                 ),
#             mainPanel(
#                 shinyjs::useShinyjs(),
#                 id = "optimisation-panel",
#                 wellPanel(
#                     h4("HIV-Testing (rho)"),
#                     helpText("by varying diagnosis rate, rho"),
#                     sliderInput('userOptRho_LengthOf','Length of parameter range:',min=0,max=10,value=1,step=1),
#                     sliderInput('userOptRho_Range','Range of values (diagnoses/py):',min=0,max=50,value=c(0.205,20),step=0.001),
#                     tableOutput("optParTable_Rho")
#                     ),
#                 wellPanel(
#                     h4("Linkage (epsilon)"),
#                     helpText("by varying care seeking rate, epsilon"),
#                     sliderInput('userOptEpsilon_LengthOf','Length of parameter range:',min=0,max=10,value=1,step=1),
#                     sliderInput('userOptEpsilon_Range','Range of values (persons seeking care/py):',min=0,max=50,value=c(16.949,30),step=0.001),
#                     tableOutput("optParTable_Epsilon")
#                     ),
#                 wellPanel(
#                     h4("Pre-ART Retention (kappa)"),
#                     helpText("by varying pre-ART dropout rate, kappa"),
#                     sliderInput('userOptKappa_LengthOf','Length of parameter range:',min=0,max=10,value=1,step=1),
#                     sliderInput('userOptKappa_Range','Range of values (person lost from pre-ART care/py):',min=0,max=2,value=c(0.01,1.079),step=0.001),
#                     tableOutput("optParTable_Kappa")
#                     ),
#                 wellPanel(
#                     h4("Treatment Initiation (gamma)"),
#                     helpText("by varying ART initiation rate, gamma"),
#                     sliderInput('userOptGamma_LengthOf','Length of parameter range:',min=0,max=10,value=1,step=1),
#                     sliderInput('userOptGamma_Range','Range of values (ART initiations/py):',min=0,max=50,value=c(2.556,20),step=0.001),
#                     tableOutput("optParTable_Gamma")
#                     ),
#                 wellPanel(
#                     h4("Adherence (sigma)"),
#                     helpText("by varying rate at which patients not adhering to treatment start to adhere, sigma"),
#                     sliderInput('userOptSigma_LengthOf','Length of parameter range:',min=0,max=10,value=4,step=1),
#                     sliderInput('userOptSigma_Range','Range of values (persons transitioning from non-adherent to adherent/py):',min=0,max=1,value=c(0,1),step=0.001),
#                     tableOutput("optParTable_Sigma")
#                     ),
#                 wellPanel(
#                     h4("ART Retention (omega)"),
#                     helpText("by varying rate at which patients are lost from ART care, omega"),
#                     sliderInput('userOptOmega_LengthOf','Length of parameter range:',min=0,max=10,value=4,step=1),
#                     sliderInput('userOptOmega_Range','Range of values (ART dropout/py):',min=0,max=0.1,value=c(0.005,0.033),step=0.001),
#                     tableOutput("optParTable_Omega")
#                     )
#                 )
#             ),
#         tabPanel("Results",
#             sidebarPanel(
#                 h4("Results"),
#                 helpText("This section is still under active development."),
#                 p("The results of the optimisation simulation are shown in the plot to the right. Hit 'Show Result Table' to view all data points and corresponding parameter values.
#                     Zoom in on data points by drawing a box on the plot with the mouse and double clicking. To view the details of a specific point, draw a box with the mouse over the point and 
#                     hit 'Show Selected Result Table'"),
#                 p(" "),
#                 selectInput("userStratPoint","Select parameter to stratify results by:", InterventionList, selected = "Rho"),
#                 p(" "),
#                 tags$b("Viral suppression against cost:"),
#                 p(" "),
#                 bsButton("showOpt909090Plot",label="Plot proportion achieving 90-90-90 targets by 2020",style="success"),
#                 p(" "),
#                 bsButton("showOpt909090Table",label="Show Result Table",style="primary"),
#                 p(" "),
#                 bsButton("showOpt909090BrushedTable",label="Show Selected Result Table",style="primary"),
#                 p(" "), br(),
#                 tags$b("DALYs averted against cost:"),
#                 p(" "),
#                 bsButton("showOptDALYsPlot",label="Plot DALYs averted against cost",style="danger"),
#                 p(" "),
#                 bsButton("showOptDALYsTable",label="Show Result Table",style="primary"),
#                 p(" "),
#                 bsButton("showOptDALYsBrushedTable",label="Show Selected Result Table",style="primary"),
#                 p(" "), br(),
#                 tags$b("DALYs averted against cost (for subset achieving 90-90-90):"),
#                 p(" "),
#                 bsButton("showOptDALYs909090Plot",label="Plot DALYs averted against cost (90-90-90)",style="info"),
#                 p(" "),
#                 bsButton("showOptDALYs909090Table",label="Show Result Table",style="primary"),
#                 p(" "),
#                 bsButton("showOptDALYs909090BrushedTable",label="Show Selected Result Table",style="primary")
#                 ),
#             mainPanel(
#                 bsModal(id = "opt909090TableModal",title = "Result Table (showing 90-90-90 targets)",trigger = "showOpt909090Table",size = "large",
#                     DT::dataTableOutput('opt909090Table', width = "100%")
#                 ),
#                 bsModal(id = "opt909090TableBrushedModal",title = "Selected Result Table (showing 90-90-90 targets)",trigger = "showOpt909090BrushedTable",size = "large",
#                     DT::dataTableOutput('opt909090TableBrushed', width = "100%")
#                 ),
#                 bsModal(id = "optDALYsTableModal",title = "Result Table",trigger = "showOptDALYsTable",size = "large",
#                     DT::dataTableOutput('optDALYsTable', width = "100%")
#                 ),
#                 bsModal(id = "optDALYsTableBrushedModal",title = "Selected Result Table",trigger = "showOptDALYsBrushedTable",size = "large",
#                     DT::dataTableOutput('optDALYsTableBrushed', width = "100%")
#                 ),
#                 bsModal(id = "optDALYs909090TableModal",title = "Result Table (results achieving 90-90-90 targets)",trigger = "showOptDALYs909090Table",size = "large",
#                     DT::dataTableOutput('optDALYs909090Table', width = "100%")
#                 ),
#                 bsModal(id = "optDALYs909090TableBrushedModal",title = "Selected Result Table (results achieving 90-90-90 targets)",trigger = "showOptDALYs909090BrushedTable",size = "large",
#                     DT::dataTableOutput('optDALYs909090TableBrushed', width = "100%")
#                 ),
#                 bsCollapse(id = 'optCollapse', open = NULL,
#                     bsCollapsePanel("Plot 90-90-90",
#                         plotOutput('plotOpt909090',
#                             dblclick = "plotOpt909090_dblclick",
#                             brush = brushOpts(
#                                 id = "plotOpt909090_brush",
#                                 clip = TRUE,
#                                 resetOnNew = TRUE
#                                 )
#                             ),
#                         style = "success"
#                         ),
#                     bsCollapsePanel("Plot DALYs",
#                         plotOutput('plotOptDALYs',
#                             dblclick = "plotOptDALYs_dblclick",
#                             brush = brushOpts(
#                                 id = "plotOptDALYs_brush",
#                                 clip = TRUE,
#                                 resetOnNew = TRUE
#                                 )
#                             ),
#                         style = "danger"
#                         ),
#                     bsCollapsePanel("Plot DALYs (90-90-90)",
#                         plotOutput('plotOptDALYs909090',
#                             dblclick = "plotOptDALYs909090_dblclick",
#                             brush = brushOpts(
#                                 id = "plotOptDALYs909090_brush",
#                                 clip = TRUE,
#                                 resetOnNew = TRUE
#                                 )
#                             ),
#                         style = "info"
#                         )
#                     )
#                 )
#             ),
#         tabPanel("Budget",
#             sidebarPanel(
#                 h4("Budget"),
#                 helpText("Please enter a health care budget value in the box below to see a subset of results that do not exceed that value."),
#                 numericInput("userBudget","Enter budget to subset results (2013 USD):", value = 0, min = 0, step = 1e+04),
#                 tags$b("Select output as 90-90-90 indicators or DALYs:"),
#                 p(" "),
#                 bsButton("showBudget909090",label="90-90-90",style="success"),
#                 p(" "),
#                 bsButton("showBudgetDALYs",label="DALYs",style="danger")
#                 ),
#             mainPanel(
#                 DT::dataTableOutput('optBudgetTable', width = "100%")
#                 )
#             )
#         ),
#     navbarMenu("Diagnostics",
#             tabPanel("Single Plot",
#                 sidebarPanel(
#                     selectInput('y','Y',VariableNames,selected="ART")
#                     ),
#                   mainPanel(
#                     plotOutput('plotOne')
#                     )
#                 ),
#             tabPanel("All Plots",
#                 mainPanel(
#                     plotOutput('plotTwo')
#                     )
#                 )
#         ),
#     navbarMenu("More",
#     tabPanel("Model Document",
#         HTML('<iframe src=\"https://drive.google.com/file/d/0B02uVauBTUwhckJ1bG1QRmdwTGM/preview\"style=\"border: 0; position:absolute; top:50px; left:0; right:0; width:100%; height:100%\"></iframe>')
#         ),
#     # tabPanel("Country Input Data",
#     #     HTML('<iframe src=\"https://drive.google.com/file/d/1rIMf-0vB77uwy7XO4rCM9Isd7_ReiCeOkwW1BvoxZw4/preview\"style=\"border: 0; position:absolute; top:50px; left:0; right:0; width:100%; height:100%\"></iframe>')
#     #     ),
#     # tabPanel("Incidence Estimates",
#     #     HTML('<iframe src=\"https://drive.google.com/file/d/1-OVjcIl7m-QZt0T52WMcsU1Oxv4znTyc4eU0464L24I/preview\"style=\"border: 0; position:absolute; top:50px; left:0; right:0; width:100%; height:100%\"></iframe>')
#     #     ),
#     tabPanel("Raw Output",
#         DT::dataTableOutput('outputTable')
#         )
#     )
# ))