library(shiny)
library(ggplot2)
library(shinythemes)
library(DT)
library(shinyjs)
library(shinyBS)
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

shinyUI(
    navbarPage("Cascade App",
    theme = shinytheme("flatly"),
    tabPanel("Introduction",
        # img(src="HIVMC-logo.jpg", height = '100%', width = '100%'),
        h1("Introduction"),
        helpText("Welcome to the Cascade App."),
        sidebarLayout(position="right",
            sidebarPanel(
                h4("Quick Start"),
                helpText("If you want to skip the introduction and get modelling, jump to 'setup' and click on 'DEMO'. 
                    Data entered can be saved by clicking 'SAVE' on any relevant page. A pdf containing details about 
                    the entire model can be found in the 'more' tab along with links to spreadsheets containing data used in the model."),
                br(),
                h5("Contributors"),
                helpText("Jack J Olney, Jeffrey W Eaton, Ellen McRobie & Timothy B Hallett")
                ),
            mainPanel(
                bsAlert("startAlert"),
                p("With the release of the Consolidated Information Guidelines for HIV by the World Health Organization (WHO)
                in May 2015, a set of indicators have been agreed upon, based on the cascade of HIV services relating to impact in
                terms of HIV incidence and mortality (see below). These guidelines provide a framework for countries to assess the current state
                of care and identify any immediate deficiencies and bottlenecks preventing patients from progressing to treatment."),
                p("Furthermore, as we pass the Millennium Development Goals of 2015 and focus attention on the UNAIDS 90-90-90 targets for 2020,
                countries will be keen to understand whether they are on the right trajectory to achieve these goals. For this purpose, data from
                countries can be input into a mathematical model and used to estimate future incidence, AIDS-deaths and the ascertainment of the 90-90-90 goals."),
                img(src="WHO-Guidelines-Front-Crop.png", height = '100%', width = '100%'),
                h3("Aims"),
                p("This webpage contains an interactive model that allows data to be entered, parameters to be altered and results to be presented in real-time. 
                    No specialist software is required as all calculations are completed on a remote server, results are then returned and displayed, along with all visualisations, in the browser."),
                p("The overarching aims of this model are to:"),
                tags$ol(
                    tags$li("Provide a simple intuitive tool to document and analyse the current state of HIV care. This mathematical model is able to handle a wide range
                        of data, characterise individuals into discrete care categories, and by building upon a set of assumptions regarding HIV-transmission, duration of
                        infection and progression through care and death, the model can project changes in incidence, mortality and care until 2020."),
                    tags$li("The model is country-specific. During setup, when a new country is selected from the drop down list, 
                        the model adjusts its incidence estimates based on data from the Spectrum software used by UNAIDS."),
                    tags$li("The model can be used to assess the current state of care and ascertain gaps that require immediate attention. Furthermore, the model can identify what
                        future changes need to be made, that differ from what countries have done so far, to be on track to achieve the 90-90-90 goals set out by UNAIDS."),
                    tags$li("Non-specific interventions can be simulated that broadly illustrate the changes that can be made to care, along with with the costs of doing so, 
                        to identify the most cost-effective strategy for reconciling any deficiencies in care (still in development)."),
                    tags$li("It is hoped that this model will help countries prioritise strategies and estimate the costs required to achieve future targets.")
                ),
                h3("The Model"),
                    p("The simplified structure of the model is shown below. State compartments do not exactly correlate with the indicators in Consolidated Indicator Guidelines, 
                        as compartments in the model must be discrete and exhaustive, while the indicators listed are not all discrete; for example, 'Knowing HIV status' includes all patients who are in care, 
                        on treatment, virally suppressed and lost from care as long as they are diagnosed. However, the model is able to reconcile this by taking individual indicators and separating them into their 
                        components to specify the initial conditions for simulations."),
                    helpText("More details on the model can be found in the following pages, along with a detailed description under the 'more' table and 'Model Document'."),
                img(src="ModelSimple.png", height = '100%', width = '100%'),
                h3("Outcomes"),
                tags$ol(
                    tags$li("Predict achievement of UNAIDS 90-90-90 targets in 2020."),
                    tags$li("Identify strategies to achieve 90-90-90 targets at minimal cost."),
                    tags$li("Illustrate the change in distribution of care between 2015 and 2020."),
                    tags$li("Predict changes in new infections and AIDS deaths between 2015 and 2020.")
                    ),
                br(),
                em(h4("Click 'Setup' to get started!"))
                )
            )
        ),
    tabPanel("Setup",
        h1("Model Setup"),
        sidebarLayout(position="right",
            sidebarPanel(
                h4("Help Panel"),
                helpText("Please fill in all boxes with relevant data, then hit 'SAVE' and wait for the confirmation below. 
                    Hit 'RESET' to reset all values to zero, and hit 'DEMO' for a random set of values to be generated. 
                    Unchecking the 'HIV incidence' checkbox prevents any new infections occurring in the model."),
                checkboxInput("incidenceInput","HIV Incidence",value=TRUE),
                bsButton("saveInput",label="SAVE",style="success"),
                p(" "),
                bsButton("resetInput",label="RESET",style="danger"),
                p(" "),
                bsButton("demoInput",label="DEMO",style="primary"),
                bsTooltip(id = "demoInput", title = "Populate model with best estimates from Kenya.", placement = "left", trigger = "hover"),
                p(" "),
                helpText("Console output:"),
                textOutput('saveText'),
                textOutput('warningText'),
                textOutput('warningCD4Text')
                ),
            mainPanel(
                shinyjs::useShinyjs(),
                id = "setup-panel",
                helpText("Select country and fill in boxes to specify the initial values of the model. 
                    The boxes correlate with indicators in the Consolidated Information Guidelines (shown below), with numbers in brackets corresponding to numbers on the indicator figure."),
                img(src="WHOGuidelinesCascade.png", height = '100%', width = '100%'),
                br(), br(),
                wellPanel(
                    selectInput("userCountry","Country:",CountryList,selected="Brazil")
                    ),
                wellPanel(
                    h4("ART Initiation Threshold"),
                    helpText("Please check the box corresponding to the correct treatment threshold. Boxes are reactive and nearby boxes will adjust to the selection made. Please note that unchecking all boxes will result in ART being witheld for all individuals."),
                    checkboxInput("userART_All","Immediate ART",value=TRUE),
                    checkboxInput("userART_500","CD4 <500",value=TRUE),
                    checkboxInput("userART_350","CD4 <350",value=TRUE),
                    checkboxInput("userART_200","CD4 <200",value=TRUE)
                    ),
                wellPanel(
                    numericInput("userPLHIV","Number of PLHIV (1):",1e+6,min=0)
                    ),
                wellPanel(
                    numericInput("userDx","Number of PLHIV who have been diagnosed (4):",0,min=0)
                    ),
                wellPanel(
                    numericInput("userCare","Number of PLHIV in HIV care (including ART) (5):",0,min=0)
                    ),
                wellPanel(
                    numericInput("userTx","Number of PLHIV in HIV care and on ART (6):",0,min=0)
                    ),
                wellPanel(
                    numericInput("userVs","Number of PLHIV in HIV care, on ART and virally suppressed (8):",0,min=0)
                    ),
                wellPanel(
                    numericInput("userLtfu","Number of PLHIV who dropped out of ART care:",0,min=0)
                    ),
                em(h5("If this value is known please enter it, otherwise leave it blank:")),
                wellPanel(
                    numericInput("userRetArt12mths","Percentage of PLHIV retained and surviving on ART 12 months after initiation (7):",0,min=0,max=1,step=0.01)
                    )
                )
            )
        ),
    tabPanel("Parameters",
        h1("Parameter Values"),
        sidebarLayout(position="right",
            sidebarPanel(
                h4("Help Panel"),
                helpText("It is not neccessary to alter any of these values, but feel free to move the sliders around and see the values in the table change. 
                    Hit 'RESET PARAMETERS' to reset all parameters including the 'ART dropout rate' if specified in the 'Setup' tab."),
                bsButton("resetParameters",label="RESET PARAMETERS",style="danger"),
                p(" "),
                tableOutput("parameterTable"),
                helpText("Details regarding the origin of these parameter values are found in the 'Model Document', under the 'More' tab.")
                ),
            mainPanel(
                shinyjs::useShinyjs(),
                id = "parameter-panel",
                helpText("Below is a detailed diagram of the model showing the flow of patients through care and the progression of HIV, 
                    captured by the decline of CD4 counts when not on treatment and the recovery of CD4 counts when on ART. 
                    A table of parameter values is shown in the 'Help Panel' and several sliders are shown below which can be 
                    used to manipulate certain parameter values. Parameter values can be manipulated by changing the rate or the inverse of the rate (time to event). 
                    You only need to change one slider as the other updated auotmatically. Please note that the parameter table is 'live' and will update in real-time."),
                img(src = "ModelSimple.png",height = "100%",width = "100%"),
                br(), br(),
                wellPanel(
                    sliderInput('rho','Diagnosis rate (diagnoses/py) (rho):',min=0,max=5,value=0.205,step=0.001,width=1000),
                    sliderInput('invRho','Average time to diagnosis (years) (1 / rho):',min=0,max=100,value=1/0.205,step=0.001,width=1000)
                    ),
                wellPanel(
                    sliderInput('epsilon','Care seeking rate (persons seeking care/py) (epsilon):',min=0,max=20,value=16.949,step=0.001,width=1000),    
                    sliderInput('invEpsilon','Average time to seeking care (years) (1 / epsilon):',min=0,max=100,value=1/16.949,step=0.001,width=1000)
                    ),
                wellPanel(
                    sliderInput('gamma','ART initiation rate (ART initiations/py) (gamma):',min=0,max=5,value=2.556,step=0.001,width=1000),    
                    sliderInput('invGamma','Average time to ART initiation (years) (1 / gamma):',min=0,max=100,value=1/2.556,step=0.001,width=1000)
                    ),
                wellPanel(
                    sliderInput('omega','ART dropout rate (ART dropout/py) (omega):',min=0,max=5,value=0.033,step=0.001,width=1000),
                    sliderInput('invOmega','Average time to ART dropout (years) (1 / omega):',min=0,max=100,value=1/0.033,step=0.001,width=1000)
                    )
                )
            )
        ),
    navbarMenu("Results",
        tabPanel("Your Cascade",
            h1(textOutput('CountryName')),
            sidebarPanel(
                tags$b("Number of PLHIV:"), 
                p(""),
                verbatimTextOutput("outPLHIV"),
                tags$b("Percentage of total PLHIV:"), 
                p(""),
                verbatimTextOutput("outPLHIV_perc"),
                p(""),
                plotOutput('plotValidation_PLHIV',
                    height = 'auto',
                    width = 'auto'),
                width = 6),

            sidebarPanel(
                tags$b("Number of PLHIV who have been diagnosed:"),
                p(""),
                verbatimTextOutput("outDIAG"),
                tags$b("Percentage of PLHIV who have been diagnosed:"), 
                p(""),
                verbatimTextOutput("outDIAG_perc"),
                p(""),
                plotOutput('plotValidation_DIAG',
                    height = 'auto',
                    width = 'auto'),
                width = 6),

            sidebarPanel(
                tags$b("Number of PLHIV in HIV care (including ART):"),
                p(""),
                verbatimTextOutput("outCARE"),
                tags$b("Percentage of PLHIV in HIV care (including ART):"),
                p(""),
                verbatimTextOutput("outCARE_perc"),
                p(""),
                plotOutput('plotValidation_CARE',
                    height = 'auto',
                    width = 'auto'),
                width = 6),

            sidebarPanel(
                tags$b("Number of PLHIV in HIV care and on ART:"),
                p(""),
                verbatimTextOutput("outART"),
                tags$b("Percentage of PLHIV in HIV care and on ART:"),
                p(""),
                verbatimTextOutput("outART_perc"),
                p(""),
                plotOutput('plotValidation_ART',
                    height = 'auto',
                    width = 'auto'),
                width = 6),

            sidebarPanel(
                tags$b("Number of PLHIV in HIV care, on ART and virally suppressed:"),
                p(""),
                verbatimTextOutput("outSUPP"),
                tags$b("Percentage of PLHIV in HIV care, on ART and virally suppressed:"),
                p(""),
                verbatimTextOutput("outSUPP_perc"),
                p(""),
                plotOutput('plotValidation_SUPP',
                    height = 'auto',
                    width = 'auto'),
                width = 6),

            sidebarPanel(
                tags$b("Number of PLHIV who dropped out of ART care:"),
                p(""),
                verbatimTextOutput("outLTFU"),
                tags$b("Percentage of PLHIV who dropped out of ART care:"),
                p(""),
                verbatimTextOutput("outLTFU_perc"),
                p(""),
                plotOutput('plotValidation_LTFU',
                    height = 'auto',
                    width = 'auto'),
                width = 6)
            ),
        tabPanel("The Care Cascade",
            fluidRow(
                plotOutput('plotCascade')
                ),
            fluidRow(
                    br(),
                    wellPanel(
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
            ),
        tabPanel("The Power's Cascade",
            fluidRow(
                plotOutput('plotPowersCascade')
                ),
            fluidRow(
                    br(),
                    wellPanel(
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
            ),
        tabPanel("90-90-90",
            sidebarPanel(
                h4("UNAIDS 90-90-90"),
                p("By 2020, this is what the model predicts will be achieved in comparison to the UNAIDS goals of 90% diagnosed, 
                    90% on treatment and 90% virally suppressed. If you would like to see what changes can be made to resolve any 
                    inefficiencies in care, then click on the 'Optimisation' tab."),
                tableOutput("table909090")
                ),
            mainPanel(
                plotOutput('plot909090')
                )
            ),
        tabPanel("New Infections",
            sidebarPanel(
                h4("New Infections"),
                p("Predictions of incident infections between 2015 and 2020, illustrated as a proportion of the total HIV-positive population.")
                ),
            mainPanel(
                plotOutput('plotNewInf')
                )
            ),
        tabPanel("AIDS Deaths",
            sidebarPanel(
                h4("AIDS Deaths"),
                p("Predictions of AIDS deaths between 2015 and 2020, illustrated as a proportion of the total HIV-positive population.")
                ),
            mainPanel(
                plotOutput('plotAidsDeaths')
                )
            )
        ),
    navbarMenu("Optimisation",
        tabPanel("Cost",
            sidebarPanel(
                h4("Cost"),
                p("Review or edit the unit costs in each box."),
                helpText("Click the 'optimisation' drop down menu and select 'results' to begin running the optimisation algorithm."),
                bsButton("resetCost",label="RESET COST",style="danger"),
                p(" "),
                h4("Unit cost table"),
                tableOutput("unitCostTable")
                ),
            mainPanel(
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
            ),
        tabPanel("Parameter Selection",
            sidebarPanel(
                h4("Parameter Selection"),
                p("The optimisation algorithm takes six model parameters representing six hypothetical interventions and simulates the cost and impact of all permutations."),
                helpText("For each intervention, select the number of parameter values to simulate and set the range of rates to sample from (rates are uniformally distributed within this range).
                    The parameter values for each intervention are then displayed in the corresponding tables."),
                bsButton("resetSliders",label="RESET SLIDERS",style="danger"),
                p(" "),
                helpText("Below is the number of iterations the model will simulate along with the estimated time to completion. Hit the 'OPTIMISE' button to begin the simulation. Note the progress bar 
                    at the top of the screen, and the run number and elapsed time on the top right. Please wait until the optimisation algorithm has completed the below bar has turned green before proceeding to the results tab."),
                bsButton("optimiseInput",label="OPTIMISE",style="info"),
                bsTooltip(id = "optimiseInput", title = "Wait for progress bar to complete before proceeding.", placement = "right", trigger = "hover"),
                p(" "),
                tableOutput("optIterationTable"),
                bsButton("optFinished",label="OPTIMISATION NOT RUN",style="danger",icon = icon("ban"),disabled = TRUE)
                ),
            mainPanel(
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
            ),
        tabPanel("Results",
            sidebarPanel(
                h4("Results"),
                helpText("This section is still under active development."),
                p("The results of the optimisation simulation are shown in the plot to the right. Hit 'Show Result Table' to view all data points and corresponding parameter values.
                    Zoom in on data points by drawing a box on the plot with the mouse and double clicking. To view the details of a specific point, draw a box with the mouse over the point and 
                    hit 'Show Selected Result Table'"),
                p(" "),
                selectInput("userStratPoint","Select parameter to stratify results by:", InterventionList, selected = "Rho"),
                p(" "),
                tags$b("Viral suppression against cost:"),
                p(" "),
                bsButton("showOpt909090Plot",label="Plot proportion achieving 90-90-90 targets by 2020",style="success"),
                p(" "),
                bsButton("showOpt909090Table",label="Show Result Table",style="primary"),
                p(" "),
                bsButton("showOpt909090BrushedTable",label="Show Selected Result Table",style="primary"),
                p(" "), br(),
                tags$b("DALYs averted against cost:"),
                p(" "),
                bsButton("showOptDALYsPlot",label="Plot DALYs averted against cost",style="danger"),
                p(" "),
                bsButton("showOptDALYsTable",label="Show Result Table",style="primary"),
                p(" "),
                bsButton("showOptDALYsBrushedTable",label="Show Selected Result Table",style="primary"),
                p(" "), br(),
                tags$b("DALYs averted against cost (for subset achieving 90-90-90):"),
                p(" "),
                bsButton("showOptDALYs909090Plot",label="Plot DALYs averted against cost (90-90-90)",style="info"),
                p(" "),
                bsButton("showOptDALYs909090Table",label="Show Result Table",style="primary"),
                p(" "),
                bsButton("showOptDALYs909090BrushedTable",label="Show Selected Result Table",style="primary")
                ),
            mainPanel(
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
            ),
        tabPanel("Budget",
            sidebarPanel(
                h4("Budget"),
                helpText("Please enter a health care budget value in the box below to see a subset of results that do not exceed that value."),
                numericInput("userBudget","Enter budget to subset results (2013 USD):", value = 0, min = 0, step = 1e+04),
                tags$b("Select output as 90-90-90 indicators or DALYs:"),
                p(" "),
                bsButton("showBudget909090",label="90-90-90",style="success"),
                p(" "),
                bsButton("showBudgetDALYs",label="DALYs",style="danger")
                ),
            mainPanel(
                DT::dataTableOutput('optBudgetTable', width = "100%")
                )
            )
        ),
    navbarMenu("Diagnostics",
            tabPanel("Single Plot",
                sidebarPanel(
                    selectInput('y','Y',VariableNames,selected="ART")
                    ),
                  mainPanel(
                    plotOutput('plotOne')
                    )
                ),
            tabPanel("All Plots",
                mainPanel(
                    plotOutput('plotTwo')
                    )
                )
        ),
    navbarMenu("More",
    tabPanel("Model Document",
        HTML('<iframe src=\"https://drive.google.com/file/d/0B02uVauBTUwhckJ1bG1QRmdwTGM/preview\"style=\"border: 0; position:absolute; top:50px; left:0; right:0; width:100%; height:100%\"></iframe>')
        ),
    # tabPanel("Country Input Data",
    #     HTML('<iframe src=\"https://drive.google.com/file/d/1rIMf-0vB77uwy7XO4rCM9Isd7_ReiCeOkwW1BvoxZw4/preview\"style=\"border: 0; position:absolute; top:50px; left:0; right:0; width:100%; height:100%\"></iframe>')
    #     ),
    # tabPanel("Incidence Estimates",
    #     HTML('<iframe src=\"https://drive.google.com/file/d/1-OVjcIl7m-QZt0T52WMcsU1Oxv4znTyc4eU0464L24I/preview\"style=\"border: 0; position:absolute; top:50px; left:0; right:0; width:100%; height:100%\"></iframe>')
    #     ),
    tabPanel("Raw Output",
        DT::dataTableOutput('outputTable')
        )
    )
))