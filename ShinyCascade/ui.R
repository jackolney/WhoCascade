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
    "UnDx_200350",
    "UnDx_200",
    "Dx_500",
    "Dx_350500",
    "Dx_200350",
    "Dx_200",
    "Care_500",
    "Care_350500",
    "Care_200350",
    "Care_200",
    "Tx_500",
    "Tx_350500",
    "Tx_200350",
    "Tx_200",
    "Vs_500",
    "Vs_350500",
    "Vs_200350",
    "Vs_200",
    "Ltfu_500",
    "Ltfu_350500",
    "Ltfu_200350",
    "Ltfu_200",
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
    "Care_Cost",
    "Tx_Cost",
    "Retention_Cost",
    "TotalCost"
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

shinyUI(
    navbarPage("Cascade App",
    theme = shinytheme("spacelab"),
    tabPanel("Introduction",
        # img(src="HIVMC-logo.jpg",height=100 * 0.75,width=300 * 0.75),
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
                p("With the release of the Consolidated Information Guidelines for HIV by the World Health Organization (WHO)
                in May 2015, a set of indicators have been agreed upon, based on the cascade of HIV services relating to impact in
                terms of HIV incidence and mortality (see below). These guidelines provide a framework for countries to assess the current state
                of care and identify any immediate deficiencies and bottlenecks preventing patients from progressing to treatment."),
                p("Furthermore, as we pass the Millennium Development Goals of 2015 and focus attention on the UNAIDS 90-90-90 targets for 2020,
                countries will be keen to understand whether they are on the right trajectory to achieve these goals. For this purpose, data from
                countries can be input into a mathematical model and used to estimate future incidence, AIDS-deaths and the ascertainment of the 90-90-90 goals."),
                img(src="WHOGuidelinesCascade.png",height=400 * 0.9,width=900 * 0.9),
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
                p("The simplified structure of the model is shown below. State compartments do not exactly correlate with the indicators in Consolidated Indicator Guidelines, as compartments in the model 
                    must be discrete and exhaustive, while the indicators listed are not all discrete; for example, ‘Knowing HIV status’ includes all patients who are in care, on treatment, virally suppressed and 
                    lost from care as long as they are diagnosed. However, the model is able to reconcile this by taking individual indicators and seperating them into their components to specify the initial conditions for simulations."),
                helpText("More details on the model can be found in the following pages, along with a detailed description under the 'more' table and 'Model Document'."),
                img(src="ModelSimple.png",height=200,width=850),
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
                p(" "),
                helpText("Console output:"),
                textOutput('saveText'),
                textOutput('warningText')
                ),
            mainPanel(
                shinyjs::useShinyjs(),
                id = "setup-panel",
                helpText("Select country and fill in boxes to specify the initial values of the model. 
                    The boxes correlate with indicators in the Consolidated Information Guidelines (shown below), with numbers in brackets corresponding to numbers on the indicator figure."),
                img(src="WHOGuidelinesCascade.png",height=400 * 0.9,width=900 * 0.9),
                br(), br(),
                wellPanel(
                    selectInput("userCountry","Country:",CountryList,selected="Brazil")
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
                imageOutput("modelFlowImage"),
                br(), br(), br(), br(), br(),
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
                            tags$li("% On Treatment = # persons diagnosed, in care, on ART, not virally suppressed / PLHIV"),
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
                h4("Optimisation - Cost"),
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
                    numericInput("userDxUnitCost","Unit cost of diagnosing a patient (USD):",2,min=0,step=0.01)
                    ),
                wellPanel(
                    numericInput("userCxUnitCost","Unit cost of getting a patient into care (USD):",2,min=0,step=0.01)
                    ),
                wellPanel(
                    numericInput("userTxUnitCost","Unit cost of getting a patient onto treatment (USD):",2,min=0,step=0.01)
                    ),
                wellPanel(
                    numericInput("userRxUnitCost","Unit cost of retaining a patient on treatment (USD):",2,min=0,step=0.01)
                    )
                )
            ),
        tabPanel("Results",
            sidebarPanel(
                h4("Optimisation"),
                p("To identify the most cost-effective strategy for achieving the UNAIDS 90-90-90 targets hit the 'optimise' button."),
                helpText("Table will turn grey while optimisation algorithm runs."),
                bsButton("optimiseInput",label="OPTIMISE",style="info"),
                helpText("After optimisation is complete, flick back to 'parameter' tab to update model parameters, before returning to see the updated 90-90-90 plot."),
                tableOutput("optimisationTable"),
                h4("Cost of optimisation"),
                tableOutput("optimisationCostTable"),
                helpText("What about the counterfactual?"),
                helpText("What about if omega is already specified (ART dropout), do we still include that as a lever for optimisation?")
                ),
            mainPanel(
                plotOutput('plotOptimised909090')
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
        HTML('<iframe src=\"https://drive.google.com/file/d/0B02uVauBTUwhazg1Y1U5RGpkeEk/preview\"style=\"border: 0; position:absolute; top:50px; left:0; right:0; width:100%; height:100%\"></iframe>')
        ),
    tabPanel("Country Input Data",
        HTML('<iframe src=\"https://drive.google.com/file/d/1rIMf-0vB77uwy7XO4rCM9Isd7_ReiCeOkwW1BvoxZw4/preview\"style=\"border: 0; position:absolute; top:50px; left:0; right:0; width:100%; height:100%\"></iframe>')
        ),
    tabPanel("Incidence Estimates",
        HTML('<iframe src=\"https://drive.google.com/file/d/1-OVjcIl7m-QZt0T52WMcsU1Oxv4znTyc4eU0464L24I/preview\"style=\"border: 0; position:absolute; top:50px; left:0; right:0; width:100%; height:100%\"></iframe>')
        ),
    tabPanel("Raw Output",
        DT::dataTableOutput('outputTable')
        )
    )
))