library(shiny)
library(ggplot2)
library(shinythemes)
library(DT)
library(shinyjs)
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
    "Retention_Cost"
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
    navbarPage("Cascade Test App",
    theme = shinytheme("spacelab"),
    tabPanel("Introduction",
        # img(src="HIVMC-logo.jpg",height=100 * 0.75,width=300 * 0.75),
        h1("Introduction"),
        helpText("Welcome to the Cascade App."),
        sidebarLayout(position="right",
            sidebarPanel(
                h4("Quick Start"),
                helpText("If you want to skip the introduction and get modelling, jump to 'setup' and click on 'DEMO'.")
                ),
            mainPanel(
                p("With the release of the Consolidated Information Guidelines for HIV by the World Health Organization (WHO)
                in May 2015, a set of indicators have been agreed upon, based on the cascade of HIV services relating to impact in
                terms of HIV incidence and mortality (see below). These guidelines provide a framework for countries to assess the current state
                of care and identify any immediate deficiencies and bottlenecks preventing patients from progressing to treatment."),
                p("Furthermore, as we pass the Millennium Development Goals of 2015 and focus attention on the UNAIDS 90-90-90 targets for 2020,
                countries will be keen to understand whether they are on the right trajectory to achieve these goals. For this purpose, data from
                countries can be input into a mathematical model and used to project forward to estimate future incidence, AIDS-deaths and the ascertainment of the 90-90-90 goals."),
                # img(src="WHOGuidelinesCascade.png",height=400,width=900),
                h3("Aims"),
                tags$ol(
                    tags$li("To develop a simple mathematical model to document and analyse the state of HIV care. This flexible model will take a wide range
                        of data, characterise individuals into discrete care categories, and build upon a set of assumptions regarding transmission, duration of
                        infection, progression through care and death, to project changes in incidence, mortality and care five years into the future to 2020."),
                    tags$li("Using data from 26 countries, the model will be calibrated to each setting in turn to provide country-specific results."),
                    tags$li("The model will be used to assess the current state of care, ascertaining gaps requiring immediate attention, before identifying what
                        future changes need to be made, that differ from what has been done so far, for countries to be on track to achieve the 90-90-90 goals set out by UNAIDS."),
                    tags$li("A range of interventions will then be simulated, each targetting a different aspect of care, to identify the most cost-effective strategy for reconciling any deficiencies in care."),
                    tags$li("These findings will help countries prioritise strategies and will provide an estimate of the costs required to achieve future targets.")
                ),
                h3("The Model"),
                p("The simplified structure of the proposed model is shown below. State compartments do not exactly correlate with the indicators in Consolidated Indicator Guidelines, as compartments in the model 
                    must be discrete and exhaustive, while the indicators listed are not all discrete; for example, ‘Knowing HIV status’ includes all patients who are in care, on treatment, virally suppressed and 
                    lost from care as long as they are diagnosed. However, the model is able to take these indicators and seperate them into their individual components to specify the initial conditions for simulations."),
                img(src="ModelSimple.jpg",height=200,width=850),
                h3("Outcomes"),
                tags$ol(
                    tags$li("Predict achievement of UNAIDS 90-90-90 targets in 2020"),
                    tags$li("Identify strategies to achieve 90-90-90 targets at minimal cost."),
                    tags$li("Illustrate the change in distribution of care between 2015 and 2020."),
                    tags$li("Predict new infections in that time-frame."),
                    tags$li("Predict AIDS deaths in that time-frame too.")
                    ),
                h4("Contributors"),
                p("Jack J Olney, Jeffrey W Eaton, Ellen McRobie & Timothy B Hallett")
                )
            )
        ),
    tabPanel("Setup",
        h1("Model Setup"),
        sidebarLayout(position="right",
            sidebarPanel(
                h4("Help Panel"),
                helpText("Please fill in all boxes with relevant data, then hit 'save' and wait for the confirmation below. Hit 'reset' to reset all values to zero, and hit 'demo' for a random set of values to be generated."),
                checkboxInput("incidenceInput","HIV Incidence",value=TRUE),
                actionButton("saveInput", "SAVE"),
                p(" "),
                actionButton("resetInput", "RESET"),
                p(" "),
                actionButton("demoInput", "DEMO"),
                p(" "),
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
                    )
                )
            )
        ),
    tabPanel("Parameters",
        h1("Parameter Values"),
        sidebarLayout(position="right",
            sidebarPanel(
                h4("Help Panel"),
                helpText("You do not need to alter any of these values."),
                actionButton("resetParameters", "RESET PARAMETERS"),
                p(" "),
                tableOutput("parameterTable"),
                p("Details regarding the origin of these parameter values are found in the 'Model Document' under the 'More' tab.")
                ),
            mainPanel(
                shinyjs::useShinyjs(),
                id = "parameter-panel",
                helpText("Here we show the parameter values used in the back end of the model. See below for a detail diagram of the model, or alternatively click 'more' and 'Model Document' to see the accompanying model document."),
                imageOutput("modelFlowImage"),
                br(), br(),
                sliderInput('rho','Diagnosis rate (rho):',min=0,max=5,value=0.5,step=0.01,width=1000),
                sliderInput('epsilon','Care seeking rate (epsilon):',min=0,max=5,value=0.5,step=0.01,width=1000),
                sliderInput('gamma','ART initiation rate (gamma):',min=0,max=5,value=0.5,step=0.01,width=1000),
                sliderInput('theta','Viral suppression rate (theta):',min=0,max=5,value=2.28,step=0.01,width=1000),
                sliderInput('omega','ART dropout rate (omega):',min=0,max=5,value=0.01,step=0.01,width=1000)
                )
            )
        ),
    tabPanel("Single Plot",
        sidebarPanel(
            selectInput('y','Y',VariableNames,selected="ART")
          ),
          mainPanel(
            plotOutput('plotOne')
          )
        ),
    navbarMenu("Results",
        tabPanel("The Care Cascade",
            fluidRow(
                plotOutput('plotCascade')
                ),
            fluidRow(
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
    tabPanel("Optimisation",
        sidebarPanel(
            h4("Model Optimisation"),
            p("To identify the most cost-effective strategy for achieving the UNAIDS 90-90-90 targets hit the 'optimise' button..."),
            helpText("Figure and table will turn grey while optimisation algorithm runs."),
            actionButton("optimiseInput", "OPTIMISE"),
            br(), br(),
            tableOutput("optimisationTable"),
            h4("Cost of optimisation"),
            tableOutput("optimisationCostTable"),
            h4("Unit cost table"),
            tableOutput("unitCostTable"),
            helpText("What about the counterfactual?")
            ),
        mainPanel(
            plotOutput('plotOptimised909090')
            )
        ),
    tabPanel("All Plots",
          mainPanel(
            plotOutput('plotTwo')
          )
        ),
    navbarMenu("More",
    tabPanel("Model Document",
        HTML('<iframe src=\"https://drive.google.com/file/d/0B02uVauBTUwhd0U2cnY3dWJMcE0/preview\"style=\"border: 0; position:absolute; top:50px; left:0; right:0; width:100%; height:100%\"></iframe>')
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