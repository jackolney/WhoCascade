library(shiny)
library(ggplot2)
library(shinythemes)
library(DT)
library(shinyjs)

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
    "NewInfProp")

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
        # img(src="HIVMC-logo.jpg",height = 72*2,width = 200*2),
        h1("Introduction"),
        helpText("Welcome."),
        sidebarLayout(position="right",
            sidebarPanel(
                h4("Quick Start"),
                helpText("If you want to skip the introduction and get modelling...")
                ),
            mainPanel(
                h3("Aims"),
                p("more details."),
                h3("The Model"),
                p("something about the model"),
                h3("Outcomes"),
                p("outcomes"),
                h3("What the model cannot do"),
                p("lots...")
                )
            )
        ),
    tabPanel("Setup",
        h1("Model Setup"),
        helpText("Here we will explain how to set the app up."),
        helpText("Perhaps include the WHO cascade flow diagram here?"),
        sidebarLayout(position="right",
            sidebarPanel(
                h4("Help Panel"),
                helpText("Please fill in all boxes with relevant data, then hit 'save' and wait for the confirmation below. Hit 'reset' to reset all values to zero, and hit 'demo' for a random set of values to be generated."),
                actionButton("saveInput", "SAVE"),
                p(" "),
                actionButton("resetInput", "RESET"),
                p(" "),
                actionButton("demoInput", "DEMO"),
                p(" "),
                textOutput('saveText')
                ),
            mainPanel(
                shinyjs::useShinyjs(),
                id = "setup-panel",
                wellPanel(
                    selectInput("userCountry","Country:",CountryList,selected="Brazil")
                    ),
                wellPanel(
                    numericInput("userPLHIV","Number of PLHIV:",0,min=0)
                    ),
                wellPanel(
                    numericInput("userDx","Number of PLHIV who have been diagnosed:",0,min=0)
                    ),
                wellPanel(
                    numericInput("userCare","Number of PLHIV in HIV care (including ART):",0,min=0)
                    ),
                wellPanel(
                    numericInput("userTx","Number of PLHIV in HIV care and on ART:",0,min=0)
                    ),
                wellPanel(
                    numericInput("userVs","Number of PLHIV in HIV care, on ART and virally suppressed:",0,min=0)
                    ),
                wellPanel(
                    numericInput("userLtfu","Number of PLHIV who dropped out of ART care:",0,min=0)
                    )
                )
            )
        ),
    tabPanel("Single Plot",
        sidebarPanel(
            sliderInput('gamma','ART Initiation Rate',min=0,max=10,value=0.5,step=0.01),
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
                        h4("Model Results"),
                        p("Here are some results...")
                    )
                )
            ),
        tabPanel("90-90-90",
            sidebarPanel(
                h4("UNAIDS 90-90-90"),
                p("Here are some results...")
                ),
            mainPanel(
                plotOutput('plot909090')
                )
            ),
        tabPanel("New Infections",
            sidebarPanel(
                h4("New Infections"),
                p("Here are some results...")
                ),
            mainPanel()
            ),
        tabPanel("AIDS Deaths",
            sidebarPanel(
                h4("AIDS Deaths"),
                p("Here are some results...")
                ),
            mainPanel()
            )
        ),
    tabPanel("Optimisation",
        sidebarPanel(
            h4("Model Optimisation"),
            p("This will contain an optimisation algorithm that will pick the most cost-effective intervention to enhance care."),
            helpText("Need a table giving exact values beneath."),
            actionButton("optimiseInput", "OPTIMISE")
            ),
        mainPanel()
        ),
    tabPanel("All Plots",
          mainPanel(
            plotOutput('plotTwo')
          )
        ),
    navbarMenu("More",
    tabPanel("PDF",
        HTML('<iframe src=\"https://drive.google.com/file/d/0B02uVauBTUwhd0U2cnY3dWJMcE0/preview\"style=\"border: 0; position:absolute; top:50px; left:0; right:0; width:100%; height:100%\"></iframe>')
        ),
    tabPanel("Raw Output",
        DT::dataTableOutput('outputTable')
        )
    )


))