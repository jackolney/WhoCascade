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
    theme = shinytheme("Spacelab"),
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
        # img(src="HIVMC-logo.jpg",height = 72*2,width = 200*2),
        h1("Model Setup"),
        helpText("Here we will explain how to set the app up."),
        sidebarLayout(position="right",
            sidebarPanel(
                h4("Help Panel"),
                helpText("Fill in the boxes...")
                ),
            mainPanel(
                shinyjs::useShinyjs(),
                id = "setup-panel",
                actionButton("resetInput", "Reset!"),
                wellPanel(
                    selectInput("userCountry","Country:",CountryList,selected="Brazil")
                    ),
                wellPanel(
                    numericInput("userPLHIV","Number of PLHIV:",0)
                    ),
                wellPanel(
                    numericInput("userDx","Number of PLHIV diagnosed:",0)
                    ),
                wellPanel(
                    numericInput("userCare","Number of PLHIV in care:",0)
                    ),
                wellPanel(
                    numericInput("userTx","Number of PLHIV on ART:",0)
                    ),
                wellPanel(
                    numericInput("userVs","Number of PLHIV on ART and virally suppressed:",0)
                    ),
                wellPanel(
                    numericInput("userLtfu","Number of PLHIV LTFU:",0)
                    )
                # numericInput("rows","How many rows?",5),
                # selectInput("letter","Which letter?",LETTERS),
                # sliderInput("value","What value?",0,100,50)
                )
            )
        ),
    tabPanel("Single Plot",
        sidebarPanel(
            sliderInput('gamma','ART Initiation Rate',min=0,max=10,value=0.5,step=0.01),
            # selectInput('x','X',"time"),
            selectInput('y','Y',VariableNames,selected="ART")
            # selectInput('color','Color',c('None',names(dataset))),
            # checkboxInput('jitter','Jitter'),
            # checkboxInput('smooth','Smooth'),
            # selectInput('facet_row','Facet Row',c(None='.',names(dataset))),
            # selectInput('facet_col','Facet Column',c(None='.',names(dataset)))
          ),
          mainPanel(
            plotOutput('plotOne')
          )
        ),
    tabPanel("All Plots",
          mainPanel(
            plotOutput('plotTwo')
          )
        ),
    navbarMenu("More",
    tabPanel("PDF",
        HTML('<iframe src=\"https://drive.google.com/file/d/0B02uVauBTUwhd0U2cnY3dWJMcE0/preview\"style=\"border: 0; position:absolute; top:0; left:0; right:0; bottom:0; width:100%; height:100%\"></iframe>')
        ),
    tabPanel("Raw Output",
        DT::dataTableOutput('outputTable')
        )
    )


))