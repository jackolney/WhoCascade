library(shiny)
library(ggplot2)
library(shinythemes)

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

library(shinythemes)

shinyUI(
    navbarPage("Cascade Test App",
    # theme = shinytheme("spacelab"),
    tabPanel("Page 1",
        sidebarPanel(
            sliderInput('gamma', 'ART Initiation Rate', min=0, max=10, value=0.5, step=0.01),
            # selectInput('x', 'X', "time"),
            selectInput('y', 'Y', VariableNames)
            # selectInput('color', 'Color', c('None', names(dataset))),
            # checkboxInput('jitter', 'Jitter'),
            # checkboxInput('smooth', 'Smooth'),
            # selectInput('facet_row', 'Facet Row', c(None='.', names(dataset))),
            # selectInput('facet_col', 'Facet Column', c(None='.', names(dataset)))
          ),

          mainPanel(
            plotOutput('plot')
          )
        ),
    tabPanel("Component 2"),
    navbarMenu("More",
    tabPanel("Sub-Component A"),
    tabPanel("Sub-Component B"))


))