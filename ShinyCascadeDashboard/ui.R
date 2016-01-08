library(shiny)
library(ggplot2)
library(shinythemes)
library(DT)
library(shinyjs)
library(V8)
library(shinyBS)
library(shinydashboard)
# devtools::install_github("shinyTable", "trestletech")
# library(shinyTable)

source("content/introduction.R")
source("content/more.R")
source("content/setup.R")
source("content/parameters.R")
source("content/results.R")
source("content/optimisation.R")
source("content/wizard.R")

# extendShinyjs(text = 'shinyjs.hideSidebar = function(params) { $("body").addClass("sidebar-collapse") }')

dashboardPage(
    skin = "blue",
    dashboardHeader(title = "Cascade App",
        dropdownMenu(type = "notifications",
            notificationItem(
                text = "5 new users today",
                icon = icon("users")
            ),
            notificationItem(
                text = "12 items delivered",
                icon = icon("truck"),
                status = "success"
            ),
            notificationItem(
                text = "Server load at 86%",
                icon = icon("exclamation-triangle"),
                status = "warning"
            )
        ),
        dropdownMenu(type = "tasks", badgeStatus = "danger",
            taskItem(value = 70, color = "red",
                "Setup"
            )
        )
    ),

    dashboardSidebar(
            sidebarMenu(
                id = "sideBar",
                menuItem("Introduction", tabName = "introduction", icon = icon("home", class = "fa-lg fa-fw", lib = "font-awesome")),
                menuItem("Setup", tabName = "setup", icon = icon("cogs", class = "fa-lg fa-fw", lib = "font-awesome")),
                menuItem("Parameters", tabName = "parameters", icon = icon("wrench", class = "fa-lg fa-fw", lib = "font-awesome")),
                menuItem("Results", icon = icon("line-chart", class = "fa-lg fa-fw", lib = "font-awesome"),
                    menuSubItem("Your Cascade", tabName = "your_cascade"),
                    menuSubItem("The Care Cascade", tabName = "care_cascade"),
                    menuSubItem("The Power's Cascade", tabName = "powers_cascade"),
                    menuSubItem("90-90-90", tabName = "_909090"),
                    menuSubItem("Incidence / Mortality", tabName = "incidence_mortality")
                    ),
                menuItem("Optimisation", icon = icon("pie-chart", class = "fa-lg fa-fw", lib = "font-awesome"),
                    menuSubItem("Cost", tabName = "opt_cost"),
                    menuSubItem("Parameter Selection", tabName = "opt_parameter"),
                    menuSubItem("Results", tabName = "opt_results"),
                    menuSubItem("Budget", tabName = "opt_budget")
                    ),
                menuItem("More", icon = icon("question", class = "fa-lg fa-fw", lib = "font-awesome"),
                    menuSubItem("Model Document", tabName = "model_document"),
                    menuSubItem("Single Plot", tabName = "single_plot"),
                    menuSubItem("All Plots", tabName = "all_plots")
                    ),
                menuItem("Wizard", tabName = "test", icon = icon("magic", class = "fa-lg fa-fw", lib = "font-awesome"))
            )
        ),
    dashboardBody(
        # This contains tabItems(tabItem(tabName = "blah"))
        tabItems(
            Tab_Introduction,
            Tab_Setup,
            Tab_Parameters,

            # Results
            Tab_YourCascade,
            Tab_CareCascade,
            Tab_PowersCascade,
            Tab_909090,
            Tab_IncidenceMortality,

            # Optimisation
            Tab_Opt_Cost,
            Tab_Opt_Parameter,
            Tab_Opt_Results,
            Tab_Opt_Budget,

            # More
            Tab_ModelDocument,
            Tab_SinglePlot,
            Tab_AllPlots,

            # Wizard Test
            Tab_Result_Test
            )
        )
    )