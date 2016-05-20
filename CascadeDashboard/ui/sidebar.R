dashboardSidebar(
    sideBarMini = TRUE,
    sidebarMenu(
        id = "sideBar",
        menuItem("Introduction", tabName = "introduction", icon = icon("home", class = "fa-lg fa-fw", lib = "font-awesome")),
        menuItem("Setup", icon = icon("cogs", class = "fa-lg fa-fw", lib = "font-awesome"),
            menuSubItem("Country",                tabName = "country"),
            menuSubItem("Data Review",            tabName = "data-review"),
            menuSubItem("People Living with HIV", tabName = "plhiv"),
            menuSubItem("Diagnosed Individuals",  tabName = "diagnosis"),
            menuSubItem("HIV Care Coverage",      tabName = "linkage"),
            menuSubItem("Treatment Coverage",     tabName = "treatment"),
            menuSubItem("Viral Suppression",      tabName = "suppression"),
            menuSubItem("Calibration",            tabName = "calibration"),
            menuSubItem("Setup",                  tabName = "setup"),
            menuSubItem("Parameters",             tabName = "parameters")
            ),
        menuItem("Results", icon = icon("line-chart", class = "fa-lg fa-fw", lib = "font-awesome"),
            menuSubItem("Your Cascade",           tabName = "your_cascade"),
            menuSubItem("The Care Cascade",       tabName = "care_cascade"),
            menuSubItem("The Power's Cascade",    tabName = "powers_cascade"),
            menuSubItem("90-90-90",               tabName = "_909090"),
            menuSubItem("Incidence / Mortality",  tabName = "incidence_mortality")
            ),
        menuItem("Optimisation", icon = icon("pie-chart", class = "fa-lg fa-fw", lib = "font-awesome"),
            menuSubItem("Introduction",           tabName = "opt-intro"),
            menuSubItem("Wizard",                 tabName = "opt-wizard"),
            menuSubItem("Cost",                   tabName = "opt-cost"),
            menuSubItem("Parameter Selection",    tabName = "opt-parameter"),
            menuSubItem("Results",                tabName = "opt-results"),
            menuSubItem("Budget",                 tabName = "opt-budget")
            ),
        menuItem("More", icon = icon("question", class = "fa-lg fa-fw", lib = "font-awesome"),
            menuSubItem("Create Report",          tabName = "report"),
            menuSubItem("Model Document",         tabName = "model_document"),
            menuSubItem("Single Plot",            tabName = "single_plot"),
            menuSubItem("All Plots",              tabName = "all_plots")
            )
    )
)