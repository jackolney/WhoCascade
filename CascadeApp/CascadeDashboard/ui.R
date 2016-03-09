source("ui/uihead.R", local = TRUE)

dashboardPage(
    skin = "blue",
    source("ui/dashboardheader.R", local = TRUE)$value,
    source("ui/sidebar.R",         local = TRUE)$value,
    dashboardBody(
        tabItems(
            source("ui/introduction.R", local = TRUE)$value,
            source("ui/country.R",      local = TRUE)$value,
            source("ui/setup.R",        local = TRUE)$value,
            source("ui/calibration.R",  local = TRUE)$value,
            source("ui/parameters.R",   local = TRUE)$value,

            # Results
            source("ui/results/res_yourcascade.R",        local = TRUE)$value,
            source("ui/results/res_carecascade.R",        local = TRUE)$value,
            source("ui/results/res_powerscascade.R",      local = TRUE)$value,
            source("ui/results/res_909090.R",             local = TRUE)$value,
            source("ui/results/res_incidencemortality.R", local = TRUE)$value,

            # Optimisation
            source("ui/optimisation/opt_cost.R",      local = TRUE)$value,
            source("ui/optimisation/opt_parameter.R", local = TRUE)$value,
            source("ui/optimisation/opt_results.R",   local = TRUE)$value,
            source("ui/optimisation/opt_budget.R",    local = TRUE)$value,

            # More
            source("ui/more/more_modeldocument.R", local = TRUE)$value,
            source("ui/more/more_singleplot.R",    local = TRUE)$value,
            source("ui/more/more_allplot.R",       local = TRUE)$value,

            # Wizard Test
            source("ui/wizard.R", local = TRUE)$value
        )
    )
)
