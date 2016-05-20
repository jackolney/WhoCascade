source("ui/uihead.R", local = TRUE)

dashboardPage(
    skin = "blue",
    source("ui/dashboardheader.R",                        local = TRUE)$value,
    source("ui/sidebar.R",                                local = TRUE)$value,
    dashboardBody(
        includeCSS("styles.css"),
        tabItems(
            source("ui/introduction.R",                   local = TRUE)$value,

            # Setup
            source("ui/setup/country.R",                  local = TRUE)$value,
            source("ui/setup/data-review.R",              local = TRUE)$value,
            source("ui/setup/plhiv.R",                    local = TRUE)$value,
            source("ui/setup/diagnosis.R",                local = TRUE)$value,
            source("ui/setup/linkage.R",                  local = TRUE)$value,
            source("ui/setup/treatment.R",                local = TRUE)$value,
            source("ui/setup/suppression.R",              local = TRUE)$value,
            source("ui/setup/calibration.R",              local = TRUE)$value,

            # TO BE REMOVED (eventually)
            source("ui/setup/setup.R",                    local = TRUE)$value,
            source("ui/setup/parameters.R",               local = TRUE)$value,

            # Results
            source("ui/results/res_yourcascade.R",        local = TRUE)$value,
            source("ui/results/res_carecascade.R",        local = TRUE)$value,
            source("ui/results/res_powerscascade.R",      local = TRUE)$value,
            source("ui/results/res_909090.R",             local = TRUE)$value,
            source("ui/results/res_incidencemortality.R", local = TRUE)$value,

            # Optimisation
            source("ui/optimisation/opt-intro.R",         local = TRUE)$value,
            source("ui/optimisation/opt-wizard.R",        local = TRUE)$value,
            source("ui/optimisation/opt-cost.R",          local = TRUE)$value,
            source("ui/optimisation/opt-parameter.R",     local = TRUE)$value,
            source("ui/optimisation/opt-results.R",       local = TRUE)$value,
            source("ui/optimisation/opt-budget.R",        local = TRUE)$value,

            # More
            source("ui/more/more_report.R",               local = TRUE)$value,
            source("ui/more/more_modeldocument.R",        local = TRUE)$value,
            source("ui/more/more_singleplot.R",           local = TRUE)$value,
            source("ui/more/more_allplot.R",              local = TRUE)$value
        )
    )
)