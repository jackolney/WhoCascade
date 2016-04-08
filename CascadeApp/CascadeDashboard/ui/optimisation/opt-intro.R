tabItem(tabName = "opt-intro",
    column(width = 8,
        box(width = NULL,
            status = "primary",
            solidHeader = TRUE,
            title = "Optimisation Introduction",
            "Now that we have a calibration model and can make projections to 2020, we can now
            investigate optimal strategies to improve population health and also achieve the UNAIDS 90-90-90 targets.",
            tags$h2("Interventions"),
            "Below are details of the six broad interventions that we have at our disposal",
            tags$h4("HIV Testing"),
            "This intervention involves up-scaling HIV-testing",
            tags$h4("Linkage"),
            "This intervention involves up-scaling linkage",
            tags$h4("Pre-ART Retention"),
            "This intervention involves up-scaling pre-ART retention",
            tags$h4("ART Initiation"),
            "This intervention involves up-scaling ART retention",
            tags$h4("Adherence"),
            "This intervention involves up-scaling adherence to treatment",
            tags$h4("ART Retention"),
            "This intervention involves up-scaling ART retention"
        )
    ),
    column(width = 4,
        box(width = NULL,
            status = "warning",
            solidHeader = TRUE,
            title = "Help Panel",
            "This page details the working of the optimsation section of the site. Please hit 'Next' to proceed to optimisation. Further page options are available from the sidebar."
        ),
        bsButton(inputId = "wizardOpt_2", label = "Next", style = "success", size = "large", block = TRUE, icon = icon("arrow-right", class = "fa-lg fa-fw", lib = "font-awesome"))
    )
)
