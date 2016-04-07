tabItem(tabName = "_909090",
    column(width = 4,
        box(width = NULL,
            status = "warning",
            solidHeader = TRUE,
            title = "UNAIDS 90-90-90",
            p("By 2020, this is what the model predicts will be achieved in comparison to the UNAIDS goals of 90% diagnosed,
                90% on treatment and 90% virally suppressed. If you would like to see what changes can be made to resolve any
                inefficiencies in care, then click on the 'Optimisation' tab.")
        ),
        bsButton(inputId = "NEXT_909090", label = "Next", style = "success", size = "large", block = TRUE, icon = icon("arrow-right", class = "fa-lg fa-fw", lib = "font-awesome"))
    ),
    column(width = 8,
        box(width = NULL,
            status = "primary",
            plotOutput('plot909090')
        ),
        valueBoxOutput("vb_90"),
        valueBoxOutput("vb_9090"),
        valueBoxOutput("vb_909090")
    )
)
