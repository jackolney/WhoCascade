tabItem(tabName = "report",
    column(width = 8,
        box(width = NULL,
            status = "primary",
            solidHeader = TRUE,
            title = "Report Generation",
            "A report can be generated automatically that details all data entered into the site, all simulations run, and all results generated.
            The report is a pdf compiled using LaTeX, and contains all important information regarding the model. Please contact me with any bugs or issues at jack.olney11@imperial.ac.uk",
        )
    ),
    column(width = 4,
        box(width = NULL,
            status = "warning",
            solidHeader = TRUE,
            title = "Help Panel",
            "The report will be generated with RMarkdown, knitR and LaTeX."
        ),
        downloadButton(outputId = 'downloadReport', label = "CREATE REPORT", class = "btn btn-success btn-lg btn-block")
    )
)
