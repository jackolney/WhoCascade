tabItem(tabName = "report",
    column(width = 8,
        box(width = NULL,
            status = "primary",
            solidHeader = TRUE,
            title = "Report Generation",
            "This page will contain details on how to print a pdf report of all the analysis conducted in an individual session on this site."
        )
    ),
    column(width = 4,
        box(width = NULL,
            status = "warning",
            solidHeader = TRUE,
            title = "Help Panel",
            "The report will be generated with RMarkdown and knitR."
        ),
        downloadButton(outputId = 'downloadReport', label = "CREATE REPORT", class = "btn btn-success btn-lg btn-block")
    )
)
