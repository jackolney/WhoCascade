# Data Table
# Table illustrating
output$dataTable <- DT::renderDataTable({
    # rely on button press
    input$viewDataTable

    return(
        datatable(MasterData$calib,
            extensions = 'Scroller',
            options = list(
                pageLength = 100,
                deferRender = TRUE,
                scrollY = 500,
                scroller = TRUE,
                autoWidth = TRUE,
                order = list(list(3, 'asc'))
            )
        )
    )
})
# order = list(list(2, 'asc'), list(4, 'desc'))
