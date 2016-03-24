output$plotCalibration <- renderPlot({
    # Dependency on 'cali_repeat' being hit
    input$calib_repeat

    RunCalibration(data = MasterData, iterations = 100)
}, height = 800, width = 'auto', bg = 'transparent')
