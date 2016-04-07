output$plotCalibration <- renderPlot({
    # Dependency on 'cali_repeat' being hit
    input$calib_repeat

    # RunCalibration(data = MasterData, maxIterations = 1e4, maxError = input$maxError, limit = input$minResults)
    RunCalibration(data = MasterData, maxIterations = 1e4, maxError = input$maxError, limit = 10)

}, height = 800, width = 'auto', bg = 'transparent')
