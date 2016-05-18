output$plotCalibration <- renderPlot({
    # Dependency on 'REPEAT_calib' being hit
    # When 'repeat' button pressed, then we re-run calibration
    input$REPEAT_calib
    RunCalibration(data = MasterData, maxIterations = 1e4, maxError = input$maxError, limit = input$minResults)
}, height = 800, width = 'auto', bg = 'transparent')
