output$plotCalibration <- renderPlot({
    # Dependency on 'REPEAT_calib' being hit
    # When 'repeat' button pressed, then we re-run calibration
    input$REPEAT_calib
    RunCalibration(data = MasterData, maxIterations = 1e4, maxError = input$maxError, limit = input$minResults)
}, height = 800, width = 'auto', bg = 'transparent')

output$plotData <- renderPlot({
    # add a dependency if we update data, like a 'review' changes.
    input$PREV_plhiv
    input$NEXT_viral
    message("plotData() running")
    BuildDataReviewPlot(data = MasterData$calib)
}, height = 400, width = 'auto', bg = 'transparent')
