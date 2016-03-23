output$plotCalibration <- renderPlot({
    result <- RunCalibration(data = MasterData, iterations = 100)
    print(result)
}, height = 800, width = 'auto', bg = 'transparent')
