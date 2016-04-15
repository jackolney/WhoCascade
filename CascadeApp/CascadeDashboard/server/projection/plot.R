output$plotValidation_PLHIV <- renderPlot({
    GenYourCascadePlot(1, font = "Avenir Next")
}, height = 400,
   width = 'auto',
   bg = "transparent")

output$plotValidation_DIAG <- renderPlot({
    GenYourCascadePlot(2, font = "Avenir Next")
}, height = 400,
   width = 'auto',
   bg = "transparent")

output$plotValidation_CARE <- renderPlot({
    GenYourCascadePlot(3, font = "Avenir Next")
}, height = 400,
   width = 'auto',
   bg = "transparent")

output$plotValidation_ART <- renderPlot({
    GenYourCascadePlot(4, font = "Avenir Next")
}, height = 400,
   width = 'auto',
   bg = "transparent")

output$plotValidation_SUPP <- renderPlot({
    GenYourCascadePlot(5, font = "Avenir Next")
}, height = 400,
   width = 'auto',
   bg = "transparent")

output$plotCascade <- renderPlot({
    GenCascadePlot(font = "Avenir Next")
}, height = 400,
   width = 'auto')

output$plotPowersCascade <- renderPlot({
    GenPowersCascadePlot(font = "Avenir Next")
}, height = 400,
   width = 'auto')

output$plot909090 <- renderPlot({
    Gen909090Plot(font = "Avenir Next")
}, height = 500,
   width = 'auto')

output$plotNewInf <- renderPlot({
    GenNewInfPlot(wizard = FALSE)
}, height = 500,
   width = 'auto')

output$plotAidsDeaths <- renderPlot({
    GenAidsDeathsPlot(wizard = FALSE)
}, height = 500,
   width = 'auto')

output$plotSingle <- renderPlot({
    GenSinglePlot()
}, height = 500,
   width = 'auto')

output$plotAll <- renderPlot({
    GenAllPlot()
}, height = 800,
   width = 'auto')

output$plotNewInf_wizard <- renderPlot({
    GenNewInfPlot(wizard = TRUE)
}, height = 240,
   width = 'auto')

output$plotAidsDeaths_wizard <- renderPlot({
    GenAidsDeathsPlot(wizard = TRUE)
}, height = 240,
   width = 'auto')

output$plotCascade_wizard <- renderPlot({
    GenCascadePlot(font = "Avenir Next")
}, height = 400,
   width = 'auto')
