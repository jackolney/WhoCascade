output$CountryName <- renderText({return(paste("The Cascade of Care in",input$userCountry))})

output$outPLHIV <- renderPrint({ input$userPLHIV }, width = 300, quoted = FALSE)
output$outPLHIV_perc <- renderPrint({ noquote(paste(round(((input$userPLHIV / input$userPLHIV) * 100),2),
    "%", sep = ''))  }, width = 300, quoted = FALSE)

output$outDIAG <- renderPrint({ input$userDx }, width = 300, quoted = FALSE)
output$outDIAG_perc <- renderPrint({ noquote(paste(round(((input$userDx / input$userPLHIV) * 100),2),
    "%", sep = ''))  }, width = 300, quoted = FALSE)

output$outCARE <- renderPrint({ input$userCare }, width = 300, quoted = FALSE)
output$outCARE_perc <- renderPrint({ noquote(paste(round(((input$userCare / input$userPLHIV) * 100),2),
    "%", sep = ''))  }, width = 300, quoted = FALSE)

output$outART <- renderPrint({ input$userTx }, width = 300, quoted = FALSE)
output$outART_perc <- renderPrint({ noquote(paste(round(((input$userTx / input$userPLHIV) * 100),2),
    "%", sep = ''))  }, width = 300, quoted = FALSE)

output$outSUPP <- renderPrint({ input$userVs }, width = 300, quoted = FALSE)
output$outSUPP_perc <- renderPrint({ noquote(paste(round(((input$userVs / input$userPLHIV) * 100),2),
    "%", sep = ''))  }, width = 300, quoted = FALSE)

output$outLTFU <- renderPrint({ input$userLtfu }, width = 300, quoted = FALSE)
output$outLTFU_perc <- renderPrint({ noquote(paste(round(((input$userLtfu / input$userPLHIV) * 100),2),
    "%", sep = ''))  }, width = 300, quoted = FALSE)
