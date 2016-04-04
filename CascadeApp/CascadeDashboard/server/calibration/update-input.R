observeEvent(input$uPLHIV, {
    print("value entered in PLHIV input:")
    print(input$uPLHIV)
    MasterData[["calib"]][MasterData[["calib"]]$year == input$uPLHIV_year & MasterData[["calib"]]$indicator == "PLHIV", "value"] <- input$uPLHIV
    print(MasterData)
})

observeEvent(input$uPLHIV_year, {
    print("value entered in PLHIV input:")
    print(input$uPLHIV_year)
    MasterData[["calib"]][MasterData[["calib"]]$year == input$uPLHIV_year & MasterData[["calib"]]$indicator == "PLHIV", "value"] <- input$uPLHIV
    print(MasterData)
})

observeEvent(input$uDIAG, {
    print("value entered in PLHIV DIAG input:")
    print(input$uDIAG)
    MasterData[["calib"]][MasterData[["calib"]]$year == input$uDIAG_year & MasterData[["calib"]]$indicator == "PLHIV Diagnosed", "value"] <- input$uDIAG
    print(MasterData)
})

observeEvent(input$uDIAG_year, {
    print("value entered in PLHIV DIAG input:")
    print(input$uDIAG_year)
    MasterData[["calib"]][MasterData[["calib"]]$year == input$uDIAG_year & MasterData[["calib"]]$indicator == "PLHIV Diagnosed", "value"] <- input$uDIAG
    print(MasterData)
})

observeEvent(input$uCARE, {
    print("value entered in PLHIV CARE input:")
    print(input$uCARE)
    MasterData[["calib"]][MasterData[["calib"]]$year == input$uCARE_year & MasterData[["calib"]]$indicator == "PLHIV in Care", "value"] <- input$uCARE
    print(MasterData)
})

observeEvent(input$uCARE_year, {
    print("value entered in PLHIV CARE input:")
    print(input$uCARE_year)
    MasterData[["calib"]][MasterData[["calib"]]$year == input$uCARE_year & MasterData[["calib"]]$indicator == "PLHIV in Care", "value"] <- input$uCARE
    print(MasterData)
})

observeEvent(input$uART, {
    print("value entered in PLHIV ART input:")
    print(input$uART)
    MasterData[["calib"]][MasterData[["calib"]]$year == input$uART_year & MasterData[["calib"]]$indicator == "PLHIV on ART", "value"] <- input$uART
    print(MasterData)
})

observeEvent(input$uART_year, {
    print("value entered in PLHIV ART input:")
    print(input$uART_year)
    MasterData[["calib"]][MasterData[["calib"]]$year == input$uART_year & MasterData[["calib"]]$indicator == "PLHIV on ART", "value"] <- input$uART
    print(MasterData)
})

observeEvent(input$uVIRAL, {
    print("value entered in PLHIV VIRAL input:")
    print(input$uVIRAL)
    MasterData[["calib"]][MasterData[["calib"]]$year == input$uVIRAL_year & MasterData[["calib"]]$indicator == "PLHIV Suppressed", "value"] <- input$uVIRAL
    print(MasterData)
})

observeEvent(input$uVIRAL_year, {
    print("value entered in PLHIV VIRAL input:")
    print(input$uVIRAL_year)
    MasterData[["calib"]][MasterData[["calib"]]$year == input$uVIRAL_year & MasterData[["calib"]]$indicator == "PLHIV Suppressed", "value"] <- input$uVIRAL
    print(MasterData)
})

