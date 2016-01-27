Parameters <- reactive({
    GetParameters(
        input$rho,
        input$epsilon,
        input$gamma,
        input$omega,
        input$userART_All,
        input$userART_500,
        input$userART_350,
        input$userART_200,
        input$userDxUnitCost,
        input$userLinkageUnitCost,
        input$userAnnualCareUnit,
        input$userAnnualARTUnitCost
    )
})

Initial <- reactive({
    p <- GetParameters(
        input$rho,
        input$epsilon,
        input$gamma,
        input$omega,
        input$userART_All,
        input$userART_500,
        input$userART_350,
        input$userART_200,
        input$userDxUnitCost,
        input$userLinkageUnitCost,
        input$userAnnualCareUnit,
        input$userAnnualARTUnitCost
    )

    GetInitial(
        # All inputs from setup page,
        # PLUS, values from Parameters?
        )

    })

out <- reactive({

    Time <- seq(0,5,0.02)

    # Ability to turn off HIV incidence in the model.
    if(input$incidenceInput == TRUE) {
        theInitial <- Initial()
        Numerator <- NewInfections
        Denominator <- as.double(((theInitial[["UnDx_500"]] + theInitial[["Dx_500"]] + theInitial[["Care_500"]] + theInitial[["PreLtfu_500"]] + theInitial[["Tx_Na_500"]] + theInitial[["Ltfu_500"]]) * 1.35) + ((theInitial[["UnDx_350500"]] + theInitial[["Dx_350500"]] + theInitial[["Care_350500"]] + theInitial[["PreLtfu_350500"]] + theInitial[["Tx_Na_350500"]] + theInitial[["Ltfu_350500"]]) * 1) + ((theInitial[["UnDx_250350"]] + theInitial[["Dx_250350"]] + theInitial[["Care_250350"]] + theInitial[["PreLtfu_250350"]] + theInitial[["Tx_Na_250350"]] + theInitial[["Ltfu_250350"]] + theInitial[["UnDx_200250"]] + theInitial[["Dx_200250"]] + theInitial[["Care_200250"]] + theInitial[["PreLtfu_200250"]] + theInitial[["Tx_Na_200250"]] + theInitial[["Ltfu_200250"]]) * 1.64) + ((theInitial[["UnDx_100200"]] + theInitial[["Dx_100200"]] + theInitial[["Care_100200"]] + theInitial[["PreLtfu_100200"]] + theInitial[["Tx_Na_100200"]] + theInitial[["Ltfu_100200"]] + theInitial[["UnDx_50100"]] + theInitial[["Dx_50100"]] + theInitial[["Care_50100"]] + theInitial[["PreLtfu_50100"]] + theInitial[["Tx_Na_50100"]] + theInitial[["Ltfu_50100"]] + theInitial[["UnDx_50"]] + theInitial[["Dx_50"]] + theInitial[["Care_50"]] + theInitial[["PreLtfu_50"]] + theInitial[["Tx_Na_50"]] + theInitial[["Ltfu_50"]]) * 5.17) + ((theInitial[["Tx_A_500"]] + theInitial[["Tx_A_350500"]] + theInitial[["Tx_A_250350"]] + theInitial[["Tx_A_200250"]] + theInitial[["Tx_A_100200"]] + theInitial[["Tx_A_50100"]] + theInitial[["Tx_A_50"]]) * 0.1))
        # print(paste("Numerator =",Numerator))
        # print(paste("Denominator =",Denominator))
        Beta <<- Numerator / Denominator
    } else {
        Beta <<- 0
    }
    print(paste("Beta:",Beta))

    # The Model #
    theOut <- data.frame(ode(times=Time, y=Initial(), func=ComplexCascade, parms=Parameters()))
    # --------- #

    # Post-simulation mutation (creation of columns) etc.
    theOut <- mutate(theOut,N = UnDx_500 + UnDx_350500 + UnDx_250350 + UnDx_200250 + UnDx_100200 + UnDx_50100 + UnDx_50 + Dx_500 + Dx_350500 + Dx_250350 + Dx_200250 + Dx_100200 + Dx_50100 + Dx_50 + Care_500 + Care_350500 + Care_250350 + Care_200250 + Care_100200 + Care_50100 + Care_50 + PreLtfu_500 + PreLtfu_350500 + PreLtfu_250350 + PreLtfu_200250 + PreLtfu_100200 + PreLtfu_50100 + PreLtfu_50 + Tx_Na_500 + Tx_Na_350500 + Tx_Na_250350 + Tx_Na_200250 + Tx_Na_100200 + Tx_Na_50100 + Tx_Na_50 + Tx_A_500 + Tx_A_350500 + Tx_A_250350 + Tx_A_200250 + Tx_A_100200 + Tx_A_50100 + Tx_A_50 + Ltfu_500 + Ltfu_350500 + Ltfu_250350 + Ltfu_200250 + Ltfu_100200 + Ltfu_50100 + Ltfu_50)
    theOut <- mutate(theOut,ART = (Tx_Na_500 + Tx_Na_350500 + Tx_Na_250350 + Tx_Na_200250 + Tx_Na_100200 + Tx_Na_50100 + Tx_Na_50 + Tx_A_500 + Tx_A_350500 + Tx_A_250350 + Tx_A_200250 + Tx_A_100200 + Tx_A_50100 + Tx_A_50) / N)
    theOut <- mutate(theOut,UnDx = (UnDx_500 + UnDx_350500 + UnDx_250350 + UnDx_200250 + UnDx_100200 + UnDx_50100 + UnDx_50) / N)
    theOut <- mutate(theOut,Dx = (Dx_500 + Dx_350500 + Dx_250350 + Dx_200250 + Dx_100200 + Dx_50100 + Dx_50) / N)
    theOut <- mutate(theOut,Care = (Care_500 + Care_350500 + Care_250350 + Care_200250 + Care_100200 + Care_50100 + Care_50) / N)
    theOut <- mutate(theOut,PreLtfu = (PreLtfu_500 + PreLtfu_350500 + PreLtfu_250350 + PreLtfu_200250 + PreLtfu_100200 + PreLtfu_50100 + PreLtfu_50) / N)
    theOut <- mutate(theOut,Tx = (Tx_Na_500 + Tx_Na_350500 + Tx_Na_250350 + Tx_Na_200250 + Tx_Na_100200 + Tx_Na_50100 + Tx_Na_50 + Tx_A_500 + Tx_A_350500 + Tx_A_250350 + Tx_A_200250 + Tx_A_100200 + Tx_A_50100 + Tx_A_50) / N)
    theOut <- mutate(theOut,Vs = (Tx_A_500 + Tx_A_350500 + Tx_A_250350 + Tx_A_200250 + Tx_A_100200 + Tx_A_50100 + Tx_A_50) / N)
    theOut <- mutate(theOut,Ltfu = (Ltfu_500 + Ltfu_350500 + Ltfu_250350 + Ltfu_200250 + Ltfu_100200 + Ltfu_50100 + Ltfu_50) / N)
    theOut <- mutate(theOut,NaturalMortalityProp = NaturalMortality / N)
    theOut <- mutate(theOut,HivMortalityProp = HivMortality / N)
    theOut <- mutate(theOut,NewInfProp = NewInf / N)
    theOut <- mutate(theOut,TotalCost = Dx_Cost + Linkage_Cost + Annual_Care_Cost + Annual_ART_Cost)
    theOut <- mutate(theOut,cd4_500 = (UnDx_500 + Dx_500 + Care_500 + PreLtfu_500 + Tx_Na_500 + Tx_A_500 + Ltfu_500) / N)
    theOut <- mutate(theOut,cd4_350500 = (UnDx_350500 + Dx_350500 + Care_350500 + PreLtfu_350500 + Tx_Na_350500 + Tx_A_350500 + Ltfu_350500) / N)
    theOut <- mutate(theOut,cd4_250350 = (UnDx_250350 + Dx_250350 + Care_250350 + PreLtfu_250350 + Tx_Na_250350 + Tx_A_250350 + Ltfu_250350) / N)
    theOut <- mutate(theOut,cd4_200250 = (UnDx_200250 + Dx_200250 + Care_200250 + PreLtfu_200250 + Tx_Na_200250 + Tx_A_200250 + Ltfu_200250) / N)
    theOut <- mutate(theOut,cd4_100200 = (UnDx_100200 + Dx_100200 + Care_100200 + PreLtfu_100200 + Tx_Na_100200 + Tx_A_100200 + Ltfu_100200) / N)
    theOut <- mutate(theOut,cd4_50100 = (UnDx_50100 + Dx_50100 + Care_50100 + PreLtfu_50100 + Tx_Na_50100 + Tx_A_50100 + Ltfu_50100) / N)
    theOut <- mutate(theOut,cd4_50 = (UnDx_50 + Dx_50 + Care_50 + PreLtfu_50 + Tx_Na_50 + Tx_A_50 + Ltfu_50) / N)
    return(theOut)
})

# Reactive ranges for plotOpt
plotOpt_909090.ranges <- reactiveValues(x = NULL, y = NULL)
plotOpt_DALYs.ranges <- reactiveValues(x = NULL, y = NULL)
plotOpt_DALYs_909090.ranges <- reactiveValues(x = NULL, y = NULL)
