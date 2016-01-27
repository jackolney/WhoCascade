# Main Model Call
out <- reactive({

    Time <- seq(0, 5, 0.02)

    # Ability to turn off HIV incidence in the model.
    if(input$incidenceInput == TRUE) {
        theInitial <- GetInitial()
        Numerator <- NewInfections
        Denominator <- as.double(((theInitial[["UnDx_500"]] + theInitial[["Dx_500"]] + theInitial[["Care_500"]] + theInitial[["PreLtfu_500"]] + theInitial[["Tx_Na_500"]] + theInitial[["Ltfu_500"]]) * 1.35) + ((theInitial[["UnDx_350500"]] + theInitial[["Dx_350500"]] + theInitial[["Care_350500"]] + theInitial[["PreLtfu_350500"]] + theInitial[["Tx_Na_350500"]] + theInitial[["Ltfu_350500"]]) * 1) + ((theInitial[["UnDx_250350"]] + theInitial[["Dx_250350"]] + theInitial[["Care_250350"]] + theInitial[["PreLtfu_250350"]] + theInitial[["Tx_Na_250350"]] + theInitial[["Ltfu_250350"]] + theInitial[["UnDx_200250"]] + theInitial[["Dx_200250"]] + theInitial[["Care_200250"]] + theInitial[["PreLtfu_200250"]] + theInitial[["Tx_Na_200250"]] + theInitial[["Ltfu_200250"]]) * 1.64) + ((theInitial[["UnDx_100200"]] + theInitial[["Dx_100200"]] + theInitial[["Care_100200"]] + theInitial[["PreLtfu_100200"]] + theInitial[["Tx_Na_100200"]] + theInitial[["Ltfu_100200"]] + theInitial[["UnDx_50100"]] + theInitial[["Dx_50100"]] + theInitial[["Care_50100"]] + theInitial[["PreLtfu_50100"]] + theInitial[["Tx_Na_50100"]] + theInitial[["Ltfu_50100"]] + theInitial[["UnDx_50"]] + theInitial[["Dx_50"]] + theInitial[["Care_50"]] + theInitial[["PreLtfu_50"]] + theInitial[["Tx_Na_50"]] + theInitial[["Ltfu_50"]]) * 5.17) + ((theInitial[["Tx_A_500"]] + theInitial[["Tx_A_350500"]] + theInitial[["Tx_A_250350"]] + theInitial[["Tx_A_200250"]] + theInitial[["Tx_A_100200"]] + theInitial[["Tx_A_50100"]] + theInitial[["Tx_A_50"]]) * 0.1))
        # print(paste("Numerator =",Numerator))
        # print(paste("Denominator =",Denominator))
        Beta <<- Numerator / Denominator
    } else {
        Beta <<- 0
    }
    print(paste("Beta:",Beta))

    # Setup
    y <- GetInitial()
    p <- GetParameters()

    # Parameter update
    p[["beta"]] <- Beta
    p[["Iota_1"]] <- p_preArt500
    p[["Iota_2"]] <- p_preArt350500
    p[["Iota_3"]] <- p_preArt250350
    p[["Iota_4"]] <- p_preArt200250
    p[["Iota_5"]] <- p_preArt100200
    p[["Iota_6"]] <- p_preArt50100
    p[["Iota_7"]] <- p_preArt50

    # The Model #
    # theOut <- data.frame(ode(times = Time, y = y, func = ComplexCascade, parms = p))
    # --------- #

    # C CODE #
    # result <- cascade_derivs(y, p)
    # ref <- ComplexCascade(0, y, p)[[1]]
    # testthat::expect_equal(result, ref, info = "Non-equal model function error.")
    result <- data.frame(deSolve::ode(times = Time, y = y, func = "derivs", parms = p, initfunc = "initmod", dllname = "cascade"))
    # expect_equal(theOut, result, check.attributes = FALSE, tolerance = 1e-16, info = "Non-equal ode() return error.")
    # --------- #

    # Post-simulation mutation (creation of columns) etc.
    result <- mutate(result,N = UnDx_500 + UnDx_350500 + UnDx_250350 + UnDx_200250 + UnDx_100200 + UnDx_50100 + UnDx_50 + Dx_500 + Dx_350500 + Dx_250350 + Dx_200250 + Dx_100200 + Dx_50100 + Dx_50 + Care_500 + Care_350500 + Care_250350 + Care_200250 + Care_100200 + Care_50100 + Care_50 + PreLtfu_500 + PreLtfu_350500 + PreLtfu_250350 + PreLtfu_200250 + PreLtfu_100200 + PreLtfu_50100 + PreLtfu_50 + Tx_Na_500 + Tx_Na_350500 + Tx_Na_250350 + Tx_Na_200250 + Tx_Na_100200 + Tx_Na_50100 + Tx_Na_50 + Tx_A_500 + Tx_A_350500 + Tx_A_250350 + Tx_A_200250 + Tx_A_100200 + Tx_A_50100 + Tx_A_50 + Ltfu_500 + Ltfu_350500 + Ltfu_250350 + Ltfu_200250 + Ltfu_100200 + Ltfu_50100 + Ltfu_50)
    result <- mutate(result,ART = (Tx_Na_500 + Tx_Na_350500 + Tx_Na_250350 + Tx_Na_200250 + Tx_Na_100200 + Tx_Na_50100 + Tx_Na_50 + Tx_A_500 + Tx_A_350500 + Tx_A_250350 + Tx_A_200250 + Tx_A_100200 + Tx_A_50100 + Tx_A_50) / N)
    result <- mutate(result,UnDx = (UnDx_500 + UnDx_350500 + UnDx_250350 + UnDx_200250 + UnDx_100200 + UnDx_50100 + UnDx_50) / N)
    result <- mutate(result,Dx = (Dx_500 + Dx_350500 + Dx_250350 + Dx_200250 + Dx_100200 + Dx_50100 + Dx_50) / N)
    result <- mutate(result,Care = (Care_500 + Care_350500 + Care_250350 + Care_200250 + Care_100200 + Care_50100 + Care_50) / N)
    result <- mutate(result,PreLtfu = (PreLtfu_500 + PreLtfu_350500 + PreLtfu_250350 + PreLtfu_200250 + PreLtfu_100200 + PreLtfu_50100 + PreLtfu_50) / N)
    result <- mutate(result,Tx = (Tx_Na_500 + Tx_Na_350500 + Tx_Na_250350 + Tx_Na_200250 + Tx_Na_100200 + Tx_Na_50100 + Tx_Na_50 + Tx_A_500 + Tx_A_350500 + Tx_A_250350 + Tx_A_200250 + Tx_A_100200 + Tx_A_50100 + Tx_A_50) / N)
    result <- mutate(result,Vs = (Tx_A_500 + Tx_A_350500 + Tx_A_250350 + Tx_A_200250 + Tx_A_100200 + Tx_A_50100 + Tx_A_50) / N)
    result <- mutate(result,Ltfu = (Ltfu_500 + Ltfu_350500 + Ltfu_250350 + Ltfu_200250 + Ltfu_100200 + Ltfu_50100 + Ltfu_50) / N)
    result <- mutate(result,NaturalMortalityProp = NaturalMortality / N)
    result <- mutate(result,HivMortalityProp = HivMortality / N)
    result <- mutate(result,NewInfProp = NewInf / N)
    result <- mutate(result,TotalCost = Dx_Cost + Linkage_Cost + Annual_Care_Cost + Annual_ART_Cost)
    result <- mutate(result,cd4_500 = (UnDx_500 + Dx_500 + Care_500 + PreLtfu_500 + Tx_Na_500 + Tx_A_500 + Ltfu_500) / N)
    result <- mutate(result,cd4_350500 = (UnDx_350500 + Dx_350500 + Care_350500 + PreLtfu_350500 + Tx_Na_350500 + Tx_A_350500 + Ltfu_350500) / N)
    result <- mutate(result,cd4_250350 = (UnDx_250350 + Dx_250350 + Care_250350 + PreLtfu_250350 + Tx_Na_250350 + Tx_A_250350 + Ltfu_250350) / N)
    result <- mutate(result,cd4_200250 = (UnDx_200250 + Dx_200250 + Care_200250 + PreLtfu_200250 + Tx_Na_200250 + Tx_A_200250 + Ltfu_200250) / N)
    result <- mutate(result,cd4_100200 = (UnDx_100200 + Dx_100200 + Care_100200 + PreLtfu_100200 + Tx_Na_100200 + Tx_A_100200 + Ltfu_100200) / N)
    result <- mutate(result,cd4_50100 = (UnDx_50100 + Dx_50100 + Care_50100 + PreLtfu_50100 + Tx_Na_50100 + Tx_A_50100 + Ltfu_50100) / N)
    result <- mutate(result,cd4_50 = (UnDx_50 + Dx_50 + Care_50 + PreLtfu_50 + Tx_Na_50 + Tx_A_50 + Ltfu_50) / N)
    return(result)
})
