GetInitial <- reactive({
    c(
        UnDx_500 = (input$userPLHIV - input$userDx) * p_preArt500,
        UnDx_350500 = (input$userPLHIV - input$userDx) * p_preArt350500,
        UnDx_250350 = (input$userPLHIV - input$userDx) * p_preArt250350,
        UnDx_200250 = (input$userPLHIV - input$userDx) * p_preArt200250,
        UnDx_100200 = (input$userPLHIV - input$userDx) * p_preArt100200,
        UnDx_50100 = (input$userPLHIV - input$userDx) * p_preArt50100,
        UnDx_50 = (input$userPLHIV - input$userDx) * p_preArt50,

        Dx_500 = (input$userDx - input$userCare - input$userLtfu) * p_preArt500,
        Dx_350500 = (input$userDx - input$userCare - input$userLtfu) * p_preArt350500,
        Dx_250350 = (input$userDx - input$userCare - input$userLtfu) * p_preArt250350,
        Dx_200250 = (input$userDx - input$userCare - input$userLtfu) * p_preArt200250,
        Dx_100200 = (input$userDx - input$userCare - input$userLtfu) * p_preArt100200,
        Dx_50100 = (input$userDx - input$userCare - input$userLtfu) * p_preArt50100,
        Dx_50 = (input$userDx - input$userCare - input$userLtfu) * p_preArt50,

        Care_500 = (input$userCare - input$userTx) * p_preArt500,
        Care_350500 = (input$userCare - input$userTx) * p_preArt350500,
        Care_250350 = (input$userCare - input$userTx) * p_preArt250350,
        Care_200250 = (input$userCare - input$userTx) * p_preArt200250,
        Care_100200 = (input$userCare - input$userTx) * p_preArt100200,
        Care_50100 = (input$userCare - input$userTx) * p_preArt50100,
        Care_50 = (input$userCare - input$userTx) * p_preArt50,

        PreLtfu_500 = 0 * p_preArt500,
        PreLtfu_350500 = 0 * p_preArt350500,
        PreLtfu_250350 = 0 * p_preArt250350,
        PreLtfu_200250 = 0 * p_preArt200250,
        PreLtfu_100200 = 0 * p_preArt100200,
        PreLtfu_50100 = 0 * p_preArt50100,
        PreLtfu_50 = 0 * p_preArt50,

        Tx_Na_500 = (input$userTx - input$userVs) * p_onArt500,
        Tx_Na_350500 = (input$userTx - input$userVs) * p_onArt350500,
        Tx_Na_250350 = (input$userTx - input$userVs) * p_onArt250350,
        Tx_Na_200250 = (input$userTx - input$userVs) * p_onArt200250,
        Tx_Na_100200 = (input$userTx - input$userVs) * p_onArt100200,
        Tx_Na_50100 = (input$userTx - input$userVs) * p_onArt50100,
        Tx_Na_50 = (input$userTx - input$userVs) * p_onArt50,

        Tx_A_500 = input$userVs * p_onArt500,
        Tx_A_350500 = input$userVs * p_onArt350500,
        Tx_A_250350 = input$userVs * p_onArt250350,
        Tx_A_200250 = input$userVs * p_onArt200250,
        Tx_A_100200 = input$userVs * p_onArt100200,
        Tx_A_50100 = input$userVs * p_onArt50100,
        Tx_A_50 = input$userVs * p_onArt50,

        Ltfu_500 = (input$userLtfu) * p_preArt500,
        Ltfu_350500 = (input$userLtfu) * p_preArt350500,
        Ltfu_250350 = (input$userLtfu) * p_preArt250350,
        Ltfu_200250 = (input$userLtfu) * p_preArt200250,
        Ltfu_100200 = (input$userLtfu) * p_preArt100200,
        Ltfu_50100 = (input$userLtfu) * p_preArt50100,
        Ltfu_50 = (input$userLtfu) * p_preArt50,

        # Keeping track
        NewInf = 0,
        HivMortality = 0,
        NaturalMortality = 0,

        # Costs
        Dx_Cost = 0,
        Linkage_Cost = 0,
        Annual_Care_Cost = 0,
        Annual_ART_Cost = 0
    )
})
