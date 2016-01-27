p <- GetParameters()

GetInitial <- function(
    in_userPLHIV,
    in_userDx,
    in_userCare,
    in_userTx,
    in_userVs,
    in_userLtfu) {
     default <- c(
        UnDx_500 = (in_userPLHIV - in_userDx) * p[["Iota_1"]],
        UnDx_350500 = (input$userPLHIV - input$userDx) * prop_preART_350500,
        UnDx_250350 = (input$userPLHIV - input$userDx) * prop_preART_250350,
        UnDx_200250 = (input$userPLHIV - input$userDx) * prop_preART_200250,
        UnDx_100200 = (input$userPLHIV - input$userDx) * prop_preART_100200,
        UnDx_50100 = (input$userPLHIV - input$userDx) * prop_preART_50100,
        UnDx_50 = (input$userPLHIV - input$userDx) * prop_preART_50,

        Dx_500 = (input$userDx - input$userCare - input$userLtfu) * prop_preART_500,
        Dx_350500 = (input$userDx - input$userCare - input$userLtfu) * prop_preART_350500,
        Dx_250350 = (input$userDx - input$userCare - input$userLtfu) * prop_preART_250350,
        Dx_200250 = (input$userDx - input$userCare - input$userLtfu) * prop_preART_200250,
        Dx_100200 = (input$userDx - input$userCare - input$userLtfu) * prop_preART_100200,
        Dx_50100 = (input$userDx - input$userCare - input$userLtfu) * prop_preART_50100,
        Dx_50 = (input$userDx - input$userCare - input$userLtfu) * prop_preART_50,

        Care_500 = (input$userCare - input$userTx) * prop_preART_500,
        Care_350500 = (input$userCare - input$userTx) * prop_preART_350500,
        Care_250350 = (input$userCare - input$userTx) * prop_preART_250350,
        Care_200250 = (input$userCare - input$userTx) * prop_preART_200250,
        Care_100200 = (input$userCare - input$userTx) * prop_preART_100200,
        Care_50100 = (input$userCare - input$userTx) * prop_preART_50100,
        Care_50 = (input$userCare - input$userTx) * prop_preART_50,

        PreLtfu_500 = 0 * prop_preART_500,
        PreLtfu_350500 = 0 * prop_preART_350500,
        PreLtfu_250350 = 0 * prop_preART_250350,
        PreLtfu_200250 = 0 * prop_preART_200250,
        PreLtfu_100200 = 0 * prop_preART_100200,
        PreLtfu_50100 = 0 * prop_preART_50100,
        PreLtfu_50 = 0 * prop_preART_50,

        Tx_Na_500 = (input$userTx - input$userVs) * prop_onART_500,
        Tx_Na_350500 = (input$userTx - input$userVs) * prop_onART_350500,
        Tx_Na_250350 = (input$userTx - input$userVs) * prop_onART_250350,
        Tx_Na_200250 = (input$userTx - input$userVs) * prop_onART_200250,
        Tx_Na_100200 = (input$userTx - input$userVs) * prop_onART_100200,
        Tx_Na_50100 = (input$userTx - input$userVs) * prop_onART_50100,
        Tx_Na_50 = (input$userTx - input$userVs) * prop_onART_50,

        Tx_A_500 = input$userVs * prop_onART_500,
        Tx_A_350500 = input$userVs * prop_onART_350500,
        Tx_A_250350 = input$userVs * prop_onART_250350,
        Tx_A_200250 = input$userVs * prop_onART_200250,
        Tx_A_100200 = input$userVs * prop_onART_100200,
        Tx_A_50100 = input$userVs * prop_onART_50100,
        Tx_A_50 = input$userVs * prop_onART_50,

        Ltfu_500 = (input$userLtfu) * prop_preART_500,
        Ltfu_350500 = (input$userLtfu) * prop_preART_350500,
        Ltfu_250350 = (input$userLtfu) * prop_preART_250350,
        Ltfu_200250 = (input$userLtfu) * prop_preART_200250,
        Ltfu_100200 = (input$userLtfu) * prop_preART_100200,
        Ltfu_50100 = (input$userLtfu) * prop_preART_50100,
        Ltfu_50 = (input$userLtfu) * prop_preART_50,

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
    default
}
