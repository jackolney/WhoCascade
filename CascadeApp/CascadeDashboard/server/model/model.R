# Main Model Call
CallModel <- reactive({
    print("CallModel() called.")
    # Setup #
    # This does ignore the cascade::parameter and cascade::initial
    time <- seq(0, 5, 0.02)
    y <- GetInitial()
    p <- GetParameters()
    beta <- GetBeta(y, p)

    # Parameter update
    p[62] <- beta # p[["beta"]]
    p[50] <- p_preArt500 # p[["Iota_1"]]
    p[51] <- p_preArt350500 # p[["Iota_2"]]
    p[52] <- p_preArt250350 # p[["Iota_3"]]
    p[53] <- p_preArt200250 # p[["Iota_4"]]
    p[54] <- p_preArt100200 # p[["Iota_5"]]
    p[55] <- p_preArt50100 # p[["Iota_6"]]
    p[56] <- p_preArt50 # p[["Iota_7"]]

    # The Model #
    result <- deSolve::ode(times = time, y = y, func = "derivs", parms = p, initfunc = "initmod", dllname = "cascade")
    # --------- #

    result <- cbind(result, N = rowSums(result[, c(
        "UnDx_500", "UnDx_350500", "UnDx_250350", "UnDx_200250", "UnDx_100200", "UnDx_50100", "UnDx_50",
        "Dx_500", "Dx_350500", "Dx_250350", "Dx_200250", "Dx_100200", "Dx_50100", "Dx_50",
        "Care_500", "Care_350500", "Care_250350", "Care_200250", "Care_100200", "Care_50100", "Care_50",
        "PreLtfu_500", "PreLtfu_350500", "PreLtfu_250350", "PreLtfu_200250", "PreLtfu_100200", "PreLtfu_50100", "PreLtfu_50",
        "Tx_Na_500", "Tx_Na_350500", "Tx_Na_250350", "Tx_Na_200250", "Tx_Na_100200", "Tx_Na_50100", "Tx_Na_50",
        "Tx_A_500", "Tx_A_350500", "Tx_A_250350", "Tx_A_200250", "Tx_A_100200", "Tx_A_50100", "Tx_A_50",
        "Ltfu_500", "Ltfu_350500", "Ltfu_250350", "Ltfu_200250", "Ltfu_100200", "Ltfu_50100", "Ltfu_50")]
    ))

    result <- cbind(result,
        ART = rowSums(result[, c(
            "Tx_Na_500", "Tx_Na_350500", "Tx_Na_250350", "Tx_Na_200250", "Tx_Na_100200", "Tx_Na_50100", "Tx_Na_50",
            "Tx_A_500", "Tx_A_350500", "Tx_A_250350", "Tx_A_200250", "Tx_A_100200", "Tx_A_50100", "Tx_A_50"
            )]) / result[, "N"],

        UnDx = rowSums(result[, c(
            "UnDx_500", "UnDx_350500", "UnDx_250350", "UnDx_200250", "UnDx_100200", "UnDx_50100", "UnDx_50"
            )]) / result[, "N"],

        Dx = rowSums(result[, c(
            "Dx_500", "Dx_350500", "Dx_250350", "Dx_200250", "Dx_100200", "Dx_50100", "Dx_50"
            )]) / result[, "N"],

        Care = rowSums(result[, c(
                "Care_500", "Care_350500", "Care_250350", "Care_200250", "Care_100200", "Care_50100", "Care_50"
                )]) / result[, "N"],

        PreLtfu = rowSums(result[, c(
                "PreLtfu_500", "PreLtfu_350500", "PreLtfu_250350", "PreLtfu_200250", "PreLtfu_100200", "PreLtfu_50100", "PreLtfu_50"
                )]) / result[, "N"],

        Tx = rowSums(result[, c(
                "Tx_Na_500", "Tx_Na_350500", "Tx_Na_250350", "Tx_Na_200250", "Tx_Na_100200", "Tx_Na_50100", "Tx_Na_50",
                "Tx_A_500", "Tx_A_350500", "Tx_A_250350", "Tx_A_200250", "Tx_A_100200", "Tx_A_50100", "Tx_A_50"
                )]) / result[, "N"],

        Vs = rowSums(result[, c(
                "Tx_A_500", "Tx_A_350500", "Tx_A_250350", "Tx_A_200250", "Tx_A_100200", "Tx_A_50100", "Tx_A_50"
                )]) / result[, "N"],

        Ltfu = rowSums(result[, c(
                "Ltfu_500", "Ltfu_350500", "Ltfu_250350", "Ltfu_200250", "Ltfu_100200", "Ltfu_50100", "Ltfu_50"
                )]) / result[, "N"],

        NaturalMortalityProp = result[, "NaturalMortality"] / result[, "N"],

        HivMortalityProp = result[, "HivMortality"] / result[, "N"],

        NewInfProp = result[, "NewInf"] / result[, "N"],

        TotalCost = rowSums(result[, c(
                "Dx_Cost", "Linkage_Cost", "Annual_Care_Cost", "Annual_ART_Cost"
                )]),

        cd4_500 = rowSums(result[, c(
                "UnDx_500", "Dx_500", "Care_500", "PreLtfu_500", "Tx_Na_500", "Tx_A_500", "Ltfu_500"
                )]) / result[, "N"],

        cd4_350500 = rowSums(result[, c(
                "UnDx_350500", "Dx_350500", "Care_350500", "PreLtfu_350500", "Tx_Na_350500", "Tx_A_350500", "Ltfu_350500"
                )]) / result[, "N"],

        cd4_250350 = rowSums(result[, c(
                "UnDx_250350", "Dx_250350", "Care_250350", "PreLtfu_250350", "Tx_Na_250350", "Tx_A_250350", "Ltfu_250350"
                )]) / result[, "N"],

        cd4_200250 = rowSums(result[, c(
                "UnDx_200250", "Dx_200250", "Care_200250", "PreLtfu_200250", "Tx_Na_200250", "Tx_A_200250", "Ltfu_200250"
                )]) / result[, "N"],

        cd4_100200 = rowSums(result[, c(
                "UnDx_100200", "Dx_100200", "Care_100200", "PreLtfu_100200", "Tx_Na_100200", "Tx_A_100200", "Ltfu_100200"
                )]) / result[, "N"],

        cd4_50100 = rowSums(result[, c(
                "UnDx_50100", "Dx_50100", "Care_50100", "PreLtfu_50100", "Tx_Na_50100", "Tx_A_50100", "Ltfu_50100"
                )]) / result[, "N"],

        cd4_50 = rowSums(result[, c(
                "UnDx_50", "Dx_50", "Care_50", "PreLtfu_50", "Tx_Na_50", "Tx_A_50", "Ltfu_50"
                )]) / result[, "N"]
    )
    return(as.data.frame(result))
})
