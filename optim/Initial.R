userPLHIV = 1000000
userDx = 831923
userCare = 504559
userLtfu = 42568
userTx = 247577
userVs = 115407

Initial <- c(
    UnDx_500 = (userPLHIV - userDx) * prop_preART_500,
    UnDx_350500 = (userPLHIV - userDx) * prop_preART_350500,
    UnDx_250350 = (userPLHIV - userDx) * prop_preART_250350,
    UnDx_200250 = (userPLHIV - userDx) * prop_preART_200250,
    UnDx_100200 = (userPLHIV - userDx) * prop_preART_100200,
    UnDx_50100 = (userPLHIV - userDx) * prop_preART_50100,
    UnDx_50 = (userPLHIV - userDx) * prop_preART_50,

    Dx_500 = (userDx - userCare - userLtfu) * prop_preART_500,
    Dx_350500 = (userDx - userCare - userLtfu) * prop_preART_350500,
    Dx_250350 = (userDx - userCare - userLtfu) * prop_preART_250350,
    Dx_200250 = (userDx - userCare - userLtfu) * prop_preART_200250,
    Dx_100200 = (userDx - userCare - userLtfu) * prop_preART_100200,
    Dx_50100 = (userDx - userCare - userLtfu) * prop_preART_50100,
    Dx_50 = (userDx - userCare - userLtfu) * prop_preART_50,

    Care_500 = (userCare - userTx) * prop_preART_500,
    Care_350500 = (userCare - userTx) * prop_preART_350500,
    Care_250350 = (userCare - userTx) * prop_preART_250350,
    Care_200250 = (userCare - userTx) * prop_preART_200250,
    Care_100200 = (userCare - userTx) * prop_preART_100200,
    Care_50100 = (userCare - userTx) * prop_preART_50100,
    Care_50 = (userCare - userTx) * prop_preART_50,

    PreLtfu_500 = 0 * prop_preART_500,
    PreLtfu_350500 = 0 * prop_preART_350500,
    PreLtfu_250350 = 0 * prop_preART_250350,
    PreLtfu_200250 = 0 * prop_preART_200250,
    PreLtfu_100200 = 0 * prop_preART_100200,
    PreLtfu_50100 = 0 * prop_preART_50100,
    PreLtfu_50 = 0 * prop_preART_50,

    Tx_Na_500 = (userTx - userVs) * prop_onART_500,
    Tx_Na_350500 = (userTx - userVs) * prop_onART_350500,
    Tx_Na_250350 = (userTx - userVs) * prop_onART_250350,
    Tx_Na_200250 = (userTx - userVs) * prop_onART_200250,
    Tx_Na_100200 = (userTx - userVs) * prop_onART_100200,
    Tx_Na_50100 = (userTx - userVs) * prop_onART_50100,
    Tx_Na_50 = (userTx - userVs) * prop_onART_50,

    Tx_A_500 = userVs * prop_onART_500,
    Tx_A_350500 = userVs * prop_onART_350500,
    Tx_A_250350 = userVs * prop_onART_250350,
    Tx_A_200250 = userVs * prop_onART_200250,
    Tx_A_100200 = userVs * prop_onART_100200,
    Tx_A_50100 = userVs * prop_onART_50100,
    Tx_A_50 = userVs * prop_onART_50,

    Ltfu_500 = (userLtfu) * prop_preART_500,
    Ltfu_350500 = (userLtfu) * prop_preART_350500,
    Ltfu_250350 = (userLtfu) * prop_preART_250350,
    Ltfu_200250 = (userLtfu) * prop_preART_200250,
    Ltfu_100200 = (userLtfu) * prop_preART_100200,
    Ltfu_50100 = (userLtfu) * prop_preART_50100,
    Ltfu_50 = (userLtfu) * prop_preART_50,

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