userPLHIV = 1000000
userDx = 831923
userCare = 504559
userLtfu = 42568
userTx = 247577
userVs = 115407

Initial <- c(
    UnDx_500 = (userPLHIV - userDx) * 0.5251,
    UnDx_350500 = (userPLHIV - userDx) * 0.2315,
    UnDx_250350 = (userPLHIV - userDx) * 0.1787,
    UnDx_200250 = (userPLHIV - userDx) * 0.0615,
    UnDx_100200 = (userPLHIV - userDx) * 0.0011,
    UnDx_50100 = (userPLHIV - userDx) * 0.0008,
    UnDx_50 = (userPLHIV - userDx) * 0.0014,

    Dx_500 = (userDx - userCare - userLtfu) * 0.5251,
    Dx_350500 = (userDx - userCare - userLtfu) * 0.2315,
    Dx_250350 = (userDx - userCare - userLtfu) * 0.1787,
    Dx_200250 = (userDx - userCare - userLtfu) * 0.0615,
    Dx_100200 = (userDx - userCare - userLtfu) * 0.0011,
    Dx_50100 = (userDx - userCare - userLtfu) * 0.0008,
    Dx_50 = (userDx - userCare - userLtfu) * 0.0014,

    Care_500 = (userCare - userTx) * 0.5251,
    Care_350500 = (userCare - userTx) * 0.2315,
    Care_250350 = (userCare - userTx) * 0.1787,
    Care_200250 = (userCare - userTx) * 0.0615,
    Care_100200 = (userCare - userTx) * 0.0011,
    Care_50100 = (userCare - userTx) * 0.0008,
    Care_50 = (userCare - userTx) * 0.0014,

    PreLtfu_500 = 0 * 0.5251,
    PreLtfu_350500 = 0 * 0.2315,
    PreLtfu_250350 = 0 * 0.1787,
    PreLtfu_200250 = 0 * 0.0615,
    PreLtfu_100200 = 0 * 0.0011,
    PreLtfu_50100 = 0 * 0.0008,
    PreLtfu_50 = 0 * 0.0014,

    Tx_Na_500 = (userTx - userVs) * 0.5251,
    Tx_Na_350500 = (userTx - userVs) * 0.2315,
    Tx_Na_250350 = (userTx - userVs) * 0.1787,
    Tx_Na_200250 = (userTx - userVs) * 0.0615,
    Tx_Na_100200 = (userTx - userVs) * 0.0011,
    Tx_Na_50100 = (userTx - userVs) * 0.0008,
    Tx_Na_50 = (userTx - userVs) * 0.0014,

    Tx_A_500 = userVs * 0.5251,
    Tx_A_350500 = userVs * 0.2315,
    Tx_A_250350 = userVs * 0.1787,
    Tx_A_200250 = userVs * 0.0615,
    Tx_A_100200 = userVs * 0.0011,
    Tx_A_50100 = userVs * 0.0008,
    Tx_A_50 = userVs * 0.0014,

    Ltfu_500 = (userLtfu) * 0.5251,
    Ltfu_350500 = (userLtfu) * 0.2315,
    Ltfu_250350 = (userLtfu) * 0.1787,
    Ltfu_200250 = (userLtfu) * 0.0615,
    Ltfu_100200 = (userLtfu) * 0.0011,
    Ltfu_50100 = (userLtfu) * 0.0008,
    Ltfu_50 = (userLtfu) * 0.0014,

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