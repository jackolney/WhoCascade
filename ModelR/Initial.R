userPLHIV = 1000000
userDx = 831923
userCare = 504559
userLtfu = 42568
userTx = 247577
userVs = 115407


Initial <- c(
    UnDx_500 = (userPLHIV - userDx) * 0.5251,
    UnDx_350500 = (userPLHIV - userDx) * 0.2315,
    UnDx_200350 = (userPLHIV - userDx) * 0.2401,
    UnDx_200 = (userPLHIV - userDx) * 0.0033,

    Dx_500 = (userDx - userCare - userLtfu) * 0.5251,
    Dx_350500 = (userDx - userCare - userLtfu) * 0.2315,
    Dx_200350 = (userDx - userCare - userLtfu) * 0.2401,
    Dx_200 = (userDx - userCare - userLtfu) * 0.0033,

    Care_500 = (userCare - userTx) * 0.5251,
    Care_350500 = (userCare - userTx) * 0.2315,
    Care_200350 = (userCare - userTx) * 0.2401,
    Care_200 = (userCare - userTx) * 0.0033,

    PreLtfu_500 = 0 * 0.5251,
    PreLtfu_350500 = 0 * 0.2315,
    PreLtfu_200350 = 0 * 0.2401,
    PreLtfu_200 = 0 * 0.0033,

    Tx_Na_500 = (userTx - userVs) * 0.5251,
    Tx_Na_350500 = (userTx - userVs) * 0.2315,
    Tx_Na_200350 = (userTx - userVs) * 0.2401,
    Tx_Na_200 = (userTx - userVs) * 0.0033,

    Tx_A_500 = userVs * 0.5251,
    Tx_A_350500 = userVs * 0.2315,
    Tx_A_200350 = userVs * 0.2401,
    Tx_A_200 = userVs * 0.0033,

    Ltfu_500 = (userLtfu) * 0.5251,
    Ltfu_350500 = (userLtfu) * 0.2315,
    Ltfu_200350 = (userLtfu) * 0.2401,
    Ltfu_200 = (userLtfu) * 0.0033,

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