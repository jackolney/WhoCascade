Initial <- c(
    UnDx_500 = 1e+4 * 1 * 0.5251,
    UnDx_350500 = 1e+4 * 1 * 0.2315,
    UnDx_200350 = 1e+4 * 1 * 0.2401,
    UnDx_200 = 1e+4 * 1 * 0.0033,

    Dx_500 = 1e+4 * 0 * 0.5251,
    Dx_350500 = 1e+4 * 0 * 0.2315,
    Dx_200350 = 1e+4 * 0 * 0.2401,
    Dx_200 = 1e+4 * 0 * 0.0033,

    Care_500 = 1e+4 * 0 * 0.5251,
    Care_350500 = 1e+4 * 0 * 0.2315,
    Care_200350 = 1e+4 * 0 * 0.2401,
    Care_200 = 1e+4 * 0 * 0.0033,

    PreLtfu_500 = 1e+4 * 0 * 0.5251,
    PreLtfu_350500 = 1e+4 * 0 * 0.2315,
    PreLtfu_200350 = 1e+4 * 0 * 0.2401,
    PreLtfu_200 = 1e+4 * 0 * 0.0033,

    Tx_Na_500 = 1e+4 * 0 * 0.5251,
    Tx_Na_350500 = 1e+4 * 0 * 0.2315,
    Tx_Na_200350 = 1e+4 * 0 * 0.2401,
    Tx_Na_200 = 1e+4 * 0 * 0.0033,

    Tx_A_500 = 1e+4 * 0 * 0.5251,
    Tx_A_350500 = 1e+4 * 0 * 0.2315,
    Tx_A_200350 = 1e+4 * 0 * 0.2401,
    Tx_A_200 = 1e+4 * 0 * 0.0033,

    Ltfu_500 = 0,
    Ltfu_350500 = 0,
    Ltfu_200350 = 0,
    Ltfu_200 = 0,

    # Keeping track
    NewInf = 0,
    HivMortality = 0,
    NaturalMortality = 0,

    # Transition costs
    Dx_Cost = 0,
    Care_Cost = 0,
    TxInit_Cost = 0,
    Retention_Cost = 0,

    # Annual costs
    AnnualTxCost = 0
)