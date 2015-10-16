Initial <- c(
    UnDx_500 = 1e+4 * 1 * 0.58,
    UnDx_350500 = 1e+4 * 1 * 0.23,
    UnDx_200350 = 1e+4 * 1 * 0.16,
    UnDx_200 = 1e+4 * 1 * 0.03,

    Dx_500 = 1e+4 * 0 * 0.58,
    Dx_350500 = 1e+4 * 0 * 0.23,
    Dx_200350 = 1e+4 * 0 * 0.16,
    Dx_200 = 1e+4 * 0 * 0.03,

    Care_500 = 1e+4 * 0 * 0.58,
    Care_350500 = 1e+4 * 0 * 0.23,
    Care_200350 = 1e+4 * 0 * 0.16,
    Care_200 = 1e+4 * 0 * 0.03,

    PreLtfu_500 = 1e+4 * 0 * 0.58,
    PreLtfu_350500 = 1e+4 * 0 * 0.23,
    PreLtfu_200350 = 1e+4 * 0 * 0.16,
    PreLtfu_200 = 1e+4 * 0 * 0.03,

    Tx_500 = 1e+4 * 0 * 0.58,
    Tx_350500 = 1e+4 * 0 * 0.23,
    Tx_200350 = 1e+4 * 0 * 0.16,
    Tx_200 = 1e+4 * 0 * 0.03,

    Vs_500 = 1e+4 * 0 * 0.58,
    Vs_350500 = 1e+4 * 0 * 0.23,
    Vs_200350 = 1e+4 * 0 * 0.16,
    Vs_200 = 1e+4 * 0 * 0.03,

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