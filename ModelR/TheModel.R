ComplexCascade <- function(t, y, parms) {

    dUnDx_500 <- 0.5251 * (Beta * (((y[1] + y[5] + y[9] + y[13] + y[17] + y[25]) * 1.35) + ((y[2] + y[6] + y[10] + y[14] + y[18] + y[26]) * 1) + ((y[3] + y[7] + y[11] + y[15] + y[19] + y[27]) * 1.64) + ((y[4] + y[8] + y[12] + y[16] + y[20] + y[28]) * 5.17) + ((y[21] + y[22] + y[23] + y[24]) * 0.1))) - (parms[1] + parms[10] + parms[4] + parms[15] + parms[23]) * y[1]
    dUnDx_350500 <- 0.2315 * (Beta * (((y[1] + y[5] + y[9] + y[13] + y[17] + y[25]) * 1.35) + ((y[2] + y[6] + y[10] + y[14] + y[18] + y[26]) * 1) + ((y[3] + y[7] + y[11] + y[15] + y[19] + y[27]) * 1.64) + ((y[4] + y[8] + y[12] + y[16] + y[20] + y[28]) * 5.17) + ((y[21] + y[22] + y[23] + y[24]) * 0.1))) + parms[1] * y[1] - (parms[2] + parms[10] + parms[4] + parms[16] + parms[23]) * y[2]
    dUnDx_200350 <- 0.2401 * (Beta * (((y[1] + y[5] + y[9] + y[13] + y[17] + y[25]) * 1.35) + ((y[2] + y[6] + y[10] + y[14] + y[18] + y[26]) * 1) + ((y[3] + y[7] + y[11] + y[15] + y[19] + y[27]) * 1.64) + ((y[4] + y[8] + y[12] + y[16] + y[20] + y[28]) * 5.17) + ((y[21] + y[22] + y[23] + y[24]) * 0.1))) + parms[2] * y[2] - (parms[3] + parms[10] + parms[4] + parms[17] + parms[23]) * y[3]
    dUnDx_200 <- 0.0033 * (Beta * (((y[1] + y[5] + y[9] + y[13] + y[17] + y[25]) * 1.35) + ((y[2] + y[6] + y[10] + y[14] + y[18] + y[26]) * 1) + ((y[3] + y[7] + y[11] + y[15] + y[19] + y[27]) * 1.64) + ((y[4] + y[8] + y[12] + y[16] + y[20] + y[28]) * 5.17) + ((y[21] + y[22] + y[23] + y[24]) * 0.1))) + parms[3] * y[3] - (parms[10] + parms[4] + parms[18] + parms[23]) * y[4]

    dDx_500 <- + parms[4] * y[1] - (parms[1] + parms[9] + parms[5] + parms[15] + parms[23]) * y[5]
    dDx_350500 <- + parms[4] * y[2] + parms[1] * y[5] - (parms[2] + parms[9] + parms[5] + parms[16] + parms[23]) * y[6]
    dDx_200350 <- + parms[4] * y[3] + parms[2] * y[6] - (parms[3] + parms[9] + parms[5] + parms[17] + parms[23]) * y[7]
    dDx_200 <- + parms[4] * y[4] + parms[3] * y[7] - (parms[9] + parms[5] + parms[18] + parms[23]) * y[8]

    dCare_500 <- + parms[5] * y[5] - (parms[1] + parms[7] + parms[6] + parms[15] + parms[23]) * y[9]
    dCare_350500 <- + parms[5] * y[6] + parms[1] * y[9] - (parms[2] + parms[7] + parms[6] + parms[16] + parms[23]) * y[10]
    dCare_200350 <- + parms[5] * y[7] + parms[2] * y[10] - (parms[3] + parms[7] + parms[6] + parms[17] + parms[23]) * y[11]
    dCare_200 <- + parms[5] * y[8] + parms[3] * y[11] - (parms[7] + parms[6] + parms[18] + parms[23]) * y[12]

    dPreLtfu_500 <- + parms[6] * y[9] - (parms[1] + parms[8] + parms[15] + parms[23]) * y[13]
    dPreLtfu_350500 <- + parms[6] * y[10] + parms[1] * y[13] - (parms[2] + parms[8] + parms[16] + parms[23]) * y[14]
    dPreLtfu_200350 <- + parms[6] * y[11] + parms[2] * y[14] - (parms[3] + parms[8] + parms[17] + parms[23]) * y[15]
    dPreLtfu_200 <- + parms[6] * y[12] + parms[3] * y[15] - (parms[8] + parms[18] + parms[23]) * y[16]

    dTx_500 <- + parms[8] * y[13] + parms[7] * y[9] + parms[9] * y[5] + parms[10] * y[1] - (parms[11] + parms[12] + parms[15] + parms[23]) * y[17]
    dTx_350500 <- + parms[8] * y[14] + parms[7] * y[10] + parms[9] * y[6] + parms[10] * y[2] + parms[13] * y[19] - (parms[11] + parms[12] + parms[16] + parms[23]) * y[18]
    dTx_200350 <- + parms[8] * y[15] + parms[7] * y[11] + parms[9] * y[7] + parms[10] * y[3] + parms[14] * y[20] - (parms[13] + parms[11] + parms[12] + parms[17] + parms[23]) * y[19]
    dTx_200 <- + parms[8] * y[16] + parms[7] * y[12] + parms[9] * y[8] + parms[10] * y[4] - (parms[14] + parms[11] + parms[12] + parms[18] + parms[23]) * y[20]

    dVs_500 <- + parms[11] * y[17] - (parms[12] + parms[19] + parms[23]) * y[21]
    dVs_350500 <- + parms[11] * y[18] + parms[13] * y[23] - (parms[12] + parms[20] + parms[23]) * y[22]
    dVs_200350 <- + parms[11] * y[19] + parms[14] * y[24] - (parms[13] + parms[12] + parms[21] + parms[23]) * y[23]
    dVs_200 <- + parms[11] * y[20] - (parms[14] + parms[12] + parms[22] + parms[23]) * y[24]

    dLtfu_500 <- + parms[12] * (y[21] + y[17]) - (parms[1] + parms[15] + parms[23]) * y[25]
    dLtfu_350500 <- + parms[12] * (y[22] + y[18]) + parms[1] * y[25] - (parms[2] + parms[16] + parms[23]) * y[26]
    dLtfu_200350 <- + parms[12] * (y[23] + y[19]) + parms[2] * y[26] - (parms[3] + parms[17] + parms[23]) * y[27]
    dLtfu_200 <- + parms[12] * (y[24] + y[20]) + parms[3] * y[27] - (parms[18] + parms[23]) * y[28]


    dNewInf <- Beta * (((y[1] + y[5] + y[9] + y[13] + y[17] + y[25]) * 1.35) + ((y[2] + y[6] + y[10] + y[14] + y[18] + y[26]) * 1) + ((y[3] + y[7] + y[11] + y[15] + y[19] + y[27]) * 1.64) + ((y[4] + y[8] + y[12] + y[16] + y[20] + y[28]) * 5.17) + ((y[21] + y[22] + y[23] + y[24]) * 0.1))

    dHivMortality <- parms[15] * (y[1] + y[5] + y[9] + y[13] + y[17] + y[25]) + parms[16] * (y[2] + y[6] + y[10] + y[14] + y[18] + y[26]) + parms[17] * (y[3] + y[7] + y[11] + y[15] + y[19] + y[27]) + parms[18] * (y[4] + y[8] + y[12] + y[16] + y[20] + y[28]) + parms[19] * y[21] + parms[20] * y[22] + parms[21] * y[23] + parms[22] * y[24]

    dNaturalMortality <- parms[23] * (y[1] + y[2] + y[3] + y[4] + y[5] + y[6] + y[7] + y[8] + y[9] + y[10] + y[11] + y[12] + y[13] + y[14] + y[15] + y[16] + y[17] + y[18] + y[19] + y[20] + y[21] + y[22] + y[23] + y[24] + y[25] + y[26] + y[27] + y[28])

    dDx_Cost <- (parms[4] * (y[1] + y[2] + y[3] + y[4])) * parms[24]
    
    dCare_Cost <- (parms[5] * (y[5] + y[6] + y[7] + y[8])) * parms[25]
    
    dTxInit_Cost <- (parms[7] * (y[9] + y[10] + y[11] + y[12])) * parms[26]
    
    dRetention_Cost <- (parms[12] * (y[17] + y[18] + y[19] + y[20] + y[21] + y[22] + y[23] + y[24])) * parms[27]

    dAnnualTxCost <- (y[17] + y[18] + y[19] + y[20] + y[21] + y[22] + y[23] + y[24]) * parms[28]
    
    list(c(
        dUnDx_500,
        dUnDx_350500,
        dUnDx_200350,
        dUnDx_200,
        dDx_500,
        dDx_350500,
        dDx_200350,
        dDx_200,
        dCare_500,
        dCare_350500,
        dCare_200350,
        dCare_200,
        dPreLtfu_500,
        dPreLtfu_350500,
        dPreLtfu_200350,
        dPreLtfu_200,
        dTx_500,
        dTx_350500,
        dTx_200350,
        dTx_200,
        dVs_500,
        dVs_350500,
        dVs_200350,
        dVs_200,
        dLtfu_500,
        dLtfu_350500,
        dLtfu_200350,
        dLtfu_200,
        dNewInf,
        dHivMortality,
        dNaturalMortality,
        dDx_Cost,
        dCare_Cost,
        dTxInit_Cost,
        dRetention_Cost,
        dAnnualTxCost))
}