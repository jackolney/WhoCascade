ComplexCascade <- function(t, y, parms) {

    dUnDx_500 <- 0.58 * (Beta * (((y[1] + y[5] + y[9] + y[13] + y[21]) * 1.35) + ((y[2] + y[6] + y[10] + y[14] + y[22]) * 1) + ((y[3] + y[7] + y[11] + y[15] + y[23]) * 1.64) + ((y[4] + y[8] + y[12] + y[16] + y[24]) * 5.17) + ((y[17] + y[18] + y[19] + y[20]) * 0.1))) - (parms[4] + parms[1] + parms[10] + parms[18]) * y[1]
    dUnDx_350500 <- 0.23 * (Beta * (((y[1] + y[5] + y[9] + y[13] + y[21]) * 1.35) + ((y[2] + y[6] + y[10] + y[14] + y[22]) * 1) + ((y[3] + y[7] + y[11] + y[15] + y[23]) * 1.64) + ((y[4] + y[8] + y[12] + y[16] + y[24]) * 5.17) + ((y[17] + y[18] + y[19] + y[20]) * 0.1))) + parms[1] * y[1] - (parms[4] + parms[2] + parms[11] + parms[18]) * y[2]
    dUnDx_200350 <- 0.16 * (Beta * (((y[1] + y[5] + y[9] + y[13] + y[21]) * 1.35) + ((y[2] + y[6] + y[10] + y[14] + y[22]) * 1) + ((y[3] + y[7] + y[11] + y[15] + y[23]) * 1.64) + ((y[4] + y[8] + y[12] + y[16] + y[24]) * 5.17) + ((y[17] + y[18] + y[19] + y[20]) * 0.1))) + parms[2] * y[2] - (parms[4] + parms[3] + parms[12] + parms[18]) * y[3]
    dUnDx_200 <- 0.03 * (Beta * (((y[1] + y[5] + y[9] + y[13] + y[21]) * 1.35) + ((y[2] + y[6] + y[10] + y[14] + y[22]) * 1) + ((y[3] + y[7] + y[11] + y[15] + y[23]) * 1.64) + ((y[4] + y[8] + y[12] + y[16] + y[24]) * 5.17) + ((y[17] + y[18] + y[19] + y[20]) * 0.1))) + parms[3] * y[3] - (parms[4] + parms[13] + parms[18]) * y[4]

    dDx_500 <- + parms[4] * y[1] - (parms[19] + parms[1] + parms[10] + parms[18]) * y[5]
    dDx_350500 <- + parms[4] * y[2] + parms[1] * y[5] - (parms[19] + parms[2] + parms[11] + parms[18]) * y[6]
    dDx_200350 <- + parms[4] * y[3] + parms[2] * y[6] - (parms[19] + parms[3] + parms[12] + parms[18]) * y[7]
    dDx_200 <- + parms[4] * y[4] + parms[3] * y[7] - (parms[19] + parms[13] + parms[18]) * y[8]

    dCare_500 <- + parms[19] * y[5] - (parms[5] + parms[1] + parms[10] + parms[18]) * y[9]
    dCare_350500 <- + parms[19] * y[6] + parms[1] * y[9] - (parms[5] + parms[2] + parms[11] + parms[18]) * y[10]
    dCare_200350 <- + parms[19] * y[7] + parms[2] * y[10] - (parms[5] + parms[3] + parms[12] + parms[18]) * y[11]
    dCare_200 <- + parms[19] * y[8] + parms[3] * y[11] - (parms[5] + parms[13] + parms[18]) * y[12]

    dTx_500 <- + parms[5] * y[9] - ((parms[7] / 2) + parms[6] + parms[10] + parms[18]) * y[13]
    dTx_350500 <- + parms[5] * y[10] + parms[8] * y[15] - ((parms[7] / 2) + parms[6] + parms[11] + parms[18]) * y[14]
    dTx_200350 <- + parms[5] * y[11] + parms[9] * y[16] - ((parms[7] / 2) + parms[8] + parms[6] + parms[12] + parms[18]) * y[15]
    dTx_200 <- + parms[5] * y[12] - ((parms[7] / 2) + parms[9] + parms[6] + parms[13] + parms[18]) * y[16]

    dVs_500 <- + parms[6] * y[13] - ((parms[7] / 2) + parms[14] + parms[18]) * y[17]
    dVs_350500 <- + parms[6] * y[14] + parms[8] * y[19] - ((parms[7] / 2) + parms[15] + parms[18]) * y[18]
    dVs_200350 <- + parms[6] * y[15] + parms[9] * y[20] - ((parms[7] / 2) + parms[8] + parms[16] + parms[18]) * y[19]
    dVs_200 <- + parms[6] * y[16] - ((parms[7] / 2) + parms[9] + parms[17] + parms[18]) * y[20]

    dLtfu_500 <- + (parms[7] / 2) * (y[13] + y[17]) - (parms[1] + parms[10] + parms[18]) * y[21]
    dLtfu_350500 <- + (parms[7] / 2) * (y[14] + y[18]) + parms[1] * y[21] - (parms[2] + parms[11] + parms[18]) * y[22]
    dLtfu_200350 <- + (parms[7] / 2) * (y[15] + y[19]) + parms[2] * y[22] - (parms[3] + parms[12] + parms[18]) * y[23]
    dLtfu_200 <- + (parms[7] / 2) * (y[16] + y[20]) + parms[3] * y[23] - (parms[13] + parms[18]) * y[24]

    dNewInf <- Beta * (((y[1] + y[5] + y[9] + y[13] + y[21]) * 1.35) + ((y[2] + y[6] + y[10] + y[14] + y[22]) * 1) + ((y[3] + y[7] + y[11] + y[15] + y[23]) * 1.64) + ((y[4] + y[8] + y[12] + y[16] + y[24]) * 5.17) + ((y[17] + y[18] + y[19] + y[20]) * 0.1))

    dHivMortality <- parms[10] * (y[1] + y[5] + y[9] + y[13] + y[21]) + parms[11] * (y[2] + y[6] + y[10] + y[14] + y[22]) + parms[12] * (y[3] + y[7] + y[11] + y[15] + y[23]) + parms[13] * (y[4] + y[8] + y[12] + y[16] + y[24]) + parms[14] * y[17] + parms[15] * y[18] + parms[16] * y[19] + parms[17] * y[20]

    dNaturalMortality <- parms[18] * (y[1] + y[2] + y[3] + y[4] + y[5] + y[6] + y[7] + y[8] + y[9] + y[10] + y[11] + y[12] + y[13] + y[14] + y[15] + y[16] + y[17] + y[18] + y[19] + y[20] + y[21] + y[22] + y[23] + y[24])

    dDx_Cost <- (parms[4] * (y[5] + y[6] + y[7] + y[8])) * parms[20]
    dCare_Cost <- (parms[19] * (y[9] + y[10] + y[11] + y[12])) * parms[21]
    dTx_Cost <- (parms[5] * (y[13] + y[14] + y[15] + y[16])) * parms[22]
    dRetention_Cost <- ((parms[7] / 2) * (y[13] + y[14] + y[15] + y[16] + y[17] + y[18] + y[19] + y[20])) * parms[23]
    
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
        dTx_Cost,
        dRetention_Cost))
}