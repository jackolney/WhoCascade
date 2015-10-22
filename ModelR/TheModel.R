ComplexCascade <- function(t, y, parms) {

    dUnDx_500 <- 0.5251 * (Beta * (((y[["UnDx_500"]] + y[["Dx_500"]] + y[["Care_500"]] + y[["PreLtfu_500"]] + y[["Tx_Na_500"]] + y[["Ltfu_500"]]) * 1.35) + ((y[["UnDx_350500"]] + y[["Dx_350500"]] + y[["Care_350500"]] + y[["PreLtfu_350500"]] + y[["Tx_Na_350500"]] + y[["Ltfu_350500"]]) * 1) + ((y[["UnDx_200350"]] + y[["Dx_200350"]] + y[["Care_200350"]] + y[["PreLtfu_200350"]] + y[["Tx_Na_200350"]] + y[["Ltfu_200350"]]) * 1.64) + ((y[["UnDx_200"]] + y[["Dx_200"]] + y[["Care_200"]] + y[["PreLtfu_200"]] + y[["Tx_Na_200"]] + y[["Ltfu_200"]]) * 5.17) + ((y[["Tx_A_500"]] + y[["Tx_A_350500"]] + y[["Tx_A_200350"]] + y[["Tx_A_200"]]) * 0.1))) - (parms[["Nu_1"]] + parms[["Rho"]] + (parms[["s_1"]] * parms[["p"]] * parms[["Theta"]]) + (parms[["s_1"]] * (1-parms[["p"]]) * parms[["Theta"]]) + parms[["Alpha_1"]] + parms[["Mu"]]) * y[["UnDx_500"]]
    dUnDx_350500 <- 0.2315 * (Beta * (((y[["UnDx_500"]] + y[["Dx_500"]] + y[["Care_500"]] + y[["PreLtfu_500"]] + y[["Tx_Na_500"]] + y[["Ltfu_500"]]) * 1.35) + ((y[["UnDx_350500"]] + y[["Dx_350500"]] + y[["Care_350500"]] + y[["PreLtfu_350500"]] + y[["Tx_Na_350500"]] + y[["Ltfu_350500"]]) * 1) + ((y[["UnDx_200350"]] + y[["Dx_200350"]] + y[["Care_200350"]] + y[["PreLtfu_200350"]] + y[["Tx_Na_200350"]] + y[["Ltfu_200350"]]) * 1.64) + ((y[["UnDx_200"]] + y[["Dx_200"]] + y[["Care_200"]] + y[["PreLtfu_200"]] + y[["Tx_Na_200"]] + y[["Ltfu_200"]]) * 5.17) + ((y[["Tx_A_500"]] + y[["Tx_A_350500"]] + y[["Tx_A_200350"]] + y[["Tx_A_200"]]) * 0.1))) + parms[["Nu_1"]] * y[["UnDx_500"]] - (parms[["Nu_2"]] + parms[["Rho"]] + (parms[["s_2"]] * parms[["p"]] * parms[["Theta"]]) + (parms[["s_2"]] * (1-parms[["p"]]) * parms[["Theta"]]) + parms[["Alpha_2"]] + parms[["Mu"]]) * y[["UnDx_350500"]]
    dUnDx_200350 <- 0.2401 * (Beta * (((y[["UnDx_500"]] + y[["Dx_500"]] + y[["Care_500"]] + y[["PreLtfu_500"]] + y[["Tx_Na_500"]] + y[["Ltfu_500"]]) * 1.35) + ((y[["UnDx_350500"]] + y[["Dx_350500"]] + y[["Care_350500"]] + y[["PreLtfu_350500"]] + y[["Tx_Na_350500"]] + y[["Ltfu_350500"]]) * 1) + ((y[["UnDx_200350"]] + y[["Dx_200350"]] + y[["Care_200350"]] + y[["PreLtfu_200350"]] + y[["Tx_Na_200350"]] + y[["Ltfu_200350"]]) * 1.64) + ((y[["UnDx_200"]] + y[["Dx_200"]] + y[["Care_200"]] + y[["PreLtfu_200"]] + y[["Tx_Na_200"]] + y[["Ltfu_200"]]) * 5.17) + ((y[["Tx_A_500"]] + y[["Tx_A_350500"]] + y[["Tx_A_200350"]] + y[["Tx_A_200"]]) * 0.1))) + parms[["Nu_2"]] * y[["UnDx_350500"]] - (parms[["Nu_3"]] + parms[["Rho"]] + (parms[["s_3"]] * parms[["p"]] * parms[["Theta"]]) + (parms[["s_3"]] * (1-parms[["p"]]) * parms[["Theta"]]) + parms[["Alpha_3"]] + parms[["Mu"]]) * y[["UnDx_200350"]]
    dUnDx_200 <- 0.0033 * (Beta * (((y[["UnDx_500"]] + y[["Dx_500"]] + y[["Care_500"]] + y[["PreLtfu_500"]] + y[["Tx_Na_500"]] + y[["Ltfu_500"]]) * 1.35) + ((y[["UnDx_350500"]] + y[["Dx_350500"]] + y[["Care_350500"]] + y[["PreLtfu_350500"]] + y[["Tx_Na_350500"]] + y[["Ltfu_350500"]]) * 1) + ((y[["UnDx_200350"]] + y[["Dx_200350"]] + y[["Care_200350"]] + y[["PreLtfu_200350"]] + y[["Tx_Na_200350"]] + y[["Ltfu_200350"]]) * 1.64) + ((y[["UnDx_200"]] + y[["Dx_200"]] + y[["Care_200"]] + y[["PreLtfu_200"]] + y[["Tx_Na_200"]] + y[["Ltfu_200"]]) * 5.17) + ((y[["Tx_A_500"]] + y[["Tx_A_350500"]] + y[["Tx_A_200350"]] + y[["Tx_A_200"]]) * 0.1))) + parms[["Nu_3"]] * y[["UnDx_200350"]] - (parms[["Rho"]] + (parms[["s_4"]] * parms[["p"]] * parms[["Theta"]]) + (parms[["s_4"]] * (1-parms[["p"]]) * parms[["Theta"]]) + parms[["Alpha_4"]] + parms[["Mu"]]) * y[["UnDx_200"]]

    dDx_500 <- + parms[["Rho"]] * y[["UnDx_500"]] - (parms[["Nu_1"]] + parms[["Epsilon"]] + (parms[["s_1"]] * parms[["p"]] * parms[["Theta"]]) + (parms[["s_1"]] * (1-parms[["p"]]) * parms[["Theta"]]) + parms[["Alpha_1"]] + parms[["Mu"]]) * y[["Dx_500"]]
    dDx_350500 <- + parms[["Rho"]] * y[["UnDx_350500"]] + parms[["Nu_1"]] * y[["Dx_500"]] - (parms[["Nu_2"]] + parms[["Epsilon"]] + (parms[["s_2"]] * parms[["p"]] * parms[["Theta"]]) + (parms[["s_2"]] * (1-parms[["p"]]) * parms[["Theta"]]) + parms[["Alpha_2"]] + parms[["Mu"]]) * y[["Dx_350500"]]
    dDx_200350 <- + parms[["Rho"]] * y[["UnDx_200350"]] + parms[["Nu_2"]] * y[["Dx_350500"]] - (parms[["Nu_3"]] + parms[["Epsilon"]] + (parms[["s_3"]] * parms[["p"]] * parms[["Theta"]]) + (parms[["s_3"]] * (1-parms[["p"]]) * parms[["Theta"]]) + parms[["Alpha_3"]] + parms[["Mu"]]) * y[["Dx_200350"]]
    dDx_200 <- + parms[["Rho"]] * y[["UnDx_200"]] + parms[["Nu_3"]] * y[["Dx_200350"]] - (parms[["Epsilon"]] + (parms[["s_4"]] * parms[["p"]] * parms[["Theta"]]) + (parms[["s_4"]] * (1-parms[["p"]]) * parms[["Theta"]]) + parms[["Alpha_4"]] + parms[["Mu"]]) * y[["Dx_200"]]

    dCare_500 <- + parms[["Epsilon"]] * y[["Dx_500"]] - (parms[["Nu_1"]] + parms[["Kappa"]] + (parms[["s_1"]] * parms[["p"]] * parms[["Gamma"]]) + (parms[["s_1"]] * (1-parms[["p"]]) * parms[["Gamma"]]) + parms[["Alpha_1"]] + parms[["Mu"]]) * y[["Care_500"]]
    dCare_350500 <- + parms[["Epsilon"]] * y[["Dx_350500"]] + parms[["Nu_1"]] * y[["Care_500"]] - (parms[["Nu_2"]] + parms[["Kappa"]] + (parms[["s_2"]] * parms[["p"]] * parms[["Gamma"]]) + (parms[["s_2"]] * (1-parms[["p"]]) * parms[["Gamma"]]) + parms[["Alpha_2"]] + parms[["Mu"]]) * y[["Care_350500"]]
    dCare_200350 <- + parms[["Epsilon"]] * y[["Dx_200350"]] + parms[["Nu_2"]] * y[["Care_350500"]] - (parms[["Nu_3"]] + parms[["Kappa"]] + (parms[["s_3"]] * parms[["p"]] * parms[["Gamma"]]) + (parms[["s_3"]] * (1-parms[["p"]]) * parms[["Gamma"]]) + parms[["Alpha_3"]] + parms[["Mu"]]) * y[["Care_200350"]]
    dCare_200 <- + parms[["Epsilon"]] * y[["Dx_200"]] + parms[["Nu_3"]] * y[["Care_200350"]] - (parms[["Kappa"]] + (parms[["s_4"]] * parms[["p"]] * parms[["Gamma"]]) + (parms[["s_4"]] * (1-parms[["p"]]) * parms[["Gamma"]]) + parms[["Alpha_4"]] + parms[["Mu"]]) * y[["Care_200"]]

    dPreLtfu_500 <- + parms[["Kappa"]] * y[["Care_500"]] - (parms[["Nu_1"]] + (parms[["s_1"]] * parms[["p"]] * parms[["Theta"]]) + (parms[["s_1"]] * (1-parms[["p"]]) * parms[["Theta"]]) + parms[["Alpha_1"]] + parms[["Mu"]]) * y[["PreLtfu_500"]]
    dPreLtfu_350500 <- + parms[["Kappa"]] * y[["Care_350500"]] + parms[["Nu_1"]] * y[["PreLtfu_500"]] - (parms[["Nu_2"]] + (parms[["s_2"]] * parms[["p"]] * parms[["Theta"]]) + (parms[["s_2"]] * (1-parms[["p"]]) * parms[["Theta"]]) + parms[["Alpha_2"]] + parms[["Mu"]]) * y[["PreLtfu_350500"]]
    dPreLtfu_200350 <- + parms[["Kappa"]] * y[["Care_200350"]] + parms[["Nu_2"]] * y[["PreLtfu_350500"]] - (parms[["Nu_3"]] + (parms[["s_3"]] * parms[["p"]] * parms[["Theta"]]) + (parms[["s_3"]] * (1-parms[["p"]]) * parms[["Theta"]]) + parms[["Alpha_3"]] + parms[["Mu"]]) * y[["PreLtfu_200350"]]
    dPreLtfu_200 <- + parms[["Kappa"]] * y[["Care_200"]] + parms[["Nu_3"]] * y[["PreLtfu_200350"]] - ((parms[["s_4"]] * parms[["p"]] * parms[["Theta"]]) + (parms[["s_4"]] * (1-parms[["p"]]) * parms[["Theta"]]) + parms[["Alpha_4"]] + parms[["Mu"]]) * y[["PreLtfu_200"]]

    dTx_Na_500 <- + (parms[["s_1"]] * (1-parms[["p"]]) * parms[["Theta"]]) * y[["PreLtfu_500"]] + (parms[["s_1"]] * (1-parms[["p"]]) * parms[["Gamma"]]) * y[["Care_500"]] + (parms[["s_1"]] * (1-parms[["p"]]) * parms[["Theta"]]) * y[["Dx_500"]] + (parms[["s_1"]] * (1-parms[["p"]]) * parms[["Theta"]]) * y[["UnDx_500"]] - (parms[["Nu_1"]] + parms[["Omega"]] + parms[["Sigma"]] + parms[["Alpha_1"]] + parms[["Mu"]]) * y[["Tx_Na_500"]]
    dTx_Na_350500 <- + (parms[["s_2"]] * (1-parms[["p"]]) * parms[["Theta"]]) * y[["PreLtfu_350500"]] + (parms[["s_2"]] * (1-parms[["p"]]) * parms[["Gamma"]]) * y[["Care_350500"]] + (parms[["s_2"]] * (1-parms[["p"]]) * parms[["Theta"]]) * y[["Dx_350500"]] + (parms[["s_2"]] * (1-parms[["p"]]) * parms[["Theta"]]) * y[["UnDx_350500"]] + parms[["Nu_1"]] * y[["Tx_Na_500"]] - (parms[["Nu_2"]] + parms[["Omega"]] + parms[["Sigma"]] + parms[["Alpha_2"]] + parms[["Mu"]]) * y[["Tx_Na_350500"]]
    dTx_Na_200350 <- + (parms[["s_3"]] * (1-parms[["p"]]) * parms[["Theta"]]) * y[["PreLtfu_200350"]] + (parms[["s_3"]] * (1-parms[["p"]]) * parms[["Gamma"]]) * y[["Care_200350"]] + (parms[["s_3"]] * (1-parms[["p"]]) * parms[["Theta"]]) * y[["Dx_200350"]] + (parms[["s_3"]] * (1-parms[["p"]]) * parms[["Theta"]]) * y[["UnDx_200350"]] + parms[["Nu_2"]] * y[["Tx_Na_350500"]] - (parms[["Nu_3"]] + parms[["Omega"]] + parms[["Sigma"]] + parms[["Alpha_3"]] + parms[["Mu"]]) * y[["Tx_Na_200350"]]
    dTx_Na_200 <- + (parms[["s_4"]] * (1-parms[["p"]]) * parms[["Theta"]]) * y[["PreLtfu_200"]] + (parms[["s_4"]] * (1-parms[["p"]]) * parms[["Gamma"]]) * y[["Care_200"]] + (parms[["s_4"]] * (1-parms[["p"]]) * parms[["Theta"]]) * y[["Dx_200"]] + (parms[["s_4"]] * (1-parms[["p"]]) * parms[["Theta"]]) * y[["UnDx_200"]] + parms[["Nu_3"]] * y[["Tx_Na_200350"]] - (parms[["Omega"]] + parms[["Sigma"]] + parms[["Alpha_4"]] + parms[["Mu"]]) * y[["Tx_Na_200"]]

    dTx_A_500 <- + parms[["Sigma"]] * y[["Tx_Na_500"]] + (parms[["s_1"]] * parms[["p"]] * parms[["Theta"]]) * y[["PreLtfu_500"]] + (parms[["s_1"]] * parms[["p"]] * parms[["Gamma"]]) * y[["Care_500"]] + (parms[["s_1"]] * parms[["p"]] * parms[["Theta"]]) * y[["Dx_500"]] + (parms[["s_1"]] * parms[["p"]] * parms[["Theta"]]) * y[["UnDx_500"]] - (parms[["Omega"]] + parms[["Tau_1"]] + parms[["Mu"]]) * y[["Tx_A_500"]]
    dTx_A_350500 <- + parms[["Sigma"]] * y[["Tx_Na_350500"]] + (parms[["s_2"]] * parms[["p"]] * parms[["Theta"]]) * y[["PreLtfu_350500"]] + (parms[["s_2"]] * parms[["p"]] * parms[["Gamma"]]) * y[["Care_350500"]] + (parms[["s_2"]] * parms[["p"]] * parms[["Theta"]]) * y[["Dx_350500"]] + (parms[["s_2"]] * parms[["p"]] * parms[["Theta"]]) * y[["UnDx_350500"]] + parms[["Delta_1"]] * y[["Tx_A_200350"]] - (parms[["Omega"]] + parms[["Tau_2"]] + parms[["Mu"]]) * y[["Tx_A_350500"]]
    dTx_A_200350 <- + parms[["Sigma"]] * y[["Tx_Na_200350"]] + (parms[["s_3"]] * parms[["p"]] * parms[["Theta"]]) * y[["PreLtfu_200350"]] + (parms[["s_3"]] * parms[["p"]] * parms[["Gamma"]]) * y[["Care_200350"]] + (parms[["s_3"]] * parms[["p"]] * parms[["Theta"]]) * y[["Dx_200350"]] + (parms[["s_3"]] * parms[["p"]] * parms[["Theta"]]) * y[["UnDx_200350"]] + parms[["Delta_2"]] * y[["Tx_A_200"]] - (parms[["Delta_1"]] + parms[["Omega"]] + parms[["Tau_3"]] + parms[["Mu"]]) * y[["Tx_A_200350"]]
    dTx_A_200 <- + parms[["Sigma"]] * y[["Tx_Na_200"]] + (parms[["s_4"]] * parms[["p"]] * parms[["Theta"]]) * y[["PreLtfu_200"]] + (parms[["s_4"]] * parms[["p"]] * parms[["Gamma"]]) * y[["Care_200"]] + (parms[["s_4"]] * parms[["p"]] * parms[["Theta"]]) * y[["Dx_200"]] + (parms[["s_4"]] * parms[["p"]] * parms[["Theta"]]) * y[["UnDx_200"]] - (parms[["Delta_2"]] + parms[["Omega"]] + parms[["Tau_4"]] + parms[["Mu"]]) * y[["Tx_A_200"]]

    dLtfu_500 <- + parms[["Omega"]] * (y[["Tx_Na_500"]] + y[["Tx_A_500"]]) - (parms[["Nu_1"]] + parms[["Alpha_1"]] + parms[["Mu"]]) * y[["Ltfu_500"]]
    dLtfu_350500 <- + parms[["Omega"]] * (y[["Tx_Na_350500"]] + y[["Tx_A_350500"]]) + parms[["Nu_1"]] * y[["Ltfu_500"]] - (parms[["Nu_2"]] + parms[["Alpha_2"]] + parms[["Mu"]]) * y[["Ltfu_350500"]]
    dLtfu_200350 <- + parms[["Omega"]] * (y[["Tx_Na_200350"]] + y[["Tx_A_200350"]]) + parms[["Nu_2"]] * y[["Ltfu_350500"]] - (parms[["Nu_3"]] + parms[["Alpha_3"]] + parms[["Mu"]]) * y[["Ltfu_200350"]]
    dLtfu_200 <- + parms[["Omega"]] * (y[["Tx_Na_200"]] + y[["Tx_A_200"]]) + parms[["Nu_3"]] * y[["Ltfu_200350"]] - (parms[["Alpha_4"]] + parms[["Mu"]]) * y[["Ltfu_200"]]


    dNewInf <- Beta * (((y[["UnDx_500"]] + y[["Dx_500"]] + y[["Care_500"]] + y[["PreLtfu_500"]] + y[["Tx_Na_500"]] + y[["Ltfu_500"]]) * 1.35) + ((y[["UnDx_350500"]] + y[["Dx_350500"]] + y[["Care_350500"]] + y[["PreLtfu_350500"]] + y[["Tx_Na_350500"]] + y[["Ltfu_350500"]]) * 1) + ((y[["UnDx_200350"]] + y[["Dx_200350"]] + y[["Care_200350"]] + y[["PreLtfu_200350"]] + y[["Tx_Na_200350"]] + y[["Ltfu_200350"]]) * 1.64) + ((y[["UnDx_200"]] + y[["Dx_200"]] + y[["Care_200"]] + y[["PreLtfu_200"]] + y[["Tx_Na_200"]] + y[["Ltfu_200"]]) * 5.17) + ((y[["Tx_A_500"]] + y[["Tx_A_350500"]] + y[["Tx_A_200350"]] + y[["Tx_A_200"]]) * 0.1))
    dHivMortality <- parms[["Alpha_1"]] * (y[["UnDx_500"]] + y[["Dx_500"]] + y[["Care_500"]] + y[["PreLtfu_500"]] + y[["Tx_Na_500"]]) + parms[["Alpha_2"]] * (y[["UnDx_350500"]] + y[["Dx_350500"]] + y[["Care_350500"]] + y[["PreLtfu_350500"]] + y[["Tx_Na_350500"]]) + parms[["Alpha_3"]] * (y[["UnDx_200350"]] + y[["Dx_200350"]] + y[["Care_200350"]] + y[["PreLtfu_200350"]] + y[["Tx_Na_200350"]]) + parms[["Alpha_4"]] * (y[["UnDx_200"]] + y[["Dx_200"]] + y[["Care_200"]] + y[["PreLtfu_200"]] + y[["Tx_Na_200"]]) + parms[["Tau_1"]] * y[["Tx_A_500"]] + parms[["Tau_2"]] * y[["Tx_A_350500"]] + parms[["Tau_3"]] * y[["Tx_A_200350"]] + parms[["Tau_4"]] * y[["Tx_A_200"]]
    dNaturalMortality <- parms[["Mu"]] * (y[["UnDx_500"]] + y[["UnDx_350500"]] + y[["UnDx_200350"]] + y[["UnDx_200"]] + y[["Dx_500"]] + y[["Dx_350500"]] + y[["Dx_200350"]] + y[["Dx_200"]] + y[["Care_500"]] + y[["Care_350500"]] + y[["Care_200350"]] + y[["Care_200"]] + y[["PreLtfu_500"]] + y[["PreLtfu_350500"]] + y[["PreLtfu_200350"]] + y[["PreLtfu_200"]] + y[["Tx_Na_500"]] + y[["Tx_Na_350500"]] + y[["Tx_Na_200350"]] + y[["Tx_Na_200"]] + y[["Tx_A_500"]] + y[["Tx_A_350500"]] + y[["Tx_A_200350"]] + y[["Tx_A_200"]] + y[["Ltfu_500"]] + y[["Ltfu_350500"]] + y[["Ltfu_200350"]] + y[["Ltfu_200"]])

    # Transition Costs
    dDx_Cost <- (parms[["Rho"]] * (y[["UnDx_500"]] + y[["UnDx_350500"]] + y[["UnDx_200350"]] + y[["UnDx_200"]])) * parms[["Dx_unitCost"]]
    dCare_Cost <- (parms[["Epsilon"]] * (y[["Dx_500"]] + y[["Dx_350500"]] + y[["Dx_200350"]] + y[["Dx_200"]])) * parms[["Care_unitCost"]]
    dTxInit_Cost <- (((parms[["p"]] * parms[["Theta"]]) * (y[["UnDx_500"]] + y[["UnDx_350500"]] + y[["UnDx_200350"]] + y[["UnDx_200"]])) + (((1-parms[["p"]]) * parms[["Theta"]]) * (y[["UnDx_500"]] + y[["UnDx_350500"]] + y[["UnDx_200350"]] + y[["UnDx_200"]])) + ((parms[["p"]] * parms[["Theta"]]) * (y[["Dx_500"]] + y[["Dx_350500"]] + y[["Dx_200350"]] + y[["Dx_200"]])) + (((1-parms[["p"]]) * parms[["Theta"]]) * (y[["Dx_500"]] + y[["Dx_350500"]] + y[["Dx_200350"]] + y[["Dx_200"]])) + ((parms[["p"]] * parms[["Gamma"]]) * (y[["Care_500"]] + y[["Care_350500"]] + y[["Care_200350"]] + y[["Care_200"]])) + (((1-parms[["p"]]) * parms[["Gamma"]]) * (y[["Care_500"]] + y[["Care_350500"]] + y[["Care_200350"]] + y[["Care_200"]])) + ((parms[["p"]] * parms[["Theta"]]) * (y[["PreLtfu_500"]] + y[["PreLtfu_350500"]] + y[["PreLtfu_200350"]] + y[["PreLtfu_200"]])) + (((1-parms[["p"]]) * parms[["Theta"]]) * (y[["PreLtfu_500"]] + y[["PreLtfu_350500"]] + y[["PreLtfu_200350"]] + y[["PreLtfu_200"]]))) * parms[["TxInit_unitCost"]]
    
    dRetention_Cost <- (parms[["Omega"]] * (y[["Tx_Na_500"]] + y[["Tx_Na_350500"]] + y[["Tx_Na_200350"]] + y[["Tx_Na_200"]] + y[["Tx_A_500"]] + y[["Tx_A_350500"]] + y[["Tx_A_200350"]] + y[["Tx_A_200"]])) * parms[["Retention_unitCost"]]

    # Annual Costs
    dAnnualTxCost <- (y[["Tx_Na_500"]] + y[["Tx_Na_350500"]] + y[["Tx_Na_200350"]] + y[["Tx_Na_200"]] + y[["Tx_A_500"]] + y[["Tx_A_350500"]] + y[["Tx_A_200350"]] + y[["Tx_A_200"]]) * parms[["AnnualTx_unitCost"]]
    
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
        dTx_Na_500,
        dTx_Na_350500,
        dTx_Na_200350,
        dTx_Na_200,
        dTx_A_500,
        dTx_A_350500,
        dTx_A_200350,
        dTx_A_200,
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