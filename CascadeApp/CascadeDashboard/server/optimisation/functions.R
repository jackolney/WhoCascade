GetParaMatrix <- function() {
    ParRange <- expand.grid(
        Rho =       seq(from = input$userOptRho_Range[1],       to = input$userOptRho_Range[2],     length.out = input$userOptRho_LengthOf),
        Epsilon =   seq(from = input$userOptEpsilon_Range[1],   to = input$userOptEpsilon_Range[2], length.out = input$userOptEpsilon_LengthOf),
        Kappa =     seq(from = input$userOptKappa_Range[2],     to = input$userOptKappa_Range[1],   length.out = input$userOptKappa_LengthOf),
        Gamma =     seq(from = input$userOptGamma_Range[1],     to = input$userOptGamma_Range[2],   length.out = input$userOptGamma_LengthOf),
        Sigma =     seq(from = input$userOptSigma_Range[1],     to = input$userOptSigma_Range[2],   length.out = input$userOptSigma_LengthOf),
        Omega =     seq(from = input$userOptOmega_Range[2],     to = input$userOptOmega_Range[1],   length.out = input$userOptOmega_LengthOf)
    )
    ParRange
}

Calc_Cost <- function(out) {
    out$TotalCost[251]
}

Calc_DALY <- function(out) {
    sum(out$DALY)
}

Calc_BaselineDALY <- function() {
    base <- CallModel()
    Calc_DALY(base)
}

Calc_BaselineCost <- function() {
    base <- CallModel()
    Calc_Cost(base)
}

Calc_DALYsAverted <- function(out, base_DALY) {
    DALY <- sum(out$DALY)
    return(BaselineDALY - theDALY)
}


Calc_AdditionalCost <- function(outFile,BaselineCost) {
    theCost <- as.double(filter(outFile,time == 5) %>% select(TotalCost))
    return(theCost - BaselineCost)
}

Calc_909090_Result <- function(outFile) {
    PLHIV = as.double(sum(filter(outFile,time == 5) %>% select(N)))
    dx = as.double(sum(filter(outFile,time == 5) %>% select(c(Dx_500,Dx_350500,Dx_250350,Dx_200250,Dx_100200,Dx_50100,Dx_50,Care_500,Care_350500,Care_250350,Care_200250,Care_100200,Care_50100,Care_50,PreLtfu_500,PreLtfu_350500,PreLtfu_250350,PreLtfu_200250,PreLtfu_100200,PreLtfu_50100,PreLtfu_50,Tx_Na_500,Tx_Na_350500,Tx_Na_250350,Tx_Na_200250,Tx_Na_100200,Tx_Na_50100,Tx_Na_50,Tx_A_500,Tx_A_350500,Tx_A_250350,Tx_A_200250,Tx_A_100200,Tx_A_50100,Tx_A_50,Ltfu_500,Ltfu_350500,Ltfu_250350,Ltfu_200250,Ltfu_100200,Ltfu_50100,Ltfu_50))))
    tx = as.double(sum(filter(outFile,time == 5) %>% select(c(Tx_A_500,Tx_A_350500,Tx_A_250350,Tx_A_200250,Tx_A_100200,Tx_A_50100,Tx_A_50,Tx_Na_500,Tx_Na_350500,Tx_Na_250350,Tx_Na_200250,Tx_Na_100200,Tx_Na_50100,Tx_Na_50))))
    vs = as.double(sum(filter(outFile,time == 5) %>% select(c(Tx_A_500,Tx_A_350500,Tx_A_250350,Tx_A_200250,Tx_A_100200,Tx_A_50100,Tx_A_50))))
    p_dx <- dx / PLHIV
    p_tx <- tx / dx
    p_vs <- vs / tx
    results <- c(p_dx,p_tx,p_vs)
    definition <- c("% Diagnosed","% On Treatment","% Suppressed")
    the909090 <- data.frame(definition,results)
    return(results)
}

Calc_VS <- function(outFile) {
    return(as.double(filter(outFile,time == 5) %>% select(Vs)))
}


Calc_909090 <- function(outFile) {
    PLHIV = as.double(sum(filter(outFile,time == 5) %>% select(N)))
    dx = as.double(sum(filter(outFile,time == 5) %>% select(c(Dx_500,Dx_350500,Dx_250350,Dx_200250,Dx_100200,Dx_50100,Dx_50,Care_500,Care_350500,Care_250350,Care_200250,Care_100200,Care_50100,Care_50,PreLtfu_500,PreLtfu_350500,PreLtfu_250350,PreLtfu_200250,PreLtfu_100200,PreLtfu_50100,PreLtfu_50,Tx_Na_500,Tx_Na_350500,Tx_Na_250350,Tx_Na_200250,Tx_Na_100200,Tx_Na_50100,Tx_Na_50,Tx_A_500,Tx_A_350500,Tx_A_250350,Tx_A_200250,Tx_A_100200,Tx_A_50100,Tx_A_50,Ltfu_500,Ltfu_350500,Ltfu_250350,Ltfu_200250,Ltfu_100200,Ltfu_50100,Ltfu_50))))
    tx = as.double(sum(filter(outFile,time == 5) %>% select(c(Tx_A_500,Tx_A_350500,Tx_A_250350,Tx_A_200250,Tx_A_100200,Tx_A_50100,Tx_A_50,Tx_Na_500,Tx_Na_350500,Tx_Na_250350,Tx_Na_200250,Tx_Na_100200,Tx_Na_50100,Tx_Na_50))))
    vs = as.double(sum(filter(outFile,time == 5) %>% select(c(Tx_A_500,Tx_A_350500,Tx_A_250350,Tx_A_200250,Tx_A_100200,Tx_A_50100,Tx_A_50))))
    p_dx <- dx / PLHIV
    p_tx <- tx / dx
    p_vs <- vs / tx
    results <- c(p_dx,p_tx,p_vs)
    definition <- c("% Diagnosed","% On Treatment","% Suppressed")
    the909090 <- data.frame(definition,results)
    return(the909090)
}


FindResults_909090 <- function(ResultList) {
    theResultList <- list()
    for(i in 1:length(ResultList)) {
        print(i)
        the909090 <- Calc_909090(ResultList[[i]])
        Test <- c(0,0,0)
        for(j in 1:length(the909090$results)) {
            if(the909090$results[j] > 0.9) {
                Test[j] <- 1
            } else {
                Test[j] <- 0
            }
        }
        if(sum(Test) == 3) {
            theResultList[[length(theResultList) + 1]] <- theList[[i]]
        }
    }
    return(theResultList)
}

FindPar_909090 <- function(ResultList) {
    theResultParList <- list()
    for(i in 1:length(ResultList)) {
        print(i)
        the909090 <- Calc_909090(ResultList[[i]])
        Test <- c(0,0,0)
        for(j in 1:length(the909090$results)) {
            if(the909090$results[j] > 0.9) {
                Test[j] <- 1
            } else {
                Test[j] <- 0
            }
        }
        if(sum(Test) == 3) {
            theResultParList[[length(theResultParList) + 1]] <- ParInput[i,]
        }
    }
    return(theResultParList)
}
