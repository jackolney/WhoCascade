GetParaMatrix <- function() {
    ParRange <- expand.grid(
        rho =       seq(from = input$userOptRho_Range[1],       to = input$userOptRho_Range[2],     length.out = input$userOptRho_LengthOf),
        q =         seq(from = input$userOptq_Range[1],         to = input$userOptq_Range[2],       length.out = input$userOptq_LengthOf),
        kappa =     seq(from = input$userOptKappa_Range[2],     to = input$userOptKappa_Range[1],   length.out = input$userOptKappa_LengthOf),
        gamma =     seq(from = input$userOptGamma_Range[1],     to = input$userOptGamma_Range[2],   length.out = input$userOptGamma_LengthOf),
        sigma =     seq(from = input$userOptSigma_Range[1],     to = input$userOptSigma_Range[2],   length.out = input$userOptSigma_LengthOf),
        omega =     seq(from = input$userOptOmega_Range[2],     to = input$userOptOmega_Range[1],   length.out = input$userOptOmega_LengthOf)
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
    Calc_DALY(CallModel())
}

Calc_BaselineCost <- function() {
    Calc_Cost(CallModel())
}

Calc_DALYsAverted <- function(out, base_DALY) {
    return(base_DALY - Calc_DALY(out))
}

Calc_AdditionalCost <- function(out, base_COST) {
    return(out$TotalCost[251] - base_COST)
}

Calc_909090_Result <- function(out) {
    Extract909090Data(out)$res
}

Calc_VS <- function(out) {
    out$Vs[251]
}

Calc_909090 <- function(out) {
    Extract909090Data(out)
}

FindResults_909090 <- function(result) {
    res_list <- list()
    for(i in 1:length(result)) {
        print(i)
        t_909090 <- Calc_909090(result[[i]])
        test <- c(0,0,0)
        for(j in 1:length(t_909090$res)) {
            if(t_909090$res[j] > 0.9) {
                test[j] <- 1
            } else {
                test[j] <- 0
            }
        }
        if(sum(test) == 3) {
            res_list[[length(res_list) + 1]] <- result[[i]]
        }
    }
    return(res_list)
}

FindPar_909090 <- function(result, par) {
    res_list <- list()
    for(i in 1:length(result)) {
        print(i)
        t_909090 <- Calc_909090(result[[i]])
        Test <- c(0,0,0)
        for(j in 1:length(t_909090$res)) {
            if(t_909090$res[j] > 0.9) {
                Test[j] <- 1
            } else {
                Test[j] <- 0
            }
        }
        if(sum(Test) == 3) {
            res_list[[length(res_list) + 1]] <- par[i,]
        }
    }
    return(res_list)
}

GetAverageCalibOut <- function(calibOut) {
    # subset only model output for 2015
    out <- calibOut[calibOut$year == 2015 & calibOut$source == "model",]
    # find only unique values
    indicator <- unique(out$indicator)
    # pick out values
    value <- c()
    for (j in 1:length(indicator)) {
        value[j] <- mean(calibOut[calibOut$indicator == indicator[j], "value"])
    }
    # build data.frame and return
    df <- data.frame(indicator, value)
    df
}
