GetParaMatrix <- function() {
    ParRange <- expand.grid(
        rho =       seq(from = input$userOptRho_Range[1],       to = input$userOptRho_Range[2],     length.out = input$optimParamLength),
        q =         seq(from = input$userOptq_Range[1],         to = input$userOptq_Range[2],       length.out = input$optimParamLength),
        kappa =     seq(from = input$userOptKappa_Range[2],     to = input$userOptKappa_Range[1],   length.out = input$optimParamLength),
        gamma =     seq(from = input$userOptGamma_Range[1],     to = input$userOptGamma_Range[2],   length.out = input$optimParamLength),
        sigma =     seq(from = input$userOptSigma_Range[1],     to = input$userOptSigma_Range[2],   length.out = input$optimParamLength),
        omega =     seq(from = input$userOptOmega_Range[2],     to = input$userOptOmega_Range[1],   length.out = input$optimParamLength)
    )
    ParRange
}

Calc_Cost <- function(out) {
    out$TotalCost[251]
}

Calc_DALY <- function(out) {
    sum(out$DALY)
}

BaselineModel <- function() {
    out <- CallModel()
    tmp <- array(unlist(out), c(dim(out[[1]]),length(out)), dimnames = c(dimnames(out[[1]]), NULL))
    res <- rowMeans(tmp, dims = 2)
    res <- as.data.frame(res) # Remove this at some point and just pass round a matrix (faster)
    res
}

Calc_DALYsAverted <- function(out, base_DALY) {
    return(base_DALY - Calc_DALY(out))
}

Calc_AdditionalCost <- function(out, base_COST) {
    return(out$TotalCost[251] - base_COST)
}

Calc_909090_Result <- function(out) {
    Extract909090DataSingle(out)$res
}

Calc_VS <- function(out) {
    out$Vs[251]
}

Calc_909090 <- function(out) {
    Extract909090DataSingle(out)
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

Extract909090DataSingle <- function(data) {
    year <- 251
    PLHIV <- data$N[year]

    DX <- sum(
        data$Dx_500[year],      data$Dx_350500[year],       data$Dx_250350[year],       data$Dx_200250[year],       data$Dx_100200[year],       data$Dx_50100[year],        data$Dx_50[year],
        data$Care_500[year],    data$Care_350500[year],     data$Care_250350[year],     data$Care_200250[year],     data$Care_100200[year],     data$Care_50100[year],      data$Care_50[year],
        data$PreLtfu_500[year], data$PreLtfu_350500[year],  data$PreLtfu_250350[year],  data$PreLtfu_200250[year],  data$PreLtfu_100200[year],  data$PreLtfu_50100[year],   data$PreLtfu_50[year],
        data$Tx_Na_500[year],   data$Tx_Na_350500[year],    data$Tx_Na_250350[year],    data$Tx_Na_200250[year],    data$Tx_Na_100200[year],    data$Tx_Na_50100[year],     data$Tx_Na_50[year],
        data$Tx_A_500[year],    data$Tx_A_350500[year],     data$Tx_A_250350[year],     data$Tx_A_200250[year],     data$Tx_A_100200[year],     data$Tx_A_50100[year],      data$Tx_A_50[year],
        data$Ltfu_500[year],    data$Ltfu_350500[year],     data$Ltfu_250350[year],     data$Ltfu_200250[year],     data$Ltfu_100200[year],     data$Ltfu_50100[year],      data$Ltfu_50[year]
        )

    TX <- sum(
        data$Tx_Na_500[year], data$Tx_Na_350500[year], data$Tx_Na_250350[year], data$Tx_Na_200250[year], data$Tx_Na_100200[year], data$Tx_Na_50100[year], data$Tx_Na_50[year],
        data$Tx_A_500[year],  data$Tx_A_350500[year],  data$Tx_A_250350[year],  data$Tx_A_200250[year],  data$Tx_A_100200[year],  data$Tx_A_50100[year],  data$Tx_A_50[year]
        )

    VS <- sum(data$Tx_A_500[year], data$Tx_A_350500[year], data$Tx_A_250350[year], data$Tx_A_200250[year], data$Tx_A_100200[year], data$Tx_A_50100[year], data$Tx_A_50[year])

    un_90     <- DX / PLHIV
    un_9090   <- TX / DX
    un_909090 <- VS / TX

    res <- c(un_90, un_9090, un_909090)
    def <- c("% Diagnosed","% On Treatment","% Suppressed")
    scenario <- c("Baseline")
    df <- data.frame(def, res, scenario)
    df$def <- factor(df$def, levels = c("% Diagnosed", "% On Treatment", "% Suppressed"))
    df
}
