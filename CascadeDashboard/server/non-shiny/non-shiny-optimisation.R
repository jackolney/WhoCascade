RunNSOptimisation <- function() {
    # This should be triggered by the renderPlot().

    # CHECKLIST
    print("OptInput$intValue_rho =")
    print(OptInput$intValue_rho)

    print("OptInput$intValue_q =")
    print(OptInput$intValue_q)

    print("OptInput$intValue_kappa =")
    print(OptInput$intValue_kappa)

    print("OptInput$intValue_gamma =")
    print(OptInput$intValue_gamma)

    print("OptInput$intValue_sigma =")
    print(OptInput$intValue_sigma)

    print("OptInput$intValue_omega =")
    print(OptInput$intValue_omega)

    message("Starting optimisation...")

    par <- GetParaMatrix(cParamOut = CalibParamOut, minErrorRun = minErrorRun, length = 4)

    # Simulation Loop
    time <- proc.time()[[1]]

    theList <- list()

    # Extract the initial values of the 'best' fit from calibration
    bestCalibInitial <<- GetBestCalibOut(calibOut = CalibOut, minErrorRun = minErrorRun)

    ## THE BIG LOOP ##
    for (i in 1:dim(par)[1]) {

        message(paste('Run', i))

        p <- GetOptPar(
            masterCD4 = MasterCD4_2015,
            data = MasterData,
            iterationParam = par[i,],
            calibParamOut = CalibParamOut,
            minErrorRun = minErrorRun)

        # Now we need the initials.
        y <- GetInitial(
            p = p,
            iterationResult = bestCalibInitial,
            masterCD4 = MasterCD4_2015)

        p[["beta"]] <- GetBeta(y = y, p = p, iterationInc = CalibIncOut[minErrorRun,])

        theList[[rownames(par)[i]]] <- RunSim(y = y, p = p)
    }
    ## END OF LOOP ##

    message('Compiling results')

    # Dealing with the results #
    # I think we need to pass the average values to BaselineModel() not let it pick from the other crap.
    # Although, may need to reverse on this strategy tomorrow.
    # Need a sensible answer first.
    # BaseModel <- BaselineModel()
    BaseModel <- CallBaselineModel()
    BaseDALY  <- Calc_DALY(BaseModel)
    BaseCost  <- Calc_Cost(BaseModel)

    print(paste("BaseDALY =", scales::comma(BaseDALY)))
    print(paste("BaseCost =", scales::comma(BaseCost)))

    Result90        <- c()
    Result9090      <- c()
    Result909090    <- c()
    ResultVS        <- c()
    ResultImpact    <- c()
    ResultCost      <- c()
    ResultPar_Rho   <- c()
    ResultPar_Q     <- c()
    ResultPar_Kappa <- c()
    ResultPar_Gamma <- c()
    ResultPar_Sigma <- c()
    ResultPar_Omega <- c()
    for (i in 1:length(theList)) {
        message(paste0(round((i / length(theList) * 100), digits = 0), "%"))
        Result90[i]        <- Calc_909090_Result(theList[[i]])[1]
        Result9090[i]      <- Calc_909090_Result(theList[[i]])[2]
        Result909090[i]    <- Calc_909090_Result(theList[[i]])[3]
        ResultVS[i]        <- Calc_VS(theList[[i]])
        ResultImpact[i]    <- Calc_DALYsAverted(theList[[i]], BaseDALY)
        ResultCost[i]      <- Calc_AdditionalCost(theList[[i]], BaseCost)
        ResultPar_Rho[i]   <- par[i,"Rho"]
        ResultPar_Q[i]     <- par[i,"Q"]
        ResultPar_Kappa[i] <- par[i,"Kappa"]
        ResultPar_Gamma[i] <- par[i,"Gamma"]
        ResultPar_Sigma[i] <- par[i,"Sigma"]
        ResultPar_Omega[i] <- par[i,"Omega"]
    }

    # Result data.frame for plot(vs,cost)
    Result_909090 <<- data.frame(Result90, Result9090, Result909090, ResultVS, ResultCost, ResultPar_Rho, ResultPar_Q, ResultPar_Kappa, ResultPar_Gamma, ResultPar_Sigma, ResultPar_Omega)
    colnames(Result_909090) <<- c("90", "90-90", "90-90-90", "VS", "Cost", "Rho", "Q", "Kappa", "Gamma", "Sigma", "Omega")

    # Result data.frame for plot(DALYs,cost)
    Result_DALYs <<- data.frame(ResultImpact, ResultCost, ResultPar_Rho, ResultPar_Q, ResultPar_Kappa, ResultPar_Gamma, ResultPar_Sigma, ResultPar_Omega)
    colnames(Result_DALYs) <<- c("DALYs", "Cost", "Rho", "Q", "Kappa", "Gamma", "Sigma", "Omega")

    # ----------------------------------------- #
    # Subsetting those achieving 90-90-90 Stuff #
    # ----------------------------------------- #

    theList_909090 <- FindResults_909090(theList)

    ResultPar_909090 <- FindPar_909090(theList, par)

    Result909090Impact    <- c()
    Result909090Cost      <- c()
    Result909090Par_Rho   <- c()
    Result909090Par_Q     <- c()
    Result909090Par_Kappa <- c()
    Result909090Par_Gamma <- c()
    Result909090Par_Sigma <- c()
    Result909090Par_Omega <- c()
    if (length(theList_909090) > 0) {
        for (i in 1:length(theList_909090)) {
            message(paste0(round((i / length(theList_909090) * 100), digits = 0), "%"))
            Result909090Impact[i]    <- Calc_DALYsAverted(theList_909090[[i]], BaseDALY)
            Result909090Cost[i]      <- Calc_AdditionalCost(theList_909090[[i]], BaseCost)
            Result909090Par_Rho[i]   <- par[i,"rho"]
            Result909090Par_Q[i]     <- par[i,"q"]
            Result909090Par_Kappa[i] <- par[i,"kappa"]
            Result909090Par_Gamma[i] <- par[i,"gamma"]
            Result909090Par_Sigma[i] <- par[i,"sigma"]
            Result909090Par_Omega[i] <- par[i,"omega"]
        }

        Result_DALYs_909090 <<- data.frame(Result909090Impact, Result909090Cost, Result909090Par_Rho, Result909090Par_Q, Result909090Par_Kappa, Result909090Par_Gamma, Result909090Par_Sigma, Result909090Par_Omega)
        colnames(Result_DALYs_909090) <<- c("DALYs", "Cost", "Rho", "Q", "Kappa", "Gamma", "Sigma", "Omega")
    } else {
        Result_DALYs_909090 <<- data.frame(0, 0, 0, 0, 0, 0, 0, 0)
        colnames(Result_DALYs_909090) <<- c("DALYs", "Cost", "Rho", "Q", "Kappa", "Gamma", "Sigma", "Omega")
    }

    message("finished")
    Result_909090
}