# -------------------- #
# OPTIMISATION OBSERVE #
# -------------------- #

observeEvent(input$optimiseInput, {

    withProgress(min = 0, max = 1, {

        setProgress(value = 0, message = 'Starting optimisation', detail = 'creating parameter matrix')

        par <- GetParaMatrix()

        updateButton(session,"optFinished",
            label = " OPTIMISATION RUNNING",
            style = "warning",
            block = TRUE,
            size = "large",
            icon = icon("refresh", class = "fa-lg fa-spin", lib = "font-awesome"))

        updateButton(session,"optimiseInput",
            label = "",
            style = "primary",
            block = TRUE,
            size = "large",
            icon = icon("refresh", class = "fa-lg fa-spin", lib = "font-awesome"))

        # Simulation Loop
        time <- proc.time()[[1]]

        theList <- list()

        # Extract the mean initial values from calibration
        meanCalibInitial <- GetAverageCalibOut(CalibOut)

        ## THE BIG LOOP ##
        for(i in 1:dim(par)[1]) {

            setProgress(
                value = i/dim(par)[1],
                message = paste(paste('Run ', i, ".", sep = ''), "Time =", round(proc.time()[[1]] - time, 0), "sec"),
                detail = "")

            p <- GetOptPar(
                masterCD4 = MasterCD4_2015,
                data = MasterData,
                iterationParam = par[i,],
                calibParamOut = CalibParamOut)

            # Now we need the initials.
            y <- GetInitial(
                p = p,
                iterationResult = meanCalibInitial,
                masterCD4 = MasterCD4_2015
                )

            p[["beta"]] <- GetBeta(y = y, p = p, data = MasterData)

            theList[[rownames(par)[i]]] <- RunSim(y = y, p = p)

        }
        ## END OF LOOP ##

        setProgress(value = 0, message = 'Compiling results', detail = '')

        # Dealing with the results #

        base_DALY <- Calc_BaselineDALY()
        base_COST <- Calc_BaselineCost()
        print(paste("base_DALY =", base_DALY))
        print(paste("base_COST =", base_COST))

        Result90 <- c()
        Result9090 <- c()
        Result909090 <- c()
        ResultVS <- c()
        ResultImpact <- c()
        ResultCost <- c()
        ResultPar_Rho <- c()
        ResultPar_Epsilon <- c()
        ResultPar_Kappa <- c()
        ResultPar_Gamma <- c()
        ResultPar_Sigma <- c()
        ResultPar_Omega <- c()
        for(i in 1:length(theList)) {
            print(i)
            setProgress(value = i/length(theList), message = 'Compiling results', detail = '')
            Result90[i] <- Calc_909090_Result(theList[[i]])[1]
            Result9090[i] <- Calc_909090_Result(theList[[i]])[2]
            Result909090[i] <- Calc_909090_Result(theList[[i]])[3]
            ResultVS[i] <- Calc_VS(theList[[i]])
            ResultImpact[i] <- Calc_DALYsAverted(theList[[i]], base_DALY)
            ResultCost[i] <- Calc_AdditionalCost(theList[[i]], base_COST)
            ResultPar_Rho[i] <- par[i,]$Rho
            ResultPar_Epsilon[i] <- par[i,]$Epsilon
            ResultPar_Kappa[i] <- par[i,]$Kappa
            ResultPar_Gamma[i] <- par[i,]$Gamma
            ResultPar_Sigma[i] <- par[i,]$Sigma
            ResultPar_Omega[i] <- par[i,]$Omega
        }

        # Result data.frame for plot(vs,cost)
        Result_909090 <<- data.frame(Result90, Result9090, Result909090, ResultVS, ResultCost, ResultPar_Rho, ResultPar_Epsilon, ResultPar_Kappa, ResultPar_Gamma, ResultPar_Sigma, ResultPar_Omega)
        colnames(Result_909090) <<- c("90", "90-90", "90-90-90", "VS", "Cost", "Rho", "Epsilon", "Kappa", "Gamma", "Sigma", "Omega")

        # Result data.frame for plot(DALYs,cost)
        Result_DALYs <<- data.frame(ResultImpact, ResultCost, ResultPar_Rho, ResultPar_Epsilon, ResultPar_Kappa, ResultPar_Gamma, ResultPar_Sigma, ResultPar_Omega)
        colnames(Result_DALYs) <<- c("DALYs", "Cost", "Rho", "Epsilon", "Kappa", "Gamma", "Sigma", "Omega")

        # ----------------------------------------- #
        # Subsetting those achieving 90-90-90 Stuff #
        # ----------------------------------------- #

        theList_909090 <- FindResults_909090(theList)

        ResultPar_909090 <- FindPar_909090(theList, par)

        Result909090Impact <- c()
        Result909090Cost <- c()
        Result909090Par_Rho <- c()
        Result909090Par_Epsilon <- c()
        Result909090Par_Kappa <- c()
        Result909090Par_Gamma <- c()
        Result909090Par_Sigma <- c()
        Result909090Par_Omega <- c()
        if(length(theList_909090) > 0) {
            for(i in 1:length(theList_909090)) {
                print(i)
                setProgress(value = i/length(theList_909090), message = 'Compiling results (90-90-90)', detail = '')
                Result909090Impact[i] <- Calc_DALYsAverted(theList_909090[[i]], base_DALY)
                Result909090Cost[i] <- Calc_AdditionalCost(theList_909090[[i]], base_COST)
                Result909090Par_Rho[i] <- par[i,]$Rho
                Result909090Par_Epsilon[i] <- par[i,]$Epsilon
                Result909090Par_Kappa[i] <- par[i,]$Kappa
                Result909090Par_Gamma[i] <- par[i,]$Gamma
                Result909090Par_Sigma[i] <- par[i,]$Sigma
                Result909090Par_Omega[i] <- par[i,]$Omega
            }

            Result_DALYs_909090 <<- data.frame(Result909090Impact, Result909090Cost, Result909090Par_Rho, Result909090Par_Epsilon, Result909090Par_Kappa, Result909090Par_Gamma, Result909090Par_Sigma, Result909090Par_Omega)
            colnames(Result_DALYs_909090) <<- c("DALYs", "Cost", "Rho", "Epsilon", "Kappa", "Gamma", "Sigma", "Omega")
        } else {
            Result_DALYs_909090 <<- data.frame(0, 0, 0, 0, 0, 0, 0, 0)
            colnames(Result_DALYs_909090) <<- c("DALYs", "Cost", "Rho", "Epsilon", "Kappa", "Gamma", "Sigma", "Omega")
        }

        print("Result_909090 =")
        print(Result_909090)

        print("Result_DALYs =")
        print(Result_DALYs)

        print("Result_DALYs_909090 =")
        print(Result_DALYs_909090)

        setProgress(value = 1, message = paste("Finished. Time =", round(proc.time()[[1]] - time, 0), "sec"))
        updateButton(session, "optFinished", label = "OPTIMISATION COMPLETE", style = "success", size = "large", block = TRUE, icon = icon("check", class = "fa-lg fa-fw", lib = "font-awesome"))
        updateButton(session, "optimiseInput", label = "", style = "primary", block = TRUE, size = "large", icon = icon("check", class = "fa-lg fa-fw", lib = "font-awesome"))
    })
})
