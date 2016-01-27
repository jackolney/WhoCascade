observeEvent(input$demoInput, {
    updateSelectInput(session,"userCountry",selected="Kenya")

    if(input$userPLHIV == 0 || is.na(input$userPLHIV)) {
        newPLHIV <- round(1.4e+6,0) # Estimate from Kenya (Marrakech)
        newDx <- round(newPLHIV * 0.79262,0) # Estimate from AMPATH
        newCare <- round(848018,0) # Estimate from Kenya (Marrakech)
        newTx <- round(748000,0) # Estimate from Kenya (Marrakech)
        newVs <- round(295000,0) # Estimate from Kenya (Marrakech)
        newLtfu <- round(0,0) # Estimate from Kenya (Marrakech)

        updateNumericInput(session,"userPLHIV",value=newPLHIV)
        updateNumericInput(session,"userDx",value=newDx)
        updateNumericInput(session,"userCare",value=newCare)
        updateNumericInput(session,"userTx",value=newTx)
        updateNumericInput(session,"userVs",value=newVs)
        updateNumericInput(session,"userLtfu",value=newLtfu)
    } else {
        newPLHIV <- round(1.4e+6,0) # Estimate from Kenya (Marrakech)
        newDx <- round(newPLHIV * 0.79262,0) # Estimate from AMPATH
        newCare <- round(848018,0) # Estimate from Kenya (Marrakech)
        newTx <- round(748000,0) # Estimate from Kenya (Marrakech)
        newVs <- round(295000,0) # Estimate from Kenya (Marrakech)
        newLtfu <- round(0,0) # Estimate from Kenya (Marrakech)

        updateNumericInput(session,"userPLHIV",value=newPLHIV)
        updateNumericInput(session,"userDx",value=newDx)
        updateNumericInput(session,"userCare",value=newCare)
        updateNumericInput(session,"userTx",value=newTx)
        updateNumericInput(session,"userVs",value=newVs)
        updateNumericInput(session,"userLtfu",value=newLtfu)
    }
})

observeEvent(input$userRetArt12mths, {
    if(input$userRetArt12mths != 0 || is.na(input$userRetArt12mths)) {
        newValue <- -log(input$userRetArt12mths)
        updateSliderInput(session,"omega",value=newValue,min=0,max=5,step=0.01)
    }
})

# ------------ #
# OPTIMISATION #
# ------------ #

observeEvent(input$optimiseInput, {

    # Parameter Input Values
    ParInput <- expand.grid(
        Rho = seq(from = input$userOptRho_Range[1],to = input$userOptRho_Range[2],length.out = input$userOptRho_LengthOf),
        Epsilon = seq(from = input$userOptEpsilon_Range[1],to = input$userOptEpsilon_Range[2],length.out = input$userOptEpsilon_LengthOf),
        Kappa = seq(from = input$userOptKappa_Range[2],to = input$userOptKappa_Range[1],length.out = input$userOptKappa_LengthOf),
        Gamma = seq(from = input$userOptGamma_Range[1],to = input$userOptGamma_Range[2],length.out = input$userOptGamma_LengthOf),
        Sigma = seq(from = input$userOptSigma_Range[1],to = input$userOptSigma_Range[2],length.out = input$userOptSigma_LengthOf),
        Omega = seq(from = input$userOptOmega_Range[2],to = input$userOptOmega_Range[1],length.out = input$userOptOmega_LengthOf)
    )

    if(input$incidenceInput == TRUE) {
        theInitial <- GetInitial()
        Numerator <- NewInfections
        Denominator <- as.double(((theInitial[["UnDx_500"]] + theInitial[["Dx_500"]] + theInitial[["Care_500"]] + theInitial[["PreLtfu_500"]] + theInitial[["Tx_Na_500"]] + theInitial[["Ltfu_500"]]) * 1.35) + ((theInitial[["UnDx_350500"]] + theInitial[["Dx_350500"]] + theInitial[["Care_350500"]] + theInitial[["PreLtfu_350500"]] + theInitial[["Tx_Na_350500"]] + theInitial[["Ltfu_350500"]]) * 1) + ((theInitial[["UnDx_250350"]] + theInitial[["Dx_250350"]] + theInitial[["Care_250350"]] + theInitial[["PreLtfu_250350"]] + theInitial[["Tx_Na_250350"]] + theInitial[["Ltfu_250350"]] + theInitial[["UnDx_200250"]] + theInitial[["Dx_200250"]] + theInitial[["Care_200250"]] + theInitial[["PreLtfu_200250"]] + theInitial[["Tx_Na_200250"]] + theInitial[["Ltfu_200250"]]) * 1.64) + ((theInitial[["UnDx_100200"]] + theInitial[["Dx_100200"]] + theInitial[["Care_100200"]] + theInitial[["PreLtfu_100200"]] + theInitial[["Tx_Na_100200"]] + theInitial[["Ltfu_100200"]] + theInitial[["UnDx_50100"]] + theInitial[["Dx_50100"]] + theInitial[["Care_50100"]] + theInitial[["PreLtfu_50100"]] + theInitial[["Tx_Na_50100"]] + theInitial[["Ltfu_50100"]] + theInitial[["UnDx_50"]] + theInitial[["Dx_50"]] + theInitial[["Care_50"]] + theInitial[["PreLtfu_50"]] + theInitial[["Tx_Na_50"]] + theInitial[["Ltfu_50"]]) * 5.17) + ((theInitial[["Tx_A_500"]] + theInitial[["Tx_A_350500"]] + theInitial[["Tx_A_250350"]] + theInitial[["Tx_A_200250"]] + theInitial[["Tx_A_100200"]] + theInitial[["Tx_A_50100"]] + theInitial[["Tx_A_50"]]) * 0.1))
        # print(paste("Numerator =",Numerator))
        # print(paste("Denominator =",Denominator))
        Beta <<- Numerator / Denominator
    } else {
        Beta <<- 0
    }

    RunSimulation <- function(par,target) {

        OptPar <- c(
            Nu_1 = 0.193634,
            Nu_2 = 0.321304,
            Nu_3 = 0.328285,
            Nu_4 = 0.497247,
            Nu_5 = 0.559090,
            Nu_6 = 0.846406,
            Rho = par[["Rho"]],
            Epsilon = par[["Epsilon"]],
            Kappa = par[["Kappa"]],
            Gamma = par[["Gamma"]],
            Theta = 1.511,
            Omega = par[["Omega"]],
            p = 0.95,
            s_1 = 0.1,
            s_2 = 0.2,
            s_3 = 0.3,
            s_4 = 0.4,
            s_5 = 1,
            s_6 = 2,
            s_7 = 3,
            Sigma = par[["Sigma"]],
            Delta_1 = 0.5178,
            Delta_2 = 0.8862,
            Delta_3 = 0.8832,
            Delta_4 = 1.1581,
            Delta_5 = 2.5663,
            Alpha_1 = 0.004110,
            Alpha_2 = 0.011670,
            Alpha_3 = 0.009385,
            Alpha_4 = 0.016394,
            Alpha_5 = 0.027656,
            Alpha_6 = 0.047877,
            Alpha_7 = 1.081964,
            Tau_1 = 0.003905,
            Tau_2 = 0.011087,
            Tau_3 = 0.008916,
            Tau_4 = 0.015574,
            Tau_5 = 0.026273,
            Tau_6 = 0.045482,
            Tau_7 = 1.02785,
            Mu = 0.0374,
            ART_All = input$userART_All,
            ART_500 = input$userART_500,
            ART_350 = input$userART_350,
            ART_200 = input$userART_200,
            Dx_unitCost = input$userDxUnitCost,
            Linkage_unitCost = input$userLinkageUnitCost,
            Annual_Care_unitCost = input$userAnnualCareUnit,
            Annual_ART_unitCost = input$userAnnualARTUnitCost
        )

        Time <- seq(0,5,0.02)
        theOut <- data.frame(ode(times = Time, y = GetInitial(), func = ComplexCascade, parms = OptPar))
        theOut <- mutate(theOut,N = UnDx_500 + UnDx_350500 + UnDx_250350 + UnDx_200250 + UnDx_100200 + UnDx_50100 + UnDx_50 + Dx_500 + Dx_350500 + Dx_250350 + Dx_200250 + Dx_100200 + Dx_50100 + Dx_50 + Care_500 + Care_350500 + Care_250350 + Care_200250 + Care_100200 + Care_50100 + Care_50 + PreLtfu_500 + PreLtfu_350500 + PreLtfu_250350 + PreLtfu_200250 + PreLtfu_100200 + PreLtfu_50100 + PreLtfu_50 + Tx_Na_500 + Tx_Na_350500 + Tx_Na_250350 + Tx_Na_200250 + Tx_Na_100200 + Tx_Na_50100 + Tx_Na_50 + Tx_A_500 + Tx_A_350500 + Tx_A_250350 + Tx_A_200250 + Tx_A_100200 + Tx_A_50100 + Tx_A_50 + Ltfu_500 + Ltfu_350500 + Ltfu_250350 + Ltfu_200250 + Ltfu_100200 + Ltfu_50100 + Ltfu_50)
        theOut <- mutate(theOut,ART = (Tx_Na_500 + Tx_Na_350500 + Tx_Na_250350 + Tx_Na_200250 + Tx_Na_100200 + Tx_Na_50100 + Tx_Na_50 + Tx_A_500 + Tx_A_350500 + Tx_A_250350 + Tx_A_200250 + Tx_A_100200 + Tx_A_50100 + Tx_A_50) / N)
        theOut <- mutate(theOut,UnDx = (UnDx_500 + UnDx_350500 + UnDx_250350 + UnDx_200250 + UnDx_100200 + UnDx_50100 + UnDx_50) / N)
        theOut <- mutate(theOut,Dx = (Dx_500 + Dx_350500 + Dx_250350 + Dx_200250 + Dx_100200 + Dx_50100 + Dx_50) / N)
        theOut <- mutate(theOut,Care = (Care_500 + Care_350500 + Care_250350 + Care_200250 + Care_100200 + Care_50100 + Care_50) / N)
        theOut <- mutate(theOut,PreLtfu = (PreLtfu_500 + PreLtfu_350500 + PreLtfu_250350 + PreLtfu_200250 + PreLtfu_100200 + PreLtfu_50100 + PreLtfu_50) / N)
        theOut <- mutate(theOut,Tx = (Tx_Na_500 + Tx_Na_350500 + Tx_Na_250350 + Tx_Na_200250 + Tx_Na_100200 + Tx_Na_50100 + Tx_Na_50 + Tx_A_500 + Tx_A_350500 + Tx_A_250350 + Tx_A_200250 + Tx_A_100200 + Tx_A_50100 + Tx_A_50) / N)
        theOut <- mutate(theOut,Vs = (Tx_A_500 + Tx_A_350500 + Tx_A_250350 + Tx_A_200250 + Tx_A_100200 + Tx_A_50100 + Tx_A_50) / N)
        theOut <- mutate(theOut,Ltfu = (Ltfu_500 + Ltfu_350500 + Ltfu_250350 + Ltfu_200250 + Ltfu_100200 + Ltfu_50100 + Ltfu_50) / N)
        theOut <- mutate(theOut,NaturalMortalityProp = NaturalMortality / N)
        theOut <- mutate(theOut,HivMortalityProp = HivMortality / N)
        theOut <- mutate(theOut,NewInfProp = NewInf / N)
        theOut <- mutate(theOut,TotalCost = Dx_Cost + Linkage_Cost + Annual_Care_Cost + Annual_ART_Cost)
        theOut <- mutate(theOut,DALY = (((UnDx_500 + Dx_500 + Care_500 + PreLtfu_500 + Tx_Na_500 + Ltfu_500 + UnDx_350500 + Dx_350500 + Care_350500 + PreLtfu_350500 + Tx_Na_350500 + Ltfu_350500) * 0.078) +  # >350, no ART
                                  ((UnDx_250350 + Dx_250350 + Care_250350 + PreLtfu_250350 + Tx_Na_250350 + Ltfu_250350 + UnDx_200250 + Dx_200250 + Care_200250 + PreLtfu_200250 + Tx_Na_200250 + Ltfu_200250) * 0.274) +  # 200-350, no ART
                                  ((UnDx_100200 + Dx_100200 + Care_100200 + PreLtfu_100200 + Tx_Na_100200 + Ltfu_100200 + UnDx_50100 + Dx_50100 + Care_50100 + PreLtfu_50100 + Tx_Na_50100 + Ltfu_50100 + UnDx_50 + Dx_50 + Care_50 + PreLtfu_50 + Tx_Na_50 + Ltfu_50) * 0.582) + # <200, no ART
                                  ((Tx_A_500 + Tx_A_350500 + Tx_A_250350 + Tx_A_200250 + Tx_A_100200 + Tx_A_50100 + Tx_A_50) * 0.078))) # on ART and virally suppressed
        # PLHIV = as.double(sum(filter(theOut,time == 5) %>% select(N)))
        # dx = as.double(sum(filter(theOut,time == 5) %>% select(c(Dx_500,Dx_350500,Dx_250350,Dx_200250,Dx_100200,Dx_50100,Dx_50,Care_500,Care_350500,Care_250350,Care_200250,Care_100200,Care_50100,Care_50,PreLtfu_500,PreLtfu_350500,PreLtfu_250350,PreLtfu_200250,PreLtfu_100200,PreLtfu_50100,PreLtfu_50,Tx_Na_500,Tx_Na_350500,Tx_Na_250350,Tx_Na_200250,Tx_Na_100200,Tx_Na_50100,Tx_Na_50,Tx_A_500,Tx_A_350500,Tx_A_250350,Tx_A_200250,Tx_A_100200,Tx_A_50100,Tx_A_50,Ltfu_500,Ltfu_350500,Ltfu_250350,Ltfu_200250,Ltfu_100200,Ltfu_50100,Ltfu_50))))
        # tx = as.double(sum(filter(theOut,time == 5) %>% select(c(Tx_A_500,Tx_A_350500,Tx_A_250350,Tx_A_200250,Tx_A_100200,Tx_A_50100,Tx_A_50,Tx_Na_500,Tx_Na_350500,Tx_Na_250350,Tx_Na_200250,Tx_Na_100200,Tx_Na_50100,Tx_Na_50))))
        # vs = as.double(sum(filter(theOut,time == 5) %>% select(c(Tx_A_500,Tx_A_350500,Tx_A_250350,Tx_A_200250,Tx_A_100200,Tx_A_50100,Tx_A_50))))
        # p_dx <- dx / PLHIV
        # p_tx <- tx / dx
        # p_vs <- vs / tx
        # results <- c(p_dx,p_tx,p_vs)
        # definition <- c("% Diagnosed","% On Treatment","% Suppressed")
        # the909090 <- data.frame(definition,results)

        # Outputs to return
        # cost <<- as.double(sum(filter(theOut,time == 5) %>% select(TotalCost)))
        # output <- 1/3 * sum((target - the909090$results)^2)
        # print(paste("Error =",output,"Cost =",dollar(cost)))
        return(theOut)
    }

    withProgress(min = 0, max = 1, {

        setProgress(value = 0, message = 'Starting optimisation.', detail = 'This may take a while...')

        updateButton(session,"optFinished", label = " OPTIMISATION RUNNING", style = "warning", block = TRUE, size = "large", icon = icon("refresh", class = "fa-lg fa-spin", lib = "font-awesome"))
        updateButton(session,"optimiseInput", label = "", style = "primary", block = TRUE, size = "large", icon = icon("refresh", class = "fa-lg fa-spin", lib = "font-awesome"))

        # Need for loop in here {}
        Start.Time <- proc.time()[[1]]
        theList <- list()
        for(i in 1:dim(ParInput)[1]) {
            setProgress(value = i/dim(ParInput)[1], message = paste(paste('Run ',i,".",sep=''),"Time =",round(proc.time()[[1]] - Start.Time,0),"sec"),detail = "")
            theList[[rownames(ParInput)[i]]] <- RunSimulation(ParInput[i,],1)
        }

        Calc_Cost <- function(outFile) {
            theCost <- as.double(filter(outFile,time == 5) %>% select(TotalCost))
            return(theCost)
        }

        Calc_DALY <- function(outFile) {
            theDALY <- select(outFile,DALY) %>% sum()
            return(theDALY)
        }

        Calc_BaselineDALY <- function() {
            BaselinePar <- c(
                Rho = 0.205,
                Epsilon = 16.949,
                Kappa = 1.079,
                Gamma = 2.556,
                Sigma = 0,
                Omega = 0.033
                )
            Baseline <- RunSimulation(BaselinePar,1)
            BaselineDALY <- Calc_DALY(Baseline)
            return(BaselineDALY)
        }

        theBaselineDALY <- Calc_BaselineDALY()

        Calc_DALYsAverted <- function(outFile,BaselineDALY) {
            theDALY <- select(outFile,DALY) %>% sum()
            return(BaselineDALY - theDALY)
        }

        Calc_BaselineCost <- function() {
            BaselinePar <- c(
                Rho = 0.205,
                Epsilon = 16.949,
                Kappa = 1.079,
                Gamma = 2.556,
                Sigma = 0,
                Omega = 0.033
                )
            Baseline <- RunSimulation(BaselinePar,1)
            BaselineCost <- Calc_Cost(Baseline)
            return(BaselineCost)
        }

        theBaselineCost <- Calc_BaselineCost()

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
            Result90[i] <- Calc_909090_Result(theList[[i]])[1]
            Result9090[i] <- Calc_909090_Result(theList[[i]])[2]
            Result909090[i] <- Calc_909090_Result(theList[[i]])[3]
            ResultVS[i] <- Calc_VS(theList[[i]])
            ResultImpact[i] <- Calc_DALYsAverted(theList[[i]],theBaselineDALY)
            ResultCost[i] <- Calc_AdditionalCost(theList[[i]],theBaselineCost)
            ResultPar_Rho[i] <- ParInput[i,]$Rho
            ResultPar_Epsilon[i] <- ParInput[i,]$Epsilon
            ResultPar_Kappa[i] <- ParInput[i,]$Kappa
            ResultPar_Gamma[i] <- ParInput[i,]$Gamma
            ResultPar_Sigma[i] <- ParInput[i,]$Sigma
            ResultPar_Omega[i] <- ParInput[i,]$Omega
        }

        # ResultNames was used by ggplot for aes(color=Names) but is now deprecated
        ResultNames <- paste("p",seq(1,dim(ParInput)[1],1),sep='')

        # Result data.frame for plot(vs,cost)
        Result_909090 <<- data.frame(Result90,Result9090,Result909090,ResultVS,ResultCost,ResultPar_Rho,ResultPar_Epsilon,ResultPar_Kappa,ResultPar_Gamma,ResultPar_Sigma,ResultPar_Omega)
        colnames(Result_909090) <<- c("90","90-90","90-90-90","VS","Cost","Rho","Epsilon","Kappa","Gamma","Sigma","Omega")

        # Result data.frame for plot(DALYs,cost)
        Result_DALYs <<- data.frame(ResultImpact,ResultCost,ResultPar_Rho,ResultPar_Epsilon,ResultPar_Kappa,ResultPar_Gamma,ResultPar_Sigma,ResultPar_Omega)
        colnames(Result_DALYs) <<- c("DALYs","Cost","Rho","Epsilon","Kappa","Gamma","Sigma","Omega")

        # ----------------------------------------- #
        # Subsetting those achieving 90-90-90 Stuff #
        # ----------------------------------------- #

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

        theList909090 <- FindResults_909090(theList)
        ResultPar909090 <- FindPar_909090(theList)

        Result909090Impact <- c()
        Result909090Cost <- c()
        Result909090Par_Rho <- c()
        Result909090Par_Epsilon <- c()
        Result909090Par_Kappa <- c()
        Result909090Par_Gamma <- c()
        Result909090Par_Sigma <- c()
        Result909090Par_Omega <- c()
        if(length(theList909090) > 0) {
            for(i in 1:length(theList909090)) {
                print(i)
                Result909090Impact[i] <- Calc_DALYsAverted(theList909090[[i]],theBaselineDALY)
                Result909090Cost[i] <- Calc_AdditionalCost(theList909090[[i]],theBaselineCost)
                Result909090Par_Rho[i] <- ParInput[i,]$Rho
                Result909090Par_Epsilon[i] <- ParInput[i,]$Epsilon
                Result909090Par_Kappa[i] <- ParInput[i,]$Kappa
                Result909090Par_Gamma[i] <- ParInput[i,]$Gamma
                Result909090Par_Sigma[i] <- ParInput[i,]$Sigma
                Result909090Par_Omega[i] <- ParInput[i,]$Omega
            }

            Result_DALYs_909090 <<- data.frame(Result909090Impact,Result909090Cost,Result909090Par_Rho,Result909090Par_Epsilon,Result909090Par_Kappa,Result909090Par_Gamma,Result909090Par_Sigma,Result909090Par_Omega)
            colnames(Result_DALYs_909090) <<- c("DALYs","Cost","Rho","Epsilon","Kappa","Gamma","Sigma","Omega")
        } else {
            Result_DALYs_909090 <<- data.frame(0,0,0,0,0,0,0,0)
            colnames(Result_DALYs_909090) <<- c("DALYs","Cost","Rho","Epsilon","Kappa","Gamma","Sigma","Omega")
        }

        print("Result_909090 =")
        print(Result_909090)

        print("Result_DALYs =")
        print(Result_DALYs)

        print("Result_DALYs_909090 =")
        print(Result_DALYs_909090)

        setProgress(value = 1, message = paste("Finished. Time =",round(proc.time()[[1]] - Start.Time,0),"sec"))
        updateButton(session, "optFinished", label = "OPTIMISATION COMPLETE", style = "success", size = "large", block = TRUE, icon = icon("check", class = "fa-lg fa-fw", lib = "font-awesome"))
        updateButton(session, "optimiseInput", label = "", style = "primary", block = TRUE, size = "large", icon = icon("check", class = "fa-lg fa-fw", lib = "font-awesome"))
    })
})

# Plot 1
observeEvent(input$plotOpt909090_dblclick, {
    brush <- input$plotOpt909090_brush
    if (!is.null(brush)) {
        plotOpt_909090.ranges$x <- c(brush$xmin, brush$xmax)
        plotOpt_909090.ranges$y <- c(brush$ymin, brush$ymax)
    } else {
        plotOpt_909090.ranges$x <- NULL
        plotOpt_909090.ranges$y <- NULL
        }
    }
)

# Plot 2
observeEvent(input$plotOptDALYs_dblclick, {
    brush <- input$plotOptDALYs_brush
    if (!is.null(brush)) {
        plotOpt_DALYs.ranges$x <- c(brush$xmin, brush$xmax)
        plotOpt_DALYs.ranges$y <- c(brush$ymin, brush$ymax)
    } else {
        plotOpt_DALYs.ranges$x <- NULL
        plotOpt_DALYs.ranges$y <- NULL
        }
    }
)

# Plot 3
observeEvent(input$plotOptDALYs909090_dblclick, {
    brush <- input$plotOptDALYs909090_brush
    if (!is.null(brush)) {
        plotOpt_DALYs_909090.ranges$x <- c(brush$xmin, brush$xmax)
        plotOpt_DALYs_909090.ranges$y <- c(brush$ymin, brush$ymax)
    } else {
        plotOpt_DALYs_909090.ranges$x <- NULL
        plotOpt_DALYs_909090.ranges$y <- NULL
        }
    }
)

# Button Control

# Plot 1
observeEvent(input$showOpt909090Plot, ({
    updateCollapse(session, "optCollapse", open = "Plot 90-90-90")
}))

# Plot 2
observeEvent(input$showOptDALYsPlot, ({
    updateCollapse(session, "optCollapse", open = "Plot DALYs")
}))

# Plot 3
observeEvent(input$showOptDALYs909090Plot, ({
    updateCollapse(session, "optCollapse", open = "Plot DALYs (90-90-90)")
}))

# Reactive Budget Switch
Budget <- reactiveValues(Switch = "the909090")

observeEvent(input$showBudget909090, ({
    Budget$Switch <- "the909090"
}))

observeEvent(input$showBudgetDALYs, ({
    Budget$Switch <- "DALYs"
}))

observeEvent(input$userCountry, {
    # Find GSheet
    theTable <- locateSheet()
    # Read new infections
    NewInfections <<- as.double(as.double(filter(getIncidenceData(theTable),Country==input$userCountry) %>% select(NewInfections2014)))
    if(is.na(NewInfections)) {
        output$warningText <- renderText({return(paste("Warning! NA value returned from",input$userCountry,"data. Using Kenya as default."))})
        NewInfections <<- as.double(as.double(filter(getIncidenceData(theTable),Country=="Kenya") %>% select(NewInfections2014)))
    } else {
        output$warningText <- renderText({return(paste(input$userCountry,"data loaded."))})
    }
    # Read CD4 distributions
    theCD4 <- getCD4Data(theTable)
    if(is.na(as.double(filter(theCD4,Country == input$userCountry) %>% select(prop.Off.ART.500)))) {
        output$warningCD4Text <- renderText({return(paste("CD4 Warning! Using Kenya as default."))})
        prop_preART_500 <<- as.double(filter(theCD4,Country == "Kenya") %>% select(prop.Off.ART.500))
        prop_preART_350500 <<- as.double(filter(theCD4,Country == "Kenya") %>% select(prop.Off.ART.350500))
        prop_preART_250350 <<- as.double(filter(theCD4,Country == "Kenya") %>% select(prop.Off.ART.250350))
        prop_preART_200250 <<- as.double(filter(theCD4,Country == "Kenya") %>% select(prop.Off.ART.200250))
        prop_preART_100200 <<- as.double(filter(theCD4,Country == "Kenya") %>% select(prop.Off.ART.100200))
        prop_preART_50100 <<- as.double(filter(theCD4,Country == "Kenya") %>% select(prop.Off.ART.50100))
        prop_preART_50 <<- as.double(filter(theCD4,Country == "Kenya") %>% select(prop.Off.ART.50))
        prop_onART_500 <<- as.double(filter(theCD4,Country == "Kenya") %>% select(prop.On.ART.500))
        prop_onART_350500 <<- as.double(filter(theCD4,Country == "Kenya") %>% select(prop.On.ART.350500))
        prop_onART_250350 <<- as.double(filter(theCD4,Country == "Kenya") %>% select(prop.On.ART.250350))
        prop_onART_200250 <<- as.double(filter(theCD4,Country == "Kenya") %>% select(prop.On.ART.200250))
        prop_onART_100200 <<- as.double(filter(theCD4,Country == "Kenya") %>% select(prop.On.ART.100200))
        prop_onART_50100 <<- as.double(filter(theCD4,Country == "Kenya") %>% select(prop.On.ART.50100))
        prop_onART_50 <<- as.double(filter(theCD4,Country == "Kenya") %>% select(prop.On.ART.50))
    } else {
        # output$warningCD4Text <- renderText({return(paste(input$userCountry,"CD4 data loaded."))})
        prop_preART_500 <<- as.double(filter(theCD4,Country == input$userCountry) %>% select(prop.Off.ART.500))
        prop_preART_350500 <<- as.double(filter(theCD4,Country == input$userCountry) %>% select(prop.Off.ART.350500))
        prop_preART_250350 <<- as.double(filter(theCD4,Country == input$userCountry) %>% select(prop.Off.ART.250350))
        prop_preART_200250 <<- as.double(filter(theCD4,Country == input$userCountry) %>% select(prop.Off.ART.200250))
        prop_preART_100200 <<- as.double(filter(theCD4,Country == input$userCountry) %>% select(prop.Off.ART.100200))
        prop_preART_50100 <<- as.double(filter(theCD4,Country == input$userCountry) %>% select(prop.Off.ART.50100))
        prop_preART_50 <<- as.double(filter(theCD4,Country == input$userCountry) %>% select(prop.Off.ART.50))
        prop_onART_500 <<- as.double(filter(theCD4,Country == input$userCountry) %>% select(prop.On.ART.500))
        prop_onART_350500 <<- as.double(filter(theCD4,Country == input$userCountry) %>% select(prop.On.ART.350500))
        prop_onART_250350 <<- as.double(filter(theCD4,Country == input$userCountry) %>% select(prop.On.ART.250350))
        prop_onART_200250 <<- as.double(filter(theCD4,Country == input$userCountry) %>% select(prop.On.ART.200250))
        prop_onART_100200 <<- as.double(filter(theCD4,Country == input$userCountry) %>% select(prop.On.ART.100200))
        prop_onART_50100 <<- as.double(filter(theCD4,Country == input$userCountry) %>% select(prop.On.ART.50100))
        prop_onART_50 <<- as.double(filter(theCD4,Country == input$userCountry) %>% select(prop.On.ART.50))
    }
    print(paste("Country data:",NewInfections))
})

observeEvent(input$saveInput, {
    theResult <- c(input$userCountry,
        as.integer(input$userPLHIV),
        as.integer(input$userDx),
        as.integer(input$userCare),
        as.integer(input$userTx),
        as.integer(input$userVs),
        as.integer(input$userLtfu))
    print(theResult)
    saveCascadeData(theResult)
    output$saveText <- renderText({"Saved!"})
})

# Reset button stuff.
observeEvent(input$resetInput, {
    shinyjs::reset("setup-panel")
})

observeEvent(input$resetParameters, {
    shinyjs::reset("parameter-panel")
    updateNumericInput(session,"userRetArt12mths",value=0)
})

observeEvent(input$resetCost, {
    shinyjs::reset("cost-panel")
})

observeEvent(input$resetSliders, {
    shinyjs::reset("optimisation-panel")
})


# ART Initiation Checkbox Rules #
observeEvent(input$userART_All, {
    if(input$userART_All == TRUE) {
        updateCheckboxInput(session,"userART_500",value=TRUE)
        updateCheckboxInput(session,"userART_350",value=TRUE)
        updateCheckboxInput(session,"userART_200",value=TRUE)
    }
})

observeEvent(input$userART_500, {
    if(input$userART_500 == TRUE) {
        updateCheckboxInput(session,"userART_350",value=TRUE)
        updateCheckboxInput(session,"userART_200",value=TRUE)
    } else {
        updateCheckboxInput(session,"userART_All",value=FALSE)
    }
})

observeEvent(input$userART_350, {
    if(input$userART_350 == TRUE) {
        updateCheckboxInput(session,"userART_200",value=TRUE)
    } else {
        updateCheckboxInput(session,"userART_All",value=FALSE)
        updateCheckboxInput(session,"userART_500",value=FALSE)
    }
})

observeEvent(input$userART_200, {
    if(input$userART_200 == FALSE) {
        updateCheckboxInput(session,"userART_All",value=FALSE)
        updateCheckboxInput(session,"userART_500",value=FALSE)
        updateCheckboxInput(session,"userART_350",value=FALSE)
    }
})

# Switch between tabs without menu.
observeEvent(input$wizardSetup, {
    updateTabItems(session, inputId = "sideBar", selected = "setup")
})

observeEvent(input$wizardParameters, {
    updateTabItems(session, inputId = "sideBar", selected = "parameters")
})

observeEvent(input$wizardResults_1, {
    updateTabItems(session, inputId = "sideBar", selected = "your_cascade")
})

observeEvent(input$wizardResults_2, {
    updateTabItems(session, inputId = "sideBar", selected = "care_cascade")
})

observeEvent(input$wizardResults_3, {
    updateTabItems(session, inputId = "sideBar", selected = "powers_cascade")
})

observeEvent(input$wizardResults_4, {
    updateTabItems(session, inputId = "sideBar", selected = "_909090")
})

observeEvent(input$wizardResults_5, {
    updateTabItems(session, inputId = "sideBar", selected = "incidence_mortality")
})

observeEvent(input$wizardOpt_1, {
    updateTabItems(session, inputId = "sideBar", selected = "opt_cost")
})

observeEvent(input$wizardOpt_2, {
    updateTabItems(session, inputId = "sideBar", selected = "opt_parameter")
})

observeEvent(input$wizardOpt_3, {
    updateTabItems(session, inputId = "sideBar", selected = "opt_results")
})

observeEvent(input$wizardOpt_4, {
    updateTabItems(session, inputId = "sideBar", selected = "opt_budget")
})

# Inverse sliders for parameter window #
observeEvent(input$rho, {updateSliderInput(session,"invRho",value=1/input$rho,min=0,max=100,step=0.001)})
observeEvent(input$invRho, {updateSliderInput(session,"rho",value=1/input$invRho,min=0,max=5,step=0.001)})
observeEvent(input$epsilon, {updateSliderInput(session,"invEpsilon",value=1/input$epsilon,min=0,max=100,step=0.001)})
observeEvent(input$invEpsilon, {updateSliderInput(session,"epsilon",value=1/input$invEpsilon,min=0,max=20,step=0.001)})
observeEvent(input$gamma, {updateSliderInput(session,"invGamma",value=1/input$gamma,min=0,max=100,step=0.001)})
observeEvent(input$invGamma, {updateSliderInput(session,"gamma",value=1/input$invGamma,min=0,max=5,step=0.001)})
observeEvent(input$omega, {updateSliderInput(session,"invOmega",value=1/input$omega,min=0,max=100,step=0.001)})
observeEvent(input$invOmega, {updateSliderInput(session,"omega",value=1/input$invOmega,min=0,max=5,step=0.001)})
