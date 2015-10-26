library(shiny)
library(ggplot2)
library(dplyr)
library(deSolve)
library(gridExtra)
library(DT)
library(shinyjs)
library(googlesheets)
library(RColorBrewer)
library(scales)
library(ggthemes)
# devtools::install_github("shinyTable", "trestletech")
# library(shinyTable)

source("TheModel.R")

function(input, output, session) {

    Parameters <- reactive({c(
        Nu_1 = 0.193634,
        Nu_2 = 0.321304,
        Nu_3 = 0.328285,
        Nu_4 = 0.497247,
        Nu_5 = 0.559090,
        Nu_6 = 0.846406,
        Rho = input$rho,
        Epsilon = input$epsilon,
        Kappa = 1.079,
        Gamma = input$gamma,
        Theta = 1.511,
        Omega = input$omega,
        p = 0.95,
        s_1 = 0.1,
        s_2 = 0.2,
        s_3 = 0.3,
        s_4 = 0.4,
        s_5 = 1,
        s_6 = 2,
        s_7 = 3,
        Sigma = 0,
        Delta_1 = 1.58084765,
        Delta_2 = 3.50371789,
        Delta_3 = 3.50371789,
        Delta_4 = 3.50371789,
        Delta_5 = 3.50371789,
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
        Dx_unitCost = input$userDxUnitCost,
        Linkage_unitCost = input$userLinkageUnitCost,
        Annual_Care_unitCost = input$userAnnualCareUnit,
        Annual_ART_unitCost = input$userAnnualARTUnitCost
    )})

    output$parameterTable <- renderTable({
        theP <- Parameters()
        theParameters <- c(theP[["Rho"]],theP[["Epsilon"]],theP[["Kappa"]],theP[["Gamma"]],theP[["Theta"]],theP[["Omega"]],theP[["Nu_1"]],theP[["Nu_2"]],theP[["Nu_3"]],theP[["Nu_4"]],theP[["Nu_5"]],theP[["Nu_6"]],theP[["p"]],theP[["s_1"]],theP[["s_2"]],theP[["s_3"]],theP[["s_4"]],theP[["s_5"]],theP[["s_6"]],theP[["s_7"]],theP[["Sigma"]],theP[["Delta_1"]],theP[["Delta_2"]],theP[["Delta_3"]],theP[["Delta_4"]],theP[["Delta_5"]],theP[["Alpha_1"]],theP[["Alpha_2"]],theP[["Alpha_3"]],theP[["Alpha_4"]],theP[["Alpha_5"]],theP[["Alpha_6"]],theP[["Alpha_7"]],theP[["Tau_1"]],theP[["Tau_2"]],theP[["Tau_3"]],theP[["Tau_4"]],theP[["Tau_5"]],theP[["Tau_6"]],theP[["Tau_7"]],theP[["Mu"]],0.5251,0.2315,0.2401,0.0033)
        ParameterNames <- c("Rho","Epsilon","Kappa","Gamma","Theta","Omega","Nu_1","Nu_2","Nu_3","Nu_4","Nu_5","Nu_6","p","s_1","s_2","s_3","s_4","s_5","s_6","s_7","Sigma","Delta_1","Delta_2","Delta_3","Delta_4","Delta_5","Alpha_1","Alpha_2","Alpha_3","Alpha_4","Alpha_5","Alpha_6","Alpha_7","Tau_1","Tau_2","Tau_3","Tau_4","Tau_5","Tau_6","Tau_7","Mu","Iota_1","Iota_2","Iota_3","Iota_4")
        rows <- length(ParameterNames)
        tbl <- matrix(theParameters,rows,ncol=2)
        tbl[,1] <- ParameterNames
        colnames(tbl) <- c("Parameter","Value")
        return(tbl)
    })

    Initial <- reactive({c(
        UnDx_500 = (input$userPLHIV - input$userDx) * 0.5251,
        UnDx_350500 = (input$userPLHIV - input$userDx) * 0.2315,
        UnDx_250350 = (input$userPLHIV - input$userDx) * 0.1787,
        UnDx_200250 = (input$userPLHIV - input$userDx) * 0.0615,
        UnDx_100200 = (input$userPLHIV - input$userDx) * 0.0011,
        UnDx_50100 = (input$userPLHIV - input$userDx) * 0.0008,
        UnDx_50 = (input$userPLHIV - input$userDx) * 0.0014,

        Dx_500 = (input$userDx - input$userCare - input$userLtfu) * 0.5251,
        Dx_350500 = (input$userDx - input$userCare - input$userLtfu) * 0.2315,
        Dx_250350 = (input$userDx - input$userCare - input$userLtfu) * 0.1787,
        Dx_200250 = (input$userDx - input$userCare - input$userLtfu) * 0.0615,
        Dx_100200 = (input$userDx - input$userCare - input$userLtfu) * 0.0011,
        Dx_50100 = (input$userDx - input$userCare - input$userLtfu) * 0.0008,
        Dx_50 = (input$userDx - input$userCare - input$userLtfu) * 0.0014,

        Care_500 = (input$userCare - input$userTx) * 0.5251,
        Care_350500 = (input$userCare - input$userTx) * 0.2315,
        Care_250350 = (input$userCare - input$userTx) * 0.1787,
        Care_200250 = (input$userCare - input$userTx) * 0.0615,
        Care_100200 = (input$userCare - input$userTx) * 0.0011,
        Care_50100 = (input$userCare - input$userTx) * 0.0008,
        Care_50 = (input$userCare - input$userTx) * 0.0014,

        PreLtfu_500 = 0 * 0.5251,
        PreLtfu_350500 = 0 * 0.2315,
        PreLtfu_250350 = 0 * 0.1787,
        PreLtfu_200250 = 0 * 0.0615,
        PreLtfu_100200 = 0 * 0.0011,
        PreLtfu_50100 = 0 * 0.0008,
        PreLtfu_50 = 0 * 0.0014,

        Tx_Na_500 = (input$userTx - input$userVs) * 0.5251,
        Tx_Na_350500 = (input$userTx - input$userVs) * 0.2315,
        Tx_Na_250350 = (input$userTx - input$userVs) * 0.1787,
        Tx_Na_200250 = (input$userTx - input$userVs) * 0.0615,
        Tx_Na_100200 = (input$userTx - input$userVs) * 0.0011,
        Tx_Na_50100 = (input$userTx - input$userVs) * 0.0008,
        Tx_Na_50 = (input$userTx - input$userVs) * 0.0014,

        Tx_A_500 = input$userVs * 0.5251,
        Tx_A_350500 = input$userVs * 0.2315,
        Tx_A_250350 = input$userVs * 0.1787,
        Tx_A_200250 = input$userVs * 0.0615,
        Tx_A_100200 = input$userVs * 0.0011,
        Tx_A_50100 = input$userVs * 0.0008,
        Tx_A_50 = input$userVs * 0.0014,

        Ltfu_500 = (input$userLtfu) * 0.5251,
        Ltfu_350500 = (input$userLtfu) * 0.2315,
        Ltfu_250350 = (input$userLtfu) * 0.1787,
        Ltfu_200250 = (input$userLtfu) * 0.0615,
        Ltfu_100200 = (input$userLtfu) * 0.0011,
        Ltfu_50100 = (input$userLtfu) * 0.0008,
        Ltfu_50 = (input$userLtfu) * 0.0014,
        
        # Keeping track
        NewInf = 0,
        HivMortality = 0,
        NaturalMortality = 0,

        # Costs
        Dx_Cost = 0,
        Linkage_Cost = 0,
        Annual_Care_Cost = 0,
        Annual_ART_Cost = 0
    )})

    observeEvent(input$demoInput, {
        updateSelectInput(session,"userCountry",selected="Kenya")

        if(input$userPLHIV == 0 || is.na(input$userPLHIV)) {
            randPLHIV <- round(runif(1,1e+6,1e+7),0)
            newDx <- round(randPLHIV * 0.831923,0)
            newCare <- round(newDx * 0.606497,0)
            newTx <- round(newCare * 0.490679,0)
            newVs <- round(newTx * 0.466145,0)
            newLtfu <- round(newDx * 0.051168,0)

            updateNumericInput(session,"userPLHIV",value=randPLHIV)
            updateNumericInput(session,"userDx",value=newDx)
            updateNumericInput(session,"userCare",value=newCare)
            updateNumericInput(session,"userTx",value=newTx)
            updateNumericInput(session,"userVs",value=newVs)
            updateNumericInput(session,"userLtfu",value=newLtfu)
        } else {
            newDx <- round(input$userPLHIV * 0.831923,0)
            newCare <- round(newDx * 0.606497,0)
            newTx <- round(newCare * 0.490679,0)
            newVs <- round(newTx * 0.466145,0)
            newLtfu <- round(newDx * 0.051168,0)

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

    out <- reactive({

        Time <- seq(0,5,0.02)

        # Ability to turn off HIV incidence in the model.
        if(input$incidenceInput == TRUE) {
            theInitial <- Initial()
            Numerator <- NewInfections
            Denominator <- as.double(((theInitial[["UnDx_500"]] + theInitial[["Dx_500"]] + theInitial[["Care_500"]] + theInitial[["PreLtfu_500"]] + theInitial[["Tx_Na_500"]] + theInitial[["Ltfu_500"]]) * 1.35) + ((theInitial[["UnDx_350500"]] + theInitial[["Dx_350500"]] + theInitial[["Care_350500"]] + theInitial[["PreLtfu_350500"]] + theInitial[["Tx_Na_350500"]] + theInitial[["Ltfu_350500"]]) * 1) + ((theInitial[["UnDx_250350"]] + theInitial[["Dx_250350"]] + theInitial[["Care_250350"]] + theInitial[["PreLtfu_250350"]] + theInitial[["Tx_Na_250350"]] + theInitial[["Ltfu_250350"]] + theInitial[["UnDx_200250"]] + theInitial[["Dx_200250"]] + theInitial[["Care_200250"]] + theInitial[["PreLtfu_200250"]] + theInitial[["Tx_Na_200250"]] + theInitial[["Ltfu_200250"]]) * 1.64) + ((theInitial[["UnDx_100200"]] + theInitial[["Dx_100200"]] + theInitial[["Care_100200"]] + theInitial[["PreLtfu_100200"]] + theInitial[["Tx_Na_100200"]] + theInitial[["Ltfu_100200"]] + theInitial[["UnDx_50100"]] + theInitial[["Dx_50100"]] + theInitial[["Care_50100"]] + theInitial[["PreLtfu_50100"]] + theInitial[["Tx_Na_50100"]] + theInitial[["Ltfu_50100"]] + theInitial[["UnDx_50"]] + theInitial[["Dx_50"]] + theInitial[["Care_50"]] + theInitial[["PreLtfu_50"]] + theInitial[["Tx_Na_50"]] + theInitial[["Ltfu_50"]]) * 5.17) + ((theInitial[["Tx_A_500"]] + theInitial[["Tx_A_350500"]] + theInitial[["Tx_A_250350"]] + theInitial[["Tx_A_200250"]] + theInitial[["Tx_A_100200"]] + theInitial[["Tx_A_50100"]] + theInitial[["Tx_A_50"]]) * 0.1))
            # print(paste("Numerator =",Numerator))
            # print(paste("Denominator =",Denominator))
            Beta <<- Numerator / Denominator
        } else {
            Beta <<- 0
        }
        print(paste("Beta:",Beta))

        # The Model #
        theOut <- data.frame(ode(times=Time, y=Initial(), func=ComplexCascade, parms=Parameters()))
        # --------- #

        # Post-simulation mutation (creation of columns) etc.
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
        theOut <- mutate(theOut,cd4_500 = (UnDx_500 + Dx_500 + Care_500 + PreLtfu_500 + Tx_Na_500 + Tx_A_500 + Ltfu_500) / N)
        theOut <- mutate(theOut,cd4_350500 = (UnDx_350500 + Dx_350500 + Care_350500 + PreLtfu_350500 + Tx_Na_350500 + Tx_A_350500 + Ltfu_350500) / N)
        theOut <- mutate(theOut,cd4_250350 = (UnDx_250350 + Dx_250350 + Care_250350 + PreLtfu_250350 + Tx_Na_250350 + Tx_A_250350 + Ltfu_250350) / N)
        theOut <- mutate(theOut,cd4_200250 = (UnDx_200250 + Dx_200250 + Care_200250 + PreLtfu_200250 + Tx_Na_200250 + Tx_A_200250 + Ltfu_200250) / N)
        theOut <- mutate(theOut,cd4_100200 = (UnDx_100200 + Dx_100200 + Care_100200 + PreLtfu_100200 + Tx_Na_100200 + Tx_A_100200 + Ltfu_100200) / N)
        theOut <- mutate(theOut,cd4_50100 = (UnDx_50100 + Dx_50100 + Care_50100 + PreLtfu_50100 + Tx_Na_50100 + Tx_A_50100 + Ltfu_50100) / N)
        theOut <- mutate(theOut,cd4_50 = (UnDx_50 + Dx_50 + Care_50 + PreLtfu_50 + Tx_Na_50 + Tx_A_50 + Ltfu_50) / N)
        return(theOut)
    })

    output$plotOne <- renderPlot({
        p <- ggplot(out(), aes_string(x="time",y=input$y)) + 
        geom_line(size=2) + 
        theme_classic() +
        # theme_economist() +
        theme(axis.text.x=element_text(size=18)) +
        theme(axis.text.y=element_text(size=18)) +
        theme(axis.title=element_text(size=20)) +
        xlab("Year") +
        scale_x_continuous(limits=c(0,5),breaks=seq(0,5,1),labels=seq(2015,2020,1))
        print(p)
        }, 
        height=500,
        width=700
    )

    output$plotTwo <- renderPlot({
        out <- out()
        a <- ggplot(out,aes(x=time,y=UnDx)) +
            geom_line() +
            theme(axis.text.x=element_text(size=18)) +
            theme(axis.text.y=element_text(size=18)) +
            theme(axis.title=element_text(size=20)) +
            xlab("Year") +
            theme_classic()

        b <- ggplot(out,aes(x=time,y=Dx)) +
            geom_line() +
            theme(axis.text.x=element_text(size=18)) +
            theme(axis.text.y=element_text(size=18)) +
            theme(axis.title=element_text(size=20)) +
            xlab("Year") +
            theme_classic()

        c <- ggplot(out,aes(x=time,y=Care)) +
            geom_line() +
            theme(axis.text.x=element_text(size=18)) +
            theme(axis.text.y=element_text(size=18)) +
            theme(axis.title=element_text(size=20)) +
            xlab("Year") +
            theme_classic()

        d <- ggplot(out,aes(x=time,y=PreLtfu)) +
            geom_line() +
            theme(axis.text.x=element_text(size=18)) +
            theme(axis.text.y=element_text(size=18)) +
            theme(axis.title=element_text(size=20)) +
            xlab("Year") +
            theme_classic()

        e <- ggplot(out,aes(x=time,y=Tx)) +
            geom_line() +
            theme(axis.text.x=element_text(size=18)) +
            theme(axis.text.y=element_text(size=18)) +
            theme(axis.title=element_text(size=20)) +
            xlab("Year") +
            theme_classic()

        f <- ggplot(out,aes(x=time,y=Vs)) +
            geom_line() +
            theme(axis.text.x=element_text(size=18)) +
            theme(axis.text.y=element_text(size=18)) +
            theme(axis.title=element_text(size=20)) +
            xlab("Year") +
            theme_classic()

        g <- ggplot(out,aes(x=time,y=Ltfu)) +
            geom_line() +
            theme(axis.text.x=element_text(size=18)) +
            theme(axis.text.y=element_text(size=18)) +
            theme(axis.title=element_text(size=20)) +
            xlab("Year") +
            theme_classic()

        h <- ggplot(out,aes(x=time,y=N)) +
            geom_line() +
            theme(axis.text.x=element_text(size=18)) +
            theme(axis.text.y=element_text(size=18)) +
            theme(axis.title=element_text(size=20)) +
            xlab("Year") +
            theme_classic()

        i <- ggplot(out,aes(x=time,y=NewInf)) +
            geom_line() +
            theme(axis.text.x=element_text(size=18)) +
            theme(axis.text.y=element_text(size=18)) +
            theme(axis.title=element_text(size=20)) +
            xlab("Year") +
            theme_classic()

        j <- ggplot(out,aes(x=time,y=TotalCost)) +
            geom_line() +
            theme(axis.text.x=element_text(size=18)) +
            theme(axis.text.y=element_text(size=18)) +
            theme(axis.title=element_text(size=20)) +
            xlab("Year") +
            theme_classic()

        k <- ggplot(out,aes(x=time,y=HivMortalityProp)) +
            geom_line() +
            theme(axis.text.x=element_text(size=18)) +
            theme(axis.text.y=element_text(size=18)) +
            theme(axis.title=element_text(size=20)) +
            xlab("Year") +
            theme_classic()

        l <- ggplot(out,aes(x=time,y=NaturalMortalityProp)) +
            geom_line() +
            theme(axis.text.x=element_text(size=18)) +
            theme(axis.text.y=element_text(size=18)) +
            theme(axis.title=element_text(size=20)) +
            xlab("Year") +
            theme_classic()

        AllPlot <- grid.arrange(a,b,c,d,e,f,g,h,i,j,k,l,nrow=4,ncol=3)

        print(AllPlot)

        },
        height=900,
        width=900
    )

    output$plotCascade <- renderPlot({
        out <- out()

        t0_N = as.double(sum(filter(out,time == 0) %>% select(N)))
        t0_dx = as.double(sum(filter(out,time == 0) %>% select(c(Dx_500,Dx_350500,Dx_250350,Dx_200250,Dx_100200,Dx_50100,Dx_50,Care_500,Care_350500,Care_250350,Care_200250,Care_100200,Care_50100,Care_50,PreLtfu_500,PreLtfu_350500,PreLtfu_250350,PreLtfu_200250,PreLtfu_100200,PreLtfu_50100,PreLtfu_50,Tx_Na_500,Tx_Na_350500,Tx_Na_250350,Tx_Na_200250,Tx_Na_100200,Tx_Na_50100,Tx_Na_50,Tx_A_500,Tx_A_350500,Tx_A_250350,Tx_A_200250,Tx_A_100200,Tx_A_50100,Tx_A_50,Ltfu_500,Ltfu_350500,Ltfu_250350,Ltfu_200250,Ltfu_100200,Ltfu_50100,Ltfu_50)))) / t0_N
        t0_cx = as.double(sum(filter(out,time == 0) %>% select(c(Care_500,Care_350500,Care_250350,Care_200250,Care_100200,Care_50100,Care_50,Tx_A_500,Tx_A_350500,Tx_A_250350,Tx_A_200250,Tx_A_100200,Tx_A_50100,Tx_A_50,Tx_Na_500,Tx_Na_350500,Tx_Na_250350,Tx_Na_200250,Tx_Na_100200,Tx_Na_50100,Tx_Na_50)))) / t0_N
        t0_tx = as.double(sum(filter(out,time == 0) %>% select(c(Tx_A_500,Tx_A_350500,Tx_A_250350,Tx_A_200250,Tx_A_100200,Tx_A_50100,Tx_A_50,Tx_Na_500,Tx_Na_350500,Tx_Na_250350,Tx_Na_200250,Tx_Na_100200,Tx_Na_50100,Tx_Na_50)))) / t0_N
        t0_vs = as.double(sum(filter(out,time == 0) %>% select(c(Tx_A_500,Tx_A_350500,Tx_A_250350,Tx_A_200250,Tx_A_100200,Tx_A_50100,Tx_A_50)))) / t0_N
        t0_ltfu = as.double(sum(filter(out,time == 0) %>% select(c(PreLtfu_500,PreLtfu_350500,PreLtfu_250350,PreLtfu_200250,PreLtfu_100200,PreLtfu_50100,PreLtfu_50,Ltfu_500,Ltfu_350500,Ltfu_250350,Ltfu_200250,Ltfu_100200,Ltfu_50100,Ltfu_50)))) / t0_N
        
        t0_results <- c(t0_dx,t0_cx,t0_tx,t0_vs,t0_ltfu)

        definition <- c("% Diagnosed","% In Care","% On Treatment","% Suppressed","% LTFU")
        t0 <- data.frame(definition,t0_results)

        levels(t0$definition)
        t0$definition <- factor(t0$definition, levels=c("% Diagnosed","% In Care","% On Treatment","% Suppressed","% LTFU"))

        fill.coll <- rev(brewer.pal(9,"Blues")[4:8])

        o <- ggplot(t0,aes(definition,t0_results))
        o <- o + geom_bar(aes(fill=definition),position='dodge',stat='identity')
        o <- o + scale_y_continuous(limits=c(0,1), breaks=seq(0,1,0.1),labels=percent)
        o <- o + scale_fill_manual(values=fill.coll)
        o <- o + ggtitle("Care Cascade in 2015")
        o <- o + theme_classic()
        o <- o + theme(title=element_text(size=18))
        o <- o + theme(axis.title=element_blank())
        o <- o + theme(axis.text.x=element_text(size=15))
        o <- o + theme(axis.text.y=element_text(size=18))
        o <- o + theme(legend.position="none")

        t5_N = as.double(sum(filter(out,time == 5) %>% select(N)))
        t5_dx = as.double(sum(filter(out,time == 5) %>% select(c(Dx_500,Dx_350500,Dx_250350,Dx_200250,Dx_100200,Dx_50100,Dx_50,Care_500,Care_350500,Care_250350,Care_200250,Care_100200,Care_50100,Care_50,PreLtfu_500,PreLtfu_350500,PreLtfu_250350,PreLtfu_200250,PreLtfu_100200,PreLtfu_50100,PreLtfu_50,Tx_Na_500,Tx_Na_350500,Tx_Na_250350,Tx_Na_200250,Tx_Na_100200,Tx_Na_50100,Tx_Na_50,Tx_A_500,Tx_A_350500,Tx_A_250350,Tx_A_200250,Tx_A_100200,Tx_A_50100,Tx_A_50,Ltfu_500,Ltfu_350500,Ltfu_250350,Ltfu_200250,Ltfu_100200,Ltfu_50100,Ltfu_50)))) / t5_N
        t5_cx = as.double(sum(filter(out,time == 5) %>% select(c(Care_500,Care_350500,Care_250350,Care_200250,Care_100200,Care_50100,Care_50,Tx_A_500,Tx_A_350500,Tx_A_250350,Tx_A_200250,Tx_A_100200,Tx_A_50100,Tx_A_50,Tx_Na_500,Tx_Na_350500,Tx_Na_250350,Tx_Na_200250,Tx_Na_100200,Tx_Na_50100,Tx_Na_50)))) / t5_N
        t5_tx = as.double(sum(filter(out,time == 5) %>% select(c(Tx_A_500,Tx_A_350500,Tx_A_250350,Tx_A_200250,Tx_A_100200,Tx_A_50100,Tx_A_50,Tx_Na_500,Tx_Na_350500,Tx_Na_250350,Tx_Na_200250,Tx_Na_100200,Tx_Na_50100,Tx_Na_50)))) / t5_N
        t5_vs = as.double(sum(filter(out,time == 5) %>% select(c(Tx_A_500,Tx_A_350500,Tx_A_250350,Tx_A_200250,Tx_A_100200,Tx_A_50100,Tx_A_50)))) / t5_N
        t5_ltfu = as.double(sum(filter(out,time == 5) %>% select(c(PreLtfu_500,PreLtfu_350500,PreLtfu_250350,PreLtfu_200250,PreLtfu_100200,PreLtfu_50100,PreLtfu_50,Ltfu_500,Ltfu_350500,Ltfu_250350,Ltfu_200250,Ltfu_100200,Ltfu_50100,Ltfu_50)))) / t5_N

        t5_results <- c(t5_dx,t5_cx,t5_tx,t5_vs,t5_ltfu)

        definition <- c("% Diagnosed","% In Care","% On Treatment","% Suppressed","% LTFU")
        t5 <- data.frame(definition,t5_results)

        levels(t5$definition)
        t5$definition <- factor(t5$definition, levels=c("% Diagnosed","% In Care","% On Treatment","% Suppressed","% LTFU"))

        fill.coll <- rev(brewer.pal(9,"Blues")[4:8])

        p <- ggplot(t5,aes(definition,t5_results))
        p <- p + geom_bar(aes(fill=definition),position='dodge',stat='identity')
        p <- p + scale_y_continuous(limits=c(0,1), breaks=seq(0,1,0.1),labels=percent)
        p <- p + scale_fill_manual(values=fill.coll)
        p <- p + ggtitle("Care Cascade in 2020")
        p <- p + theme_classic()
        p <- p + theme(title=element_text(size=18))
        p <- p + theme(axis.title=element_blank())
        p <- p + theme(axis.text.x=element_text(size=15))
        p <- p + theme(axis.text.y=element_text(size=18))
        p <- p + theme(legend.position="none")

        print(grid.arrange(o,p,nrow=1,ncol=2))
        },
        height=400,
        width=1250
    )

    output$plotPowersCascade <- renderPlot({
        out <- out()

        t0_N = as.double(sum(filter(out,time == 0) %>% select(N)))
        t0_undx = as.double(sum(filter(out,time == 0) %>% select(c(UnDx_500,UnDx_350500,UnDx_250350,UnDx_200250,UnDx_100200,UnDx_50100,UnDx_50)))) / t0_N
        t0_dx = as.double(sum(filter(out,time == 0) %>% select(c(Dx_500,Dx_350500,Dx_250350,Dx_200250,Dx_100200,Dx_50100,Dx_50)))) / t0_N
        t0_cx = as.double(sum(filter(out,time == 0) %>% select(c(Care_500,Care_350500,Care_250350,Care_200250,Care_100200,Care_50100,Care_50)))) / t0_N
        t0_preltfu = as.double(sum(filter(out,time == 0) %>% select(c(PreLtfu_500,PreLtfu_350500,PreLtfu_250350,PreLtfu_200250,PreLtfu_100200,PreLtfu_50100,PreLtfu_50)))) / t0_N
        t0_tx_na = as.double(sum(filter(out,time == 0) %>% select(c(Tx_Na_500,Tx_Na_350500,Tx_Na_250350,Tx_Na_200250,Tx_Na_100200,Tx_Na_50100,Tx_Na_50)))) / t0_N
        t0_vs = as.double(sum(filter(out,time == 0) %>% select(c(Tx_A_500,Tx_A_350500,Tx_A_250350,Tx_A_200250,Tx_A_100200,Tx_A_50100,Tx_A_50)))) / t0_N
        t0_ltfu = as.double(sum(filter(out,time == 0) %>% select(c(Ltfu_500,Ltfu_350500,Ltfu_250350,Ltfu_200250,Ltfu_100200,Ltfu_50100,Ltfu_50)))) / t0_N

        tResult <- c(t0_vs,t0_tx_na,t0_cx,t0_dx,t0_undx,t0_preltfu,t0_ltfu,
                     t0_vs,t0_tx_na,t0_cx,t0_dx,t0_preltfu,t0_ltfu,
                     t0_vs,t0_tx_na,t0_cx,
                     t0_vs,t0_tx_na,
                     t0_vs,
                     t0_preltfu,t0_ltfu)

        State <- c("% Suppressed","% On Treatment (non-adherent)","% In Care","% Diagnosed","% Undiagnosed","% pre-ART LTFU","% LTFU",
                   "% Suppressed","% On Treatment (non-adherent)","% In Care","% Diagnosed","% pre-ART LTFU","% LTFU",
                   "% Suppressed","% On Treatment (non-adherent)","% In Care",
                   "% Suppressed","% On Treatment (non-adherent)",
                   "% Suppressed",
                   "% pre-ART LTFU","% LTFU")

        tOrder <- c(rep("All",7),
                    rep("Diagnosed",6),
                    rep("In Care",3),
                    rep("On Treatment",2),
                    rep("Virally Suppressed",1),
                    rep("LTFU",2))

        t0 <- data.frame(State,tResult,tOrder)

        levels(t0$tOrder)
        t0$tOrder <- factor(t0$tOrder, levels=c("All","Diagnosed","In Care","On Treatment","Virally Suppressed","LTFU"))

        levels(t0$State)
        t0$State <- factor(t0$State, levels=c("% Suppressed","% On Treatment (non-adherent)","% In Care","% Diagnosed","% Undiagnosed","% pre-ART LTFU","% LTFU"))

        cols <- brewer.pal(9,"Set1")
        power.col <- c(cols[3],cols[2],cols[4],cols[5],cols[1],cols[9],cols[8])

        o <- ggplot(t0,aes(x=tOrder,y=tResult,fill=State))
        o <- o + geom_bar(stat='identity')
        o <- o + scale_y_continuous(breaks=seq(0,1,0.1),labels=percent)
        o <- o + scale_fill_manual(values=power.col)
        o <- o + ggtitle("Care Cascade in 2015")
        o <- o + theme_classic()
        o <- o + theme(title=element_text(size=18))
        o <- o + theme(axis.title=element_blank())
        o <- o + theme(axis.text.x=element_text(size=13))
        o <- o + theme(axis.text.y=element_text(size=15))
        o <- o + theme(legend.text=element_text(size=15))

        t5_N = as.double(sum(filter(out,time == 5) %>% select(N)))
        t5_undx = as.double(sum(filter(out,time == 5) %>% select(c(UnDx_500,UnDx_350500,UnDx_250350,UnDx_200250,UnDx_100200,UnDx_50100,UnDx_50)))) / t5_N
        t5_dx = as.double(sum(filter(out,time == 5) %>% select(c(Dx_500,Dx_350500,Dx_250350,Dx_200250,Dx_100200,Dx_50100,Dx_50)))) / t5_N
        t5_cx = as.double(sum(filter(out,time == 5) %>% select(c(Care_500,Care_350500,Care_250350,Care_200250,Care_100200,Care_50100,Care_50)))) / t5_N
        t5_preltfu = as.double(sum(filter(out,time == 5) %>% select(c(PreLtfu_500,PreLtfu_350500,PreLtfu_250350,PreLtfu_200250,PreLtfu_100200,PreLtfu_50100,PreLtfu_50)))) / t5_N
        t5_tx_na = as.double(sum(filter(out,time == 5) %>% select(c(Tx_Na_500,Tx_Na_350500,Tx_Na_250350,Tx_Na_200250,Tx_Na_100200,Tx_Na_50100,Tx_Na_50)))) / t5_N
        t5_vs = as.double(sum(filter(out,time == 5) %>% select(c(Tx_A_500,Tx_A_350500,Tx_A_250350,Tx_A_200250,Tx_A_100200,Tx_A_50100,Tx_A_50)))) / t5_N
        t5_ltfu = as.double(sum(filter(out,time == 5) %>% select(c(Ltfu_500,Ltfu_350500,Ltfu_250350,Ltfu_200250,Ltfu_100200,Ltfu_50100,Ltfu_50)))) / t5_N

        tResult <- c(t5_vs,t5_tx_na,t5_cx,t5_dx,t5_undx,t5_preltfu,t5_ltfu,
                     t5_vs,t5_tx_na,t5_cx,t5_dx,t5_preltfu,t5_ltfu,
                     t5_vs,t5_tx_na,t5_cx,
                     t5_vs,t5_tx_na,
                     t5_vs,
                     t5_preltfu,t5_ltfu)

        State <- c("% Suppressed","% On Treatment (non-adherent)","% In Care","% Diagnosed","% Undiagnosed","% pre-ART LTFU","% LTFU",
                   "% Suppressed","% On Treatment (non-adherent)","% In Care","% Diagnosed","% pre-ART LTFU","% LTFU",
                   "% Suppressed","% On Treatment (non-adherent)","% In Care",
                   "% Suppressed","% On Treatment (non-adherent)",
                   "% Suppressed",
                   "% pre-ART LTFU","% LTFU")

        tOrder <- c(rep("All",7),
                    rep("Diagnosed",6),
                    rep("In Care",3),
                    rep("On Treatment",2),
                    rep("Virally Suppressed",1),
                    rep("LTFU",2))

        t5 <- data.frame(State,tResult,tOrder)

        levels(t5$tOrder)
        t5$tOrder <- factor(t5$tOrder, levels=c("All","Diagnosed","In Care","On Treatment","Virally Suppressed","LTFU"))

        levels(t5$State)
        t5$State <- factor(t5$State, levels=c("% Suppressed","% On Treatment (non-adherent)","% In Care","% Diagnosed","% Undiagnosed","% pre-ART LTFU","% LTFU"))

        power.col <- c(cols[3],cols[2],cols[4],cols[5],cols[1],cols[9],cols[8])

        p <- ggplot(t5,aes(x=tOrder,y=tResult,fill=State))
        p <- p + geom_bar(stat='identity')
        p <- p + scale_y_continuous(breaks=seq(0,1,0.1),labels=percent)
        p <- p + scale_fill_manual(values=power.col)
        p <- p + ggtitle("Care Cascade in 2020")
        p <- p + theme_classic()
        p <- p + theme(title=element_text(size=18))
        p <- p + theme(axis.title=element_blank())
        p <- p + theme(axis.text.x=element_text(size=13))
        p <- p + theme(axis.text.y=element_text(size=15))
        p <- p + theme(legend.text=element_text(size=15))

        return(grid.arrange(o,p,nrow=1,ncol=2))
    },
    height=400
    )

    output$plot909090 <- renderPlot({
        out <- out()
        PLHIV = as.double(sum(filter(out,time == 5) %>% select(N)))
        # dx / PLHIV
        dx = as.double(sum(filter(out,time == 5) %>% select(c(Dx_500,Dx_350500,Dx_250350,Dx_200250,Dx_100200,Dx_50100,Dx_50,Care_500,Care_350500,Care_250350,Care_200250,Care_100200,Care_50100,Care_50,PreLtfu_500,PreLtfu_350500,PreLtfu_250350,PreLtfu_200250,PreLtfu_100200,PreLtfu_50100,PreLtfu_50,Tx_Na_500,Tx_Na_350500,Tx_Na_250350,Tx_Na_200250,Tx_Na_100200,Tx_Na_50100,Tx_Na_50,Tx_A_500,Tx_A_350500,Tx_A_250350,Tx_A_200250,Tx_A_100200,Tx_A_50100,Tx_A_50,Ltfu_500,Ltfu_350500,Ltfu_250350,Ltfu_200250,Ltfu_100200,Ltfu_50100,Ltfu_50))))
        # tx / dx
        tx = as.double(sum(filter(out,time == 5) %>% select(c(Tx_A_500,Tx_A_350500,Tx_A_250350,Tx_A_200250,Tx_A_100200,Tx_A_50100,Tx_A_50,Tx_Na_500,Tx_Na_350500,Tx_Na_250350,Tx_Na_200250,Tx_Na_100200,Tx_Na_50100,Tx_Na_50))))
        # vs / tx
        vs = as.double(sum(filter(out,time == 5) %>% select(c(Tx_A_500,Tx_A_350500,Tx_A_250350,Tx_A_200250,Tx_A_100200,Tx_A_50100,Tx_A_50))))

        p_dx <- dx / PLHIV
        p_tx <- tx / dx
        p_vs <- vs / tx

        results <- c(p_dx,p_tx,p_vs)
        definition <- c("% Diagnosed","% On Treatment","% Suppressed")
        Scenario <- c("Baseline")
        the909090 <- data.frame(definition,results,Scenario)

        levels(the909090$definition)
        the909090$definition <- factor(the909090$definition, levels=c("% Diagnosed","% On Treatment","% Suppressed"))

        fill.coll <- brewer.pal(4,"Set1")

        o <- ggplot(the909090,aes(definition,results))
        o <- o + geom_bar(aes(fill=definition),position='dodge',stat='identity')
        o <- o + scale_y_continuous(limits=c(0,1), breaks=seq(0,1,0.1),labels=percent)
        o <- o + scale_fill_manual(values=fill.coll)
        o <- o + geom_abline(intercept=0.9, slope=0)
        o <- o + theme_classic()
        o <- o + theme(title=element_text(size=20))
        o <- o + theme(axis.title=element_blank())
        o <- o + theme(axis.text.x=element_text(size=18))
        o <- o + theme(axis.text.y=element_text(size=18))
        o <- o + theme(legend.position="none")
        print(o)
        },
        height=400,
        width=700
    )

    output$table909090 <- renderTable({
        out <- out()
        PLHIV = as.double(sum(filter(out,time == 5) %>% select(N)))
        # dx / PLHIV
        dx = as.double(sum(filter(out,time == 5) %>% select(c(Dx_500,Dx_350500,Dx_250350,Dx_200250,Dx_100200,Dx_50100,Dx_50,Care_500,Care_350500,Care_250350,Care_200250,Care_100200,Care_50100,Care_50,PreLtfu_500,PreLtfu_350500,PreLtfu_250350,PreLtfu_200250,PreLtfu_100200,PreLtfu_50100,PreLtfu_50,Tx_Na_500,Tx_Na_350500,Tx_Na_250350,Tx_Na_200250,Tx_Na_100200,Tx_Na_50100,Tx_Na_50,Tx_A_500,Tx_A_350500,Tx_A_250350,Tx_A_200250,Tx_A_100200,Tx_A_50100,Tx_A_50,Ltfu_500,Ltfu_350500,Ltfu_250350,Ltfu_200250,Ltfu_100200,Ltfu_50100,Ltfu_50))))
        # tx / dx
        tx = as.double(sum(filter(out,time == 5) %>% select(c(Tx_A_500,Tx_A_350500,Tx_A_250350,Tx_A_200250,Tx_A_100200,Tx_A_50100,Tx_A_50,Tx_Na_500,Tx_Na_350500,Tx_Na_250350,Tx_Na_200250,Tx_Na_100200,Tx_Na_50100,Tx_Na_50))))
        # vs / tx
        vs = as.double(sum(filter(out,time == 5) %>% select(c(Tx_A_500,Tx_A_350500,Tx_A_250350,Tx_A_200250,Tx_A_100200,Tx_A_50100,Tx_A_50))))

        p_dx <- dx / PLHIV
        p_tx <- tx / dx
        p_vs <- vs / tx

        Result <- c(p_dx,p_tx,p_vs)
        Target <- c("% Diagnosed","% On Treatment","% Suppressed")
        the909090 <- data.frame(Target,Result)

        levels(the909090$Target)
        the909090$Target <- factor(the909090$Target, levels=c("% Diagnosed","% On Treatment","% Suppressed"))

        return(the909090)
    })

    output$plotNewInf <- renderPlot({
        p <- ggplot(out(), aes(x=time,y=NewInfProp)) + 
            geom_line(size=2) + 
            theme_classic() +
            theme(axis.text.x=element_text(size=18)) +
            theme(axis.text.y=element_text(size=18)) +
            theme(axis.title=element_text(size=20)) +
            xlab("Year") +
            ylab("# new infections / total infected population") +
            scale_x_continuous(limits=c(0,5),breaks=seq(0,5,1),labels=seq(2015,2020,1))
        print(p)
        }, 
        height=500,
        width=700
    )

    output$plotAidsDeaths <- renderPlot({
        p <- ggplot(out(), aes(x=time,y=HivMortalityProp)) + 
            geom_line(size=2) + 
            theme_classic() +
            theme(axis.text.x=element_text(size=18)) +
            theme(axis.text.y=element_text(size=18)) +
            theme(axis.title=element_text(size=20)) +
            xlab("Year") +
            ylab("# AIDS deaths / total infected population") +
            scale_x_continuous(limits=c(0,5),breaks=seq(0,5,1),labels=seq(2015,2020,1))
        print(p)
        }, 
        height=500,
        width=700
    )

    output$outputTable <- DT::renderDataTable({
        return(out())
        },
        options=list(autoWidth=TRUE,pageLength=100)
    )

    # ------------ #
    # OPTIMISATION #
    # ------------ #

    optimisationValues <- reactiveValues(
        theRho = 0,
        theEpsilon = 0,
        theGamma = 0,
        theOmega = 0
        )
    
    output$optimisationTable <- renderTable({
        tbl_names <- c("Rho","Epsilon","Gamma","Omega")
        tbl_data <- c(
            optimisationValues$theRho,
            optimisationValues$theEpsilon,
            optimisationValues$theGamma,
            optimisationValues$theOmega
            )
        tbl <- matrix(tbl_data,nrow=4,ncol=2)
        tbl[,1] <- c(tbl_names)
        colnames(tbl) <- c("Parameter","Value")
        return(tbl)
    })

    output$optimisationCostTable <- renderTable({
        out <- out()
        theDx_Cost <- dollar(round(sum(filter(out,time == 5) %>% select(Dx_Cost)),0))
        theLinkage_Cost <- dollar(round(sum(filter(out,time == 5) %>% select(Linkage_Cost)),0))
        theAnnual_Care_Cost <- dollar(round(sum(filter(out,time == 5) %>% select(Annual_Care_Cost)),0))
        theAnnual_ART_Cost <- dollar(round(sum(filter(out,time == 5) %>% select(Annual_ART_Cost)),0))
        Cost <- c(theDx_Cost,theLinkage_Cost,theAnnual_Care_Cost,theAnnual_ART_Cost)
        Category <- c("Testing costs","Linkage costs","Annual Care Costs","Annual Treatment Costs")
        CostTable <- data.frame(Category,Cost)
        levels(CostTable$Category)
        CostTable$Category <- factor(CostTable$Category, levels=c("Testing costs","Linkage costs","Annual Care Costs","Annual Treatment Costs"))
        return(CostTable)
    })

    output$unitCostTable <- renderTable({
        theP <- Parameters()
        Dx_unitCost <- dollar(as.double(theP["Dx_unitCost"]))
        Linkage_unitCost <- dollar(as.double(theP["Linkage_unitCost"]))
        Annual_Care_unitCost <- dollar(as.double(theP["Annual_Care_unitCost"]))
        Annual_ART_unitCost <- dollar(as.double(theP["Annual_ART_unitCost"]))
        Cost <- c(Dx_unitCost,Linkage_unitCost,Annual_Care_unitCost,Annual_ART_unitCost)
        Unit <- c("HIV-testing","Linkage","Annual Care","Annual Treatment")
        UnitCostTable <- data.frame(Unit,Cost)
        UnitCostTable$Unit <- factor(UnitCostTable$Unit, levels=c("HIV-testing","Linkage","Annual Care","Annual Treatment"))
        return(UnitCostTable)
    })

    observeEvent(input$optimiseInput, {

        find909090 <- function(target, par) {

            optimisationValues$theRho <- par[1]
            optimisationValues$theEpsilon <- par[4]
            optimisationValues$theGamma <- par[2]
            optimisationValues$theOmega <- par[3]

            print(paste("par =",par))

            Time <- seq(0,5,0.02)

            # Ability to turn off HIV incidence in the model.
            if(input$incidenceInput == TRUE) {
                theInitial <- Initial()
                Numerator <- NewInfections
                Denominator <- as.double(((theInitial[["UnDx_500"]] + theInitial[["Dx_500"]] + theInitial[["Care_500"]] + theInitial[["PreLtfu_500"]] + theInitial[["Tx_Na_500"]] + theInitial[["Ltfu_500"]]) * 1.35) + ((theInitial[["UnDx_350500"]] + theInitial[["Dx_350500"]] + theInitial[["Care_350500"]] + theInitial[["PreLtfu_350500"]] + theInitial[["Tx_Na_350500"]] + theInitial[["Ltfu_350500"]]) * 1) + ((theInitial[["UnDx_250350"]] + theInitial[["Dx_250350"]] + theInitial[["Care_250350"]] + theInitial[["PreLtfu_250350"]] + theInitial[["Tx_Na_250350"]] + theInitial[["Ltfu_250350"]] + theInitial[["UnDx_200250"]] + theInitial[["Dx_200250"]] + theInitial[["Care_200250"]] + theInitial[["PreLtfu_200250"]] + theInitial[["Tx_Na_200250"]] + theInitial[["Ltfu_200250"]]) * 1.64) + ((theInitial[["UnDx_100200"]] + theInitial[["Dx_100200"]] + theInitial[["Care_100200"]] + theInitial[["PreLtfu_100200"]] + theInitial[["Tx_Na_100200"]] + theInitial[["Ltfu_100200"]] + theInitial[["UnDx_50100"]] + theInitial[["Dx_50100"]] + theInitial[["Care_50100"]] + theInitial[["PreLtfu_50100"]] + theInitial[["Tx_Na_50100"]] + theInitial[["Ltfu_50100"]] + theInitial[["UnDx_50"]] + theInitial[["Dx_50"]] + theInitial[["Care_50"]] + theInitial[["PreLtfu_50"]] + theInitial[["Tx_Na_50"]] + theInitial[["Ltfu_50"]]) * 5.17) + ((theInitial[["Tx_A_500"]] + theInitial[["Tx_A_350500"]] + theInitial[["Tx_A_250350"]] + theInitial[["Tx_A_200250"]] + theInitial[["Tx_A_100200"]] + theInitial[["Tx_A_50100"]] + theInitial[["Tx_A_50"]]) * 0.1))
                Beta <<- Numerator / Denominator
            } else {
                Beta <<- 0
            }

            theP <- Parameters()
            theP["Rho"] = par[1]
            theP["Gamma"] = par[2]
            theP["Omega"] = par[3]
            theP["Epsilon"] = par[4]

            # The Model #
            theOut <- data.frame(ode(times=Time, y=Initial(), func=ComplexCascade, parms=theP))
            # --------- #

            # Post-simulation mutation (creation of columns) etc.
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
            theOut <- mutate(theOut,cd4_500 = (UnDx_500 + Dx_500 + Care_500 + PreLtfu_500 + Tx_Na_500 + Tx_A_500 + Ltfu_500) / N)
            theOut <- mutate(theOut,cd4_350500 = (UnDx_350500 + Dx_350500 + Care_350500 + PreLtfu_350500 + Tx_Na_350500 + Tx_A_350500 + Ltfu_350500) / N)
            theOut <- mutate(theOut,cd4_250350 = (UnDx_250350 + Dx_250350 + Care_250350 + PreLtfu_250350 + Tx_Na_250350 + Tx_A_250350 + Ltfu_250350) / N)
            theOut <- mutate(theOut,cd4_200250 = (UnDx_200250 + Dx_200250 + Care_200250 + PreLtfu_200250 + Tx_Na_200250 + Tx_A_200250 + Ltfu_200250) / N)
            theOut <- mutate(theOut,cd4_100200 = (UnDx_100200 + Dx_100200 + Care_100200 + PreLtfu_100200 + Tx_Na_100200 + Tx_A_100200 + Ltfu_100200) / N)
            theOut <- mutate(theOut,cd4_50100 = (UnDx_50100 + Dx_50100 + Care_50100 + PreLtfu_50100 + Tx_Na_50100 + Tx_A_50100 + Ltfu_50100) / N)
            theOut <- mutate(theOut,cd4_50 = (UnDx_50 + Dx_50 + Care_50 + PreLtfu_50 + Tx_Na_50 + Tx_A_50 + Ltfu_50) / N)
           
            PLHIV = as.double(sum(filter(theOut,time == 5) %>% select(N)))
            dx = as.double(sum(filter(theOut,time == 5) %>% select(c(Dx_500,Dx_350500,Dx_250350,Dx_200250,Dx_100200,Dx_50100,Dx_50,Care_500,Care_350500,Care_250350,Care_200250,Care_100200,Care_50100,Care_50,PreLtfu_500,PreLtfu_350500,PreLtfu_250350,PreLtfu_200250,PreLtfu_100200,PreLtfu_50100,PreLtfu_50,Tx_Na_500,Tx_Na_350500,Tx_Na_250350,Tx_Na_200250,Tx_Na_100200,Tx_Na_50100,Tx_Na_50,Tx_A_500,Tx_A_350500,Tx_A_250350,Tx_A_200250,Tx_A_100200,Tx_A_50100,Tx_A_50,Ltfu_500,Ltfu_350500,Ltfu_250350,Ltfu_200250,Ltfu_100200,Ltfu_50100,Ltfu_50))))
            tx = as.double(sum(filter(theOut,time == 5) %>% select(c(Tx_A_500,Tx_A_350500,Tx_A_250350,Tx_A_200250,Tx_A_100200,Tx_A_50100,Tx_A_50,Tx_Na_500,Tx_Na_350500,Tx_Na_250350,Tx_Na_200250,Tx_Na_100200,Tx_Na_50100,Tx_Na_50))))
            vs = as.double(sum(filter(theOut,time == 5) %>% select(c(Tx_A_500,Tx_A_350500,Tx_A_250350,Tx_A_200250,Tx_A_100200,Tx_A_50100,Tx_A_50))))
            p_dx <- dx / PLHIV
            p_tx <- tx / dx
            p_vs <- vs / tx
            results <- c(p_dx,p_tx,p_vs)
            definition <- c("% Diagnosed","% On Treatment","% Suppressed")
            the909090 <- data.frame(definition,results)
            output <- 1/3 * sum((target - the909090$results)^2)
            return(output)
        }

        withProgress(message = 'Running optimisation', value = 0, {
            incProgress(0.1/1, message = 'Running optimisation')
            theResult <- optim(par = c(0.205,2.556,0.033,16.949), find909090, target = 0.9, lower = c(0.01,0.01,0.01,0.05), upper = c(5,5,5,20), method = 'L-BFGS-B')
            incProgress(0.5/1, message = 'calculating results')

            # Fill out results table.
            optimisationValues$theRho <- theResult$par[1]
            optimisationValues$theEpsilon <- theResult$par[4]
            optimisationValues$theGamma <- theResult$par[2]
            optimisationValues$theOmega <- theResult$par[3]

            # Update sliders on "Parameter" page.
            updateSliderInput(session,"rho",value=theResult$par[1],min=0,max=5,step=0.01)
            updateSliderInput(session,"epsilon",value=theResult$par[4],min=0,max=5,step=0.01)
            updateSliderInput(session,"gamma",value=theResult$par[2],min=0,max=5,step=0.01)
            updateSliderInput(session,"omega",value=theResult$par[3],min=0,max=5,step=0.01)

            print(theResult$par)
            incProgress(1, message = 'Complete.')
        })
    })

    output$plotOptimised909090 <- renderPlot({
        # Should refresh when this button is pressed.
        input$optimiseInput

        out <- out()
        
        PLHIV = as.double(sum(filter(out,time == 5) %>% select(N)))
        # dx / PLHIV
        dx = as.double(sum(filter(out,time == 5) %>% select(c(Dx_500,Dx_350500,Dx_250350,Dx_200250,Dx_100200,Dx_50100,Dx_50,Care_500,Care_350500,Care_250350,Care_200250,Care_100200,Care_50100,Care_50,PreLtfu_500,PreLtfu_350500,PreLtfu_250350,PreLtfu_200250,PreLtfu_100200,PreLtfu_50100,PreLtfu_50,Tx_Na_500,Tx_Na_350500,Tx_Na_250350,Tx_Na_200250,Tx_Na_100200,Tx_Na_50100,Tx_Na_50,Tx_A_500,Tx_A_350500,Tx_A_250350,Tx_A_200250,Tx_A_100200,Tx_A_50100,Tx_A_50,Ltfu_500,Ltfu_350500,Ltfu_250350,Ltfu_200250,Ltfu_100200,Ltfu_50100,Ltfu_50))))
        # tx / dx
        tx = as.double(sum(filter(out,time == 5) %>% select(c(Tx_A_500,Tx_A_350500,Tx_A_250350,Tx_A_200250,Tx_A_100200,Tx_A_50100,Tx_A_50,Tx_Na_500,Tx_Na_350500,Tx_Na_250350,Tx_Na_200250,Tx_Na_100200,Tx_Na_50100,Tx_Na_50))))
        # vs / tx
        vs = as.double(sum(filter(out,time == 5) %>% select(c(Tx_A_500,Tx_A_350500,Tx_A_250350,Tx_A_200250,Tx_A_100200,Tx_A_50100,Tx_A_50))))

        p_dx <- dx / PLHIV
        p_tx <- tx / dx
        p_vs <- vs / tx

        results <- c(p_dx,p_tx,p_vs)
        definition <- c("% Diagnosed","% On Treatment","% Suppressed")
        Scenario <- c("Baseline")
        the909090 <- data.frame(definition,results,Scenario)

        levels(the909090$definition)
        the909090$definition <- factor(the909090$definition, levels=c("% Diagnosed","% On Treatment","% Suppressed"))

        fill.coll <- brewer.pal(4,"Set1")

        o <- ggplot(the909090,aes(definition,results))
        o <- o + geom_bar(aes(fill=definition),position='dodge',stat='identity')
        o <- o + scale_y_continuous(limits=c(0,1), breaks=seq(0,1,0.1),labels=percent)
        o <- o + scale_fill_manual(values=fill.coll)
        o <- o + geom_abline(intercept=0.9, slope=0)
        o <- o + theme_classic()
        o <- o + theme(title=element_text(size=20))
        o <- o + theme(axis.title=element_blank())
        o <- o + theme(axis.text.x=element_text(size=18))
        o <- o + theme(axis.text.y=element_text(size=18))
        o <- o + theme(legend.position="none")
        print(o)        
        },
        height=400,
        width=700
    )

    # Saving input values from setup tab.
    saveCascadeData <- function(data) {
        # Grab the Google Sheet
        sheet <- gs_title("WHO-Cascade-Data")
        # Add the data as a new row
        gs_add_row(sheet, input = data)
    }

    getIncidenceData <- function() {
        theTable <- gs_title("SpectrumIncidenceEstimates")
        return(gs_read(theTable,ws="NewInfections"))
    }

    observeEvent(input$userCountry, {
        NewInfections <<- as.double(as.double(filter(getIncidenceData(),Country==input$userCountry) %>% select(NewInfections2014)))
        if(is.na(NewInfections)) {
            output$warningText <- renderText({return(paste("Warning! NA value returned from",input$userCountry,"data. Using Kenya as default."))})
            NewInfections <<- 56353
        } else {
            output$warningText <- renderText({return(paste(input$userCountry,"data loaded."))})
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

    # Render Flow Diagram of Model.
    output$modelFlowImage <- renderImage({
        return(list(
            src='www/ModelSimple.png',
            contentType='image/png',
            alt='ModelFlowDiagram',
            height=250,
            width=1000))
    },
    deleteFile=FALSE)

    # Inverse sliders for parameter window #

    observeEvent(input$rho, {updateSliderInput(session,"invRho",value=1/input$rho,min=0,max=100,step=0.001)})
    observeEvent(input$invRho, {updateSliderInput(session,"rho",value=1/input$invRho,min=0,max=5,step=0.001)})
    observeEvent(input$epsilon, {updateSliderInput(session,"invEpsilon",value=1/input$epsilon,min=0,max=100,step=0.001)})
    observeEvent(input$invEpsilon, {updateSliderInput(session,"epsilon",value=1/input$invEpsilon,min=0,max=20,step=0.001)})
    observeEvent(input$gamma, {updateSliderInput(session,"invGamma",value=1/input$gamma,min=0,max=100,step=0.001)})
    observeEvent(input$invGamma, {updateSliderInput(session,"gamma",value=1/input$invGamma,min=0,max=5,step=0.001)})
    observeEvent(input$omega, {updateSliderInput(session,"invOmega",value=1/input$omega,min=0,max=100,step=0.001)})
    observeEvent(input$invOmega, {updateSliderInput(session,"omega",value=1/input$invOmega,min=0,max=5,step=0.001)})
}