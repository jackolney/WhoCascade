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
        ART_All = input$userART_All,
        ART_500 = input$userART_500,
        ART_350 = input$userART_350,
        ART_200 = input$userART_200,
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
        UnDx_500 = (input$userPLHIV - input$userDx) * prop_preART_500,
        UnDx_350500 = (input$userPLHIV - input$userDx) * prop_preART_350500,
        UnDx_250350 = (input$userPLHIV - input$userDx) * prop_preART_250350,
        UnDx_200250 = (input$userPLHIV - input$userDx) * prop_preART_200250,
        UnDx_100200 = (input$userPLHIV - input$userDx) * prop_preART_100200,
        UnDx_50100 = (input$userPLHIV - input$userDx) * prop_preART_50100,
        UnDx_50 = (input$userPLHIV - input$userDx) * prop_preART_50,

        Dx_500 = (input$userDx - input$userCare - input$userLtfu) * prop_preART_500,
        Dx_350500 = (input$userDx - input$userCare - input$userLtfu) * prop_preART_350500,
        Dx_250350 = (input$userDx - input$userCare - input$userLtfu) * prop_preART_250350,
        Dx_200250 = (input$userDx - input$userCare - input$userLtfu) * prop_preART_200250,
        Dx_100200 = (input$userDx - input$userCare - input$userLtfu) * prop_preART_100200,
        Dx_50100 = (input$userDx - input$userCare - input$userLtfu) * prop_preART_50100,
        Dx_50 = (input$userDx - input$userCare - input$userLtfu) * prop_preART_50,

        Care_500 = (input$userCare - input$userTx) * prop_preART_500,
        Care_350500 = (input$userCare - input$userTx) * prop_preART_350500,
        Care_250350 = (input$userCare - input$userTx) * prop_preART_250350,
        Care_200250 = (input$userCare - input$userTx) * prop_preART_200250,
        Care_100200 = (input$userCare - input$userTx) * prop_preART_100200,
        Care_50100 = (input$userCare - input$userTx) * prop_preART_50100,
        Care_50 = (input$userCare - input$userTx) * prop_preART_50,

        PreLtfu_500 = 0 * prop_preART_500,
        PreLtfu_350500 = 0 * prop_preART_350500,
        PreLtfu_250350 = 0 * prop_preART_250350,
        PreLtfu_200250 = 0 * prop_preART_200250,
        PreLtfu_100200 = 0 * prop_preART_100200,
        PreLtfu_50100 = 0 * prop_preART_50100,
        PreLtfu_50 = 0 * prop_preART_50,

        Tx_Na_500 = (input$userTx - input$userVs) * prop_onART_500,
        Tx_Na_350500 = (input$userTx - input$userVs) * prop_onART_350500,
        Tx_Na_250350 = (input$userTx - input$userVs) * prop_onART_250350,
        Tx_Na_200250 = (input$userTx - input$userVs) * prop_onART_200250,
        Tx_Na_100200 = (input$userTx - input$userVs) * prop_onART_100200,
        Tx_Na_50100 = (input$userTx - input$userVs) * prop_onART_50100,
        Tx_Na_50 = (input$userTx - input$userVs) * prop_onART_50,

        Tx_A_500 = input$userVs * prop_onART_500,
        Tx_A_350500 = input$userVs * prop_onART_350500,
        Tx_A_250350 = input$userVs * prop_onART_250350,
        Tx_A_200250 = input$userVs * prop_onART_200250,
        Tx_A_100200 = input$userVs * prop_onART_100200,
        Tx_A_50100 = input$userVs * prop_onART_50100,
        Tx_A_50 = input$userVs * prop_onART_50,

        Ltfu_500 = (input$userLtfu) * prop_preART_500,
        Ltfu_350500 = (input$userLtfu) * prop_preART_350500,
        Ltfu_250350 = (input$userLtfu) * prop_preART_250350,
        Ltfu_200250 = (input$userLtfu) * prop_preART_200250,
        Ltfu_100200 = (input$userLtfu) * prop_preART_100200,
        Ltfu_50100 = (input$userLtfu) * prop_preART_50100,
        Ltfu_50 = (input$userLtfu) * prop_preART_50,
        
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

    output$optParTable_Rho <- renderTable({
        theData <- seq(from = input$userOptRho_Range[1],to = input$userOptRho_Range[2],length.out = input$userOptRho_LengthOf)
        tbl <- matrix(theData,nrow=1,ncol=input$userOptRho_LengthOf)
        rownames(tbl) <- "Values:"
        colnames(tbl) <- paste("p",seq(1,input$userOptRho_LengthOf,1),sep='')
        return(tbl)
    },digits = 3)

    output$optParTable_Epsilon <- renderTable({
        theData <- seq(from = input$userOptEpsilon_Range[1],to = input$userOptEpsilon_Range[2],length.out = input$userOptEpsilon_LengthOf)
        tbl <- matrix(theData,nrow=1,ncol=input$userOptEpsilon_LengthOf)
        rownames(tbl) <- "Values:"
        colnames(tbl) <- paste("p",seq(1,input$userOptEpsilon_LengthOf,1),sep='')
        return(tbl)
    },digits = 3)

    output$optParTable_Kappa <- renderTable({
        theData <- seq(from = input$userOptKappa_Range[1],to = input$userOptKappa_Range[2],length.out = input$userOptKappa_LengthOf)
        tbl <- matrix(theData,nrow=1,ncol=input$userOptKappa_LengthOf)
        rownames(tbl) <- "Values:"
        colnames(tbl) <- paste("p",seq(1,input$userOptKappa_LengthOf,1),sep='')
        return(tbl)
    },digits = 3)

    output$optParTable_Gamma <- renderTable({
        theData <- seq(from = input$userOptGamma_Range[1],to = input$userOptGamma_Range[2],length.out = input$userOptGamma_LengthOf)
        tbl <- matrix(theData,nrow=1,ncol=input$userOptGamma_LengthOf)
        rownames(tbl) <- "Values:"
        colnames(tbl) <- paste("p",seq(1,input$userOptGamma_LengthOf,1),sep='')
        return(tbl)
    },digits = 3)

    output$optParTable_Sigma <- renderTable({
        theData <- seq(from = input$userOptSigma_Range[1],to = input$userOptSigma_Range[2],length.out = input$userOptSigma_LengthOf)
        tbl <- matrix(theData,nrow=1,ncol=input$userOptSigma_LengthOf)
        rownames(tbl) <- "Values:"
        colnames(tbl) <- paste("p",seq(1,input$userOptSigma_LengthOf,1),sep='')
        return(tbl)
    },digits = 3)

    output$optParTable_Omega <- renderTable({
        theData <- seq(from = input$userOptOmega_Range[1],to = input$userOptOmega_Range[2],length.out = input$userOptOmega_LengthOf)
        tbl <- matrix(theData,nrow=1,ncol=input$userOptOmega_LengthOf)
        rownames(tbl) <- "Values:"
        colnames(tbl) <- paste("p",seq(1,input$userOptOmega_LengthOf,1),sep='')
        return(tbl)
    },digits = 3)

    output$optIterationTable <- renderTable({
        ParInput <- expand.grid(
            Rho = seq(from = input$userOptRho_Range[1],to = input$userOptRho_Range[2],length.out = input$userOptRho_LengthOf),
            Epsilon = seq(from = input$userOptEpsilon_Range[1],to = input$userOptEpsilon_Range[2],length.out = input$userOptEpsilon_LengthOf),
            Kappa = seq(from = input$userOptKappa_Range[1],to = input$userOptKappa_Range[2],length.out = input$userOptKappa_LengthOf),
            Gamma = seq(from = input$userOptGamma_Range[1],to = input$userOptGamma_Range[2],length.out = input$userOptGamma_LengthOf),
            Sigma = seq(from = input$userOptSigma_Range[1],to = input$userOptSigma_Range[2],length.out = input$userOptSigma_LengthOf),
            Omega = seq(from = input$userOptOmega_Range[1],to = input$userOptOmega_Range[2],length.out = input$userOptOmega_LengthOf)
        )
        tbl <- matrix(0,nrow=2,ncol=1)
        tbl[1,] <- dim(ParInput)[1]
        tbl[2,] <- (dim(ParInput)[1] * 0.35) / 60
        colnames(tbl) <- "Value"
        rownames(tbl) <- c("Number of iterations:","Estimated completion time (min):")
        return(tbl)
    })

    observeEvent(input$optimiseInput, {

        # Parameter Input Values
        ParInput <- expand.grid(
            Rho = seq(from = input$userOptRho_Range[1],to = input$userOptRho_Range[2],length.out = input$userOptRho_LengthOf),
            Epsilon = seq(from = input$userOptEpsilon_Range[1],to = input$userOptEpsilon_Range[2],length.out = input$userOptEpsilon_LengthOf),
            Kappa = seq(from = input$userOptKappa_Range[1],to = input$userOptKappa_Range[2],length.out = input$userOptKappa_LengthOf),
            Gamma = seq(from = input$userOptGamma_Range[1],to = input$userOptGamma_Range[2],length.out = input$userOptGamma_LengthOf),
            Sigma = seq(from = input$userOptSigma_Range[1],to = input$userOptSigma_Range[2],length.out = input$userOptSigma_LengthOf),
            Omega = seq(from = input$userOptOmega_Range[1],to = input$userOptOmega_Range[2],length.out = input$userOptOmega_LengthOf)
        )

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
            theOut <- data.frame(ode(times=Time, y=Initial(), func=ComplexCascade, parms=OptPar))
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
            theOut <- mutate(theOut,DALY = (((UnDx_500 + Dx_500 + Care_500 + PreLtfu_500 + Tx_Na_500 + Ltfu_500 + UnDx_350500 + Dx_350500 + Care_350500 + PreLtfu_350500 + Tx_Na_350500 + Ltfu_350500) * 0.053) +  # >350, no ART
                                      ((UnDx_250350 + Dx_250350 + Care_250350 + PreLtfu_250350 + Tx_Na_250350 + Ltfu_250350 + UnDx_200250 + Dx_200250 + Care_200250 + PreLtfu_200250 + Tx_Na_200250 + Ltfu_200250) * 0.221) +  # 200-350, no ART
                                      ((UnDx_100200 + Dx_100200 + Care_100200 + PreLtfu_100200 + Tx_Na_100200 + Ltfu_100200 + UnDx_50100 + Dx_50100 + Care_50100 + PreLtfu_50100 + Tx_Na_50100 + Ltfu_50100 + UnDx_50 + Dx_50 + Care_50 + PreLtfu_50 + Tx_Na_50 + Ltfu_50) * 0.547) + # <200, no ART
                                      ((Tx_A_500 + Tx_A_350500 + Tx_A_250350 + Tx_A_200250 + Tx_A_100200 + Tx_A_50100 + Tx_A_50) * 0.053))) # on ART and virally suppressed
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

            updateButton(session,"optFinished",label="OPTIMISATION RUNNING",style="warning",icon="")
            
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
                ResultImpact[i] <- Calc_DALYsAverted(theList[[i]],theBaselineDALY)
                ResultCost[i] <- Calc_Cost(theList[[i]])
                ResultPar_Rho[i] <- ParInput[i,]$Rho
                ResultPar_Epsilon[i] <- ParInput[i,]$Epsilon
                ResultPar_Kappa[i] <- ParInput[i,]$Kappa
                ResultPar_Gamma[i] <- ParInput[i,]$Gamma
                ResultPar_Sigma[i] <- ParInput[i,]$Sigma
                ResultPar_Omega[i] <- ParInput[i,]$Omega
            }

            ResultNames <- paste("p",seq(1,dim(ParInput)[1],1),sep='')

            Result <<- data.frame(ResultNames,ResultImpact,ResultCost,ResultPar_Rho,ResultPar_Epsilon,ResultPar_Kappa,ResultPar_Gamma,ResultPar_Sigma,ResultPar_Omega)
            colnames(Result) <<- c("Names","Impact","Cost","Rho","Epsilon","Kappa","Gamma","Sigma","Omega")

            print(Result)
            setProgress(value = 1, message = paste("Finished. Time =",round(proc.time()[[1]] - Start.Time,0),"sec"))
            updateButton(session,"optFinished",label=" OPTIMISATION COMPLETE",style="success",icon=icon("check"))
        })
    })

    # Reactive ranges for plotOpt
    plotOpt.ranges <- reactiveValues(x = NULL, y = NULL)

    output$plotOpt <- renderPlot({
        input$optimiseInput
        ggplot(Result,aes(x=Impact,y=Cost)) + 
        geom_point(aes(color=Names),size=5) + 
        theme_classic() + 
        theme(legend.position="none") + 
        theme(axis.text.x=element_text(size=18)) + 
        theme(axis.text.y=element_text(size=18)) + 
        theme(axis.title=element_text(size=20)) + 
        xlab("Impact (DALYs Averted)") + 
        ylab("Cost (2013 USD)") + 
        coord_cartesian(xlim = plotOpt.ranges$x, ylim = plotOpt.ranges$y)
        },
        height=400,
        width=700
    )

    # When a double-click happens, check if there's a brush on the plot.
    # If so, zoom to the brush bounds; if not, reset the zoom.
    observeEvent(input$plotOpt_dblclick, {
        brush <- input$plotOpt_brush
        if (!is.null(brush)) {
            plotOpt.ranges$x <- c(brush$xmin, brush$xmax)
            plotOpt.ranges$y <- c(brush$ymin, brush$ymax)
        } else {
            plotOpt.ranges$x <- NULL
            plotOpt.ranges$y <- NULL
            }
        }
    )

    output$optTable <- DT::renderDataTable({
        return(Result)
        },
        options=list(pageLength=25)
    )

    output$optTableBrushed <- DT::renderDataTable({
        return(brushedPoints(df = Result,brush = input$plotOpt_brush))
        },
        options=list(pageLength=25)
    )

    # Saving input values from setup tab.
    saveCascadeData <- function(data) {
        # Grab the Google Sheet
        sheet <- gs_title("WHO-Cascade-Data")
        # Add the data as a new row
        gs_add_row(sheet, input = data)
    }

    # Google Sheet API Interface

    locateSheet <- function() {
        return(gs_title("SpectrumIncidenceEstimates"))
    }

    getIncidenceData <- function(theTable) {
        return(gs_read(theTable,ws="NewInfections"))
    }

    getCD4Data <- function(theTable) {
        return(gs_read(theTable,ws="CD4-Distribution"))
    }

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
            output$warningCD4Text <- renderText({return(paste(input$userCountry,"CD4 data loaded."))})
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