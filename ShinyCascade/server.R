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
        Nu_1 = 0.2139008,
        Nu_2 = 0.3379898,
        Nu_3 = 0.2744363,
        Rho = input$rho,
        Gamma = input$gamma,
        Theta = input$theta,
        Omega = input$omega,
        Delta_1 = 1.1491019,
        Delta_2 = 2.5468165,
        Alpha_1 = 0.0043812,
        Alpha_2 = 0.0179791,
        Alpha_3 = 0.0664348,
        Alpha_4 = 0.1289688,
        Tau_1 = 0.0041621,
        Tau_2 = 0.0170798,
        Tau_3 = 0.0631120,
        Tau_4 = 0.1225184,
        Mu = 0.0374,
        Epsilon = input$epsilon,
        Dx_unitCost = 2,
        Care_unitCost = 2,
        Tx_unitCost = 2,
        Retention_unitCost = 2
    )})

    output$parameterTable <- renderTable({
        theP <- Parameters()
        theParameters <- c(theP[4],theP[19],theP[5],theP[6],theP[7],theP[1],theP[2],theP[3],theP[8],theP[9],theP[10],theP[11],theP[12],theP[13],theP[14],theP[15],theP[16],theP[17],theP[18])
        ParameterNames <- c("Rho","Epsilon","Gamma","Theta","Omega","Nu_1","Nu_2","Nu_3","Delta_1","Delta_2","Alpha_1","Alpha_2","Alpha_3","Alpha_4","Tau_1","Tau_2","Tau_3","Tau_4","Mu")
        rows <- length(ParameterNames)
        tbl <- matrix(theParameters,rows,ncol=2)
        tbl[,1] <- ParameterNames
        colnames(tbl) <- c("Parameter","Value")
        return(tbl)
    })

    Initial <- reactive({c(
        UnDx_500 = (input$userPLHIV - input$userDx) * 0.58,
        UnDx_350500 = (input$userPLHIV - input$userDx) * 0.23,
        UnDx_200350 = (input$userPLHIV - input$userDx) * 0.16,
        UnDx_200 = (input$userPLHIV - input$userDx) * 0.03,

        Dx_500 = (input$userDx - input$userCare - input$userLtfu) * 0.58,
        Dx_350500 = (input$userDx - input$userCare - input$userLtfu) * 0.23,
        Dx_200350 = (input$userDx - input$userCare - input$userLtfu) * 0.16,
        Dx_200 = (input$userDx - input$userCare - input$userLtfu) * 0.03,

        Care_500 = (input$userCare - input$userTx) * 0.58,
        Care_350500 = (input$userCare - input$userTx) * 0.23,
        Care_200350 = (input$userCare - input$userTx) * 0.16,
        Care_200 = (input$userCare - input$userTx) * 0.03,

        Tx_500 = (input$userTx - input$userVs) * 0.58,
        Tx_350500 = (input$userTx - input$userVs) * 0.23,
        Tx_200350 = (input$userTx - input$userVs) * 0.16,
        Tx_200 = (input$userTx - input$userVs) * 0.03,

        Vs_500 = (input$userVs) * 0.58,
        Vs_350500 = (input$userVs) * 0.23,
        Vs_200350 = (input$userVs) * 0.16,
        Vs_200 = (input$userVs) * 0.03,

        Ltfu_500 = (input$userLtfu) * 0.58,
        Ltfu_350500 = (input$userLtfu) * 0.23,
        Ltfu_200350 = (input$userLtfu) * 0.16,
        Ltfu_200 = (input$userLtfu) * 0.03,

        NewInf = 0,

        HivMortality = 0,

        NaturalMortality = 0,

        Dx_Cost = 0,
        Care_Cost = 0,
        Tx_Cost = 0,
        Retention_Cost = 0
    )})

    observeEvent(input$demoInput, {
        if(input$userPLHIV == 0 || is.na(input$userPLHIV)) {
            randPLHIV <- round(runif(1,1e+6,1e+7),0)
            newDx <- round(randPLHIV * runif(1,0.5,0.7),0)
            newCare <- round(newDx * 0.7,0)
            newTx <- round(newCare * 0.6,0)
            newVs <- round(newTx * 0.8923,0)
            newLtfu <- round(newDx * 0.1,0)

            updateNumericInput(session,"userPLHIV",value=randPLHIV)
            updateNumericInput(session,"userDx",value=newDx)
            updateNumericInput(session,"userCare",value=newCare)
            updateNumericInput(session,"userTx",value=newTx)
            updateNumericInput(session,"userVs",value=newVs)
            updateNumericInput(session,"userLtfu",value=newLtfu)
        } else {
            newDx <- round(input$userPLHIV * runif(1,0.5,0.7),0)
            newCare <- round(newDx * 0.7,0)
            newTx <- round(newCare * 0.6,0)
            newVs <- round(newTx * 0.8923,0)
            newLtfu <- round(newDx * 0.1,0)

            updateNumericInput(session,"userDx",value=newDx)
            updateNumericInput(session,"userCare",value=newCare)
            updateNumericInput(session,"userTx",value=newTx)
            updateNumericInput(session,"userVs",value=newVs)
            updateNumericInput(session,"userLtfu",value=newLtfu)
        }
    })

    out <- reactive({

        Time <- seq(0,5,0.02)

        # Ability to turn off HIV incidence in the model.
        if(input$incidenceInput == TRUE) {
            theInitial <- Initial()
            Numerator <- NewInfections
            Denominator <- as.double((((theInitial[1] + theInitial[5] + theInitial[9] + theInitial[13] + theInitial[21]) * 1.35) + ((theInitial[2] + theInitial[6] + theInitial[10] + theInitial[14] + theInitial[22]) * 1) + ((theInitial[3] + theInitial[7] + theInitial[11] + theInitial[15] + theInitial[23]) * 1.64) + ((theInitial[4] + theInitial[8] + theInitial[12] + theInitial[16] + theInitial[24]) * 5.17) + ((theInitial[17] + theInitial[18] + theInitial[19] + theInitial[20]) * 0.1)))
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
        theOut <- mutate(theOut,N = UnDx_500 + UnDx_350500 + UnDx_200350 + UnDx_200 + Dx_500 + Dx_350500 + Dx_200350 + Dx_200 + Care_500 + Care_350500 + Care_200350 + Care_200 + Tx_500 + Tx_350500 + Tx_200350 + Tx_200 + Vs_500 + Vs_350500 + Vs_200350 + Vs_200 + Ltfu_500 + Ltfu_350500 + Ltfu_200350 + Ltfu_200)
        theOut <- mutate(theOut,ART = (Tx_500 + Tx_350500 + Tx_200350 + Tx_200 + Vs_500 + Vs_350500 + Vs_200350 + Vs_200) / N)
        theOut <- mutate(theOut,UnDx = (UnDx_500 + UnDx_350500 + UnDx_200350 + UnDx_200) / N)
        theOut <- mutate(theOut,Dx = (Dx_500 + Dx_350500 + Dx_200350 + Dx_200) / N)
        theOut <- mutate(theOut,Care = (Care_500 + Care_350500 + Care_200350 + Care_200) / N)
        theOut <- mutate(theOut,Tx = (Tx_500 + Tx_350500 + Tx_200350 + Tx_200) / N)
        theOut <- mutate(theOut,Vs = (Vs_500 + Vs_350500 + Vs_200350 + Vs_200) / N)
        theOut <- mutate(theOut,Ltfu = (Ltfu_500 + Ltfu_350500 + Ltfu_200350 + Ltfu_200) / N)
        theOut <- mutate(theOut,NaturalMortalityProp = NaturalMortality / N)
        theOut <- mutate(theOut,HivMortalityProp = HivMortality / N)
        theOut <- mutate(theOut,NewInfProp = NewInf / N)

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

        c <- ggplot(out,aes(x=time,y=Tx)) +
        geom_line() +
        theme(axis.text.x=element_text(size=18)) +
        theme(axis.text.y=element_text(size=18)) +
        theme(axis.title=element_text(size=20)) +
        xlab("Year") +
        theme_classic()

        d <- ggplot(out,aes(x=time,y=Vs)) +
        geom_line() +
        theme(axis.text.x=element_text(size=18)) +
        theme(axis.text.y=element_text(size=18)) +
        theme(axis.title=element_text(size=20)) +
        xlab("Year") +
        theme_classic()

        e <- ggplot(out,aes(x=time,y=Ltfu)) +
        geom_line() +
        theme(axis.text.x=element_text(size=18)) +
        theme(axis.text.y=element_text(size=18)) +
        theme(axis.title=element_text(size=20)) +
        xlab("Year") +
        theme_classic()

        f <- ggplot(out,aes(x=time,y=N)) +
        geom_line() +
        theme(axis.text.x=element_text(size=18)) +
        theme(axis.text.y=element_text(size=18)) +
        theme(axis.title=element_text(size=20)) +
        xlab("Year") +
        theme_classic()

        g <- ggplot(out,aes(x=time,y=NewInf)) +
        geom_line() +
        theme(axis.text.x=element_text(size=18)) +
        theme(axis.text.y=element_text(size=18)) +
        theme(axis.title=element_text(size=20)) +
        xlab("Year") +
        theme_classic()

        h <- ggplot(out,aes(x=time,y=NaturalMortalityProp)) +
        geom_line() +
        theme(axis.text.x=element_text(size=18)) +
        theme(axis.text.y=element_text(size=18)) +
        theme(axis.title=element_text(size=20)) +
        xlab("Year") +
        theme_classic()

        i <- ggplot(out,aes(x=time,y=HivMortalityProp)) +
        geom_line() +
        theme(axis.text.x=element_text(size=18)) +
        theme(axis.text.y=element_text(size=18)) +
        theme(axis.title=element_text(size=20)) +
        xlab("Year") +
        theme_classic()

        AllPlot <- grid.arrange(a,b,c,d,e,f,g,h,i,nrow=3,ncol=3)

        print(AllPlot)

        },
        height=900,
        width=900
    )

    output$plotCascade <- renderPlot({
        out <- out()

        t0_N = as.double(sum(filter(out,time == 0) %>% select(N)))
        t0_dx = as.double(sum(filter(out,time == 0) %>% select(c(Dx_500,Dx_350500,Dx_200350,Dx_200,Care_500,Care_350500,Care_200350,Care_200,Tx_500,Tx_350500,Tx_200350,Tx_200,Vs_500,Vs_350500,Vs_200350,Vs_200,Ltfu_500,Ltfu_350500,Ltfu_200350,Ltfu_200)))) / t0_N
        t0_cx = as.double(sum(filter(out,time == 0) %>% select(c(Care_500,Care_350500,Care_200350,Care_200,Tx_500,Tx_350500,Tx_200350,Tx_200,Vs_500,Vs_350500,Vs_200350,Vs_200)))) / t0_N
        t0_tx = as.double(sum(filter(out,time == 0) %>% select(c(Tx_500,Tx_350500,Tx_200350,Tx_200,Vs_500,Vs_350500,Vs_200350,Vs_200)))) / t0_N
        t0_vs = as.double(sum(filter(out,time == 0) %>% select(c(Vs_500,Vs_350500,Vs_200350,Vs_200)))) / t0_N
        t0_ltfu = as.double(sum(filter(out,time == 0) %>% select(c(Ltfu_500,Ltfu_350500,Ltfu_200350,Ltfu_200)))) / t0_N

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
        t5_dx = as.double(sum(filter(out,time == 5) %>% select(c(Dx_500,Dx_350500,Dx_200350,Dx_200,Care_500,Care_350500,Care_200350,Care_200,Tx_500,Tx_350500,Tx_200350,Tx_200,Vs_500,Vs_350500,Vs_200350,Vs_200,Ltfu_500,Ltfu_350500,Ltfu_200350,Ltfu_200)))) / t5_N
        t5_cx = as.double(sum(filter(out,time == 5) %>% select(c(Care_500,Care_350500,Care_200350,Care_200,Tx_500,Tx_350500,Tx_200350,Tx_200,Vs_500,Vs_350500,Vs_200350,Vs_200)))) / t5_N
        t5_tx = as.double(sum(filter(out,time == 5) %>% select(c(Tx_500,Tx_350500,Tx_200350,Tx_200,Vs_500,Vs_350500,Vs_200350,Vs_200)))) / t5_N
        t5_vs = as.double(sum(filter(out,time == 5) %>% select(c(Vs_500,Vs_350500,Vs_200350,Vs_200)))) / t5_N
        t5_ltfu = as.double(sum(filter(out,time == 5) %>% select(c(Ltfu_500,Ltfu_350500,Ltfu_200350,Ltfu_200)))) / t5_N

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

    output$plot909090 <- renderPlot({
        out <- out()
        PLHIV = as.double(sum(filter(out,time == 5) %>% select(N)))
        # dx / PLHIV
        dx = as.double(sum(filter(out,time == 5) %>% select(c(Dx_500,Dx_350500,Dx_200350,Dx_200,Care_500,Care_350500,Care_200350,Care_200,Tx_500,Tx_350500,Tx_200350,Tx_200,Vs_500,Vs_350500,Vs_200350,Vs_200,Ltfu_500,Ltfu_350500,Ltfu_200350,Ltfu_200))))
        # tx / dx
        tx = as.double(sum(filter(out,time == 5) %>% select(c(Tx_500,Tx_350500,Tx_200350,Tx_200,Vs_500,Vs_350500,Vs_200350,Vs_200))))
        # vs / tx
        vs = as.double(sum(filter(out,time == 5) %>% select(c(Vs_500,Vs_350500,Vs_200350,Vs_200))))

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
        dx = as.double(sum(filter(out,time == 5) %>% select(c(Dx_500,Dx_350500,Dx_200350,Dx_200,Care_500,Care_350500,Care_200350,Care_200,Tx_500,Tx_350500,Tx_200350,Tx_200,Vs_500,Vs_350500,Vs_200350,Vs_200,Ltfu_500,Ltfu_350500,Ltfu_200350,Ltfu_200))))
        # tx / dx
        tx = as.double(sum(filter(out,time == 5) %>% select(c(Tx_500,Tx_350500,Tx_200350,Tx_200,Vs_500,Vs_350500,Vs_200350,Vs_200))))
        # vs / tx
        vs = as.double(sum(filter(out,time == 5) %>% select(c(Vs_500,Vs_350500,Vs_200350,Vs_200))))

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
                Denominator <- as.double((((theInitial[1] + theInitial[5] + theInitial[9] + theInitial[13] + theInitial[21]) * 1.35) + ((theInitial[2] + theInitial[6] + theInitial[10] + theInitial[14] + theInitial[22]) * 1) + ((theInitial[3] + theInitial[7] + theInitial[11] + theInitial[15] + theInitial[23]) * 1.64) + ((theInitial[4] + theInitial[8] + theInitial[12] + theInitial[16] + theInitial[24]) * 5.17) + ((theInitial[17] + theInitial[18] + theInitial[19] + theInitial[20]) * 0.1)))
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
            theOut <- mutate(theOut,N = UnDx_500 + UnDx_350500 + UnDx_200350 + UnDx_200 + Dx_500 + Dx_350500 + Dx_200350 + Dx_200 + Care_500 + Care_350500 + Care_200350 + Care_200 + Tx_500 + Tx_350500 + Tx_200350 + Tx_200 + Vs_500 + Vs_350500 + Vs_200350 + Vs_200 + Ltfu_500 + Ltfu_350500 + Ltfu_200350 + Ltfu_200)
            theOut <- mutate(theOut,ART = (Tx_500 + Tx_350500 + Tx_200350 + Tx_200 + Vs_500 + Vs_350500 + Vs_200350 + Vs_200) / N)
            theOut <- mutate(theOut,UnDx = (UnDx_500 + UnDx_350500 + UnDx_200350 + UnDx_200) / N)
            theOut <- mutate(theOut,Dx = (Dx_500 + Dx_350500 + Dx_200350 + Dx_200) / N)
            theOut <- mutate(theOut,Care = (Care_500 + Care_350500 + Care_200350 + Care_200) / N)
            theOut <- mutate(theOut,Tx = (Tx_500 + Tx_350500 + Tx_200350 + Tx_200) / N)
            theOut <- mutate(theOut,Vs = (Vs_500 + Vs_350500 + Vs_200350 + Vs_200) / N)
            theOut <- mutate(theOut,Ltfu = (Ltfu_500 + Ltfu_350500 + Ltfu_200350 + Ltfu_200) / N)
            theOut <- mutate(theOut,NaturalMortalityProp = NaturalMortality / N)
            theOut <- mutate(theOut,HivMortalityProp = HivMortality / N)
            theOut <- mutate(theOut,NewInfProp = NewInf / N)

            PLHIV = as.double(sum(filter(theOut,time == 5) %>% select(N)))
            dx = as.double(sum(filter(theOut,time == 5) %>% select(c(Dx_500,Dx_350500,Dx_200350,Dx_200,Care_500,Care_350500,Care_200350,Care_200,Tx_500,Tx_350500,Tx_200350,Tx_200,Vs_500,Vs_350500,Vs_200350,Vs_200,Ltfu_500,Ltfu_350500,Ltfu_200350,Ltfu_200))))
            tx = as.double(sum(filter(theOut,time == 5) %>% select(c(Tx_500,Tx_350500,Tx_200350,Tx_200,Vs_500,Vs_350500,Vs_200350,Vs_200))))
            vs = as.double(sum(filter(theOut,time == 5) %>% select(c(Vs_500,Vs_350500,Vs_200350,Vs_200))))
            p_dx <- dx / PLHIV
            p_tx <- tx / dx
            p_vs <- vs / tx
            results <- c(p_dx,p_tx,p_vs)
            definition <- c("% Diagnosed","% On Treatment","% Suppressed")
            the909090 <- data.frame(definition,results)
            output <- sum((target - the909090$results)^2)
            return(output)
        }

        theResult <- optim(par = c(0,0,0,0), find909090, target = 0.9, lower = c(0.01,0.01,0.01,0.05), upper = c(5,5,5,5), method = 'L-BFGS-B')

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

        Parameters()
        out()
    })

    output$plotOptimised909090 <- renderPlot({
        # This should retrigger the plot to render... (I hope.)
        input$optimiseInput

        out <- out()
        PLHIV = as.double(sum(filter(out,time == 5) %>% select(N)))
        # dx / PLHIV
        dx = as.double(sum(filter(out,time == 5) %>% select(c(Dx_500,Dx_350500,Dx_200350,Dx_200,Care_500,Care_350500,Care_200350,Care_200,Tx_500,Tx_350500,Tx_200350,Tx_200,Vs_500,Vs_350500,Vs_200350,Vs_200,Ltfu_500,Ltfu_350500,Ltfu_200350,Ltfu_200))))
        # tx / dx
        tx = as.double(sum(filter(out,time == 5) %>% select(c(Tx_500,Tx_350500,Tx_200350,Tx_200,Vs_500,Vs_350500,Vs_200350,Vs_200))))
        # vs / tx
        vs = as.double(sum(filter(out,time == 5) %>% select(c(Vs_500,Vs_350500,Vs_200350,Vs_200))))

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
            output$warningText <- renderText({"Warning! NA value returned from Country. Using data from Kenya as default."})
            NewInfections <<- 56353
        } else {
            output$warningText <- renderText({""})
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
    })

    # Render Flow Diagram of Model.
    output$modelFlowImage <- renderImage({
        return(list(
            src='www/Model.png',
            contentType='image/png',
            alt='ModelFlowDiagram',
            height=444,
            width=1000))
    },
    deleteFile=FALSE)

}