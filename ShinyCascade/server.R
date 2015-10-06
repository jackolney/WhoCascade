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

source("TheModel.R")

function(input, output, session) {

  Parameters <- reactive({c(
        Nu_1 = 0.2139008,
        Nu_2 = 0.3379898,
        Nu_3 = 0.2744363,
        Rho = 0.5,
        Gamma = input$gamma,
        Theta = 2,
        Omega = 0.01,
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
        Epsilon = 0.5
    )})

    Initial <- reactive({c(
        UnDx_500 = (input$userPLHIV - input$userDx) * 0.58,
        UnDx_350500 = (input$userPLHIV - input$userDx) * 0.23,
        UnDx_200350 = (input$userPLHIV - input$userDx) * 0.16,
        UnDx_200 = (input$userPLHIV - input$userDx) * 0.03,

        Dx_500 = (input$userDx - input$userCare - input$userLtfu) * 0.58,
        Dx_350500 = (input$userDx - input$userCare - input$userLtfu) * 0.23,
        Dx_200350 = (input$userDx - input$userCare - input$userLtfu) * 0.16,
        Dx_200 = (input$userDx - input$userCare - input$userLtfu) * 0.03,

        Care_500 = (input$userCare - input$userTx - input$userVs) * 0.58,
        Care_350500 = (input$userCare - input$userTx - input$userVs) * 0.23,
        Care_200350 = (input$userCare - input$userTx - input$userVs) * 0.16,
        Care_200 = (input$userCare - input$userTx - input$userVs) * 0.03,

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

        NaturalMortality = 0
    )})

    observeEvent(input$demoInput, {
        if(input$userPLHIV == 0) {
            randPLHIV <- round(runif(1,100,1e+04),0)
            updateNumericInput(session,"userPLHIV",value=randPLHIV)
            updateNumericInput(session,"userDx",value=round(randPLHIV * 0.8,0))
            updateNumericInput(session,"userCare",value=round(randPLHIV * 0.6,0))
            updateNumericInput(session,"userTx",value=round(randPLHIV * 0.3,0))
            updateNumericInput(session,"userVs",value=round(randPLHIV * 0.25,0))
            updateNumericInput(session,"userLtfu",value=round(randPLHIV * 0.1,0))
        } else {
            updateNumericInput(session,"userDx",value=round(input$userPLHIV * 0.8,0))
            updateNumericInput(session,"userCare",value=round(input$userPLHIV * 0.6,0))
            updateNumericInput(session,"userTx",value=round(input$userPLHIV * 0.3,0))
            updateNumericInput(session,"userVs",value=round(input$userPLHIV * 0.25,0))
            updateNumericInput(session,"userLtfu",value=round(input$userPLHIV * 0.1,0))
        }
    })

    out <- reactive({

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
        print(paste("Beta:",Beta))

        # The Model #
        out <- data.frame(ode(times=Time, y=Initial(), func=ComplexCascade, parms=Parameters()))
        # --------- #

        # Post-simulation mutation (creation of columns) etc.
        out <- mutate(out,N = UnDx_500 + UnDx_350500 + UnDx_200350 + UnDx_200 + Dx_500 + Dx_350500 + Dx_200350 + Dx_200 + Care_500 + Care_350500 + Care_200350 + Care_200 + Tx_500 + Tx_350500 + Tx_200350 + Tx_200 + Vs_500 + Vs_350500 + Vs_200350 + Vs_200 + Ltfu_500 + Ltfu_350500 + Ltfu_200350 + Ltfu_200)
        out <- mutate(out,ART = (Tx_500 + Tx_350500 + Tx_200350 + Tx_200 + Vs_500 + Vs_350500 + Vs_200350 + Vs_200) / N)
        out <- mutate(out,UnDx = (UnDx_500 + UnDx_350500 + UnDx_200350 + UnDx_200) / N)
        out <- mutate(out,Dx = (Dx_500 + Dx_350500 + Dx_200350 + Dx_200) / N)
        out <- mutate(out,Care = (Care_500 + Care_350500 + Care_200350 + Care_200) / N)
        out <- mutate(out,Tx = (Tx_500 + Tx_350500 + Tx_200350 + Tx_200) / N)
        out <- mutate(out,Vs = (Vs_500 + Vs_350500 + Vs_200350 + Vs_200) / N)
        out <- mutate(out,Ltfu = (Ltfu_500 + Ltfu_350500 + Ltfu_200350 + Ltfu_200) / N)
        out <- mutate(out,NaturalMortalityProp = NaturalMortality / N)
        out <- mutate(out,HivMortalityProp = HivMortality / N)
        out <- mutate(out,NewInfProp = NewInf / N)

        return(out)
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
        height=700
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
        height=1000,
        width=1000
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
        o <- o + ggtitle("Care Cascade in 2015\n(denominator is PLHIV)")
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
        p <- p + ggtitle("Care Cascade in 2020\n(denominator is PLHIV)")
        p <- p + theme_classic()
        p <- p + theme(title=element_text(size=18))
        p <- p + theme(axis.title=element_blank())
        p <- p + theme(axis.text.x=element_text(size=15))
        p <- p + theme(axis.text.y=element_text(size=18))
        p <- p + theme(legend.position="none")

        print(grid.arrange(o,p,nrow=1,ncol=2))
        },
        height=400
    )

    output$plotCascadeThen <- renderPlot({
        out <- out()
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

        o <- ggplot(t5,aes(definition,t5_results))
        o <- o + geom_bar(aes(fill=definition),position='dodge',stat='identity')
        o <- o + scale_y_continuous(limits=c(0,1), breaks=seq(0,1,0.1),labels=percent)
        o <- o + scale_fill_manual(values=fill.coll)
        o <- o + ggtitle("Care Cascade in 2020\n(denominator is PLHIV)")
        o <- o + theme_classic()
        o <- o + theme(title=element_text(size=20))
        o <- o + theme(axis.title=element_blank())
        o <- o + theme(axis.text.x=element_text(size=18))
        o <- o + theme(axis.text.y=element_text(size=18))
        o <- o + theme(legend.position="none")
        print(o)
        },
        height=700
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
        height=400
    )

    output$outputTable <- DT::renderDataTable({
        return(out())

        },
        options=list(autoWidth=TRUE,pageLength=100)
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

}