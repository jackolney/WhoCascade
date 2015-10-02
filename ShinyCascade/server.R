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
        Beta = 0.0275837,
        Epsilon = 0.5
    )})

    Initial <- reactive({c(
        UnDx_500 = 1e+4 * ((input$userPLHIV - input$userDx) / input$userPLHIV) * 0.58,
        UnDx_350500 = 1e+4 * ((input$userPLHIV - input$userDx) / input$userPLHIV) * 0.23,
        UnDx_200350 = 1e+4 * ((input$userPLHIV - input$userDx) / input$userPLHIV) * 0.16,
        UnDx_200 = 1e+4 * ((input$userPLHIV - input$userDx) / input$userPLHIV) * 0.03,

        Dx_500 = 1e+4 * ((input$userDx - input$userCare - input$userLtfu) / input$userPLHIV) * 0.58,
        Dx_350500 = 1e+4 * ((input$userDx - input$userCare - input$userLtfu) / input$userPLHIV) * 0.23,
        Dx_200350 = 1e+4 * ((input$userDx - input$userCare - input$userLtfu) / input$userPLHIV) * 0.16,
        Dx_200 = 1e+4 * ((input$userDx - input$userCare - input$userLtfu) / input$userPLHIV) * 0.03,

        Care_500 = 1e+4 * ((input$userCare - input$userTx - input$userVs) / input$userPLHIV) * 0.58,
        Care_350500 = 1e+4 * ((input$userCare - input$userTx - input$userVs) / input$userPLHIV) * 0.23,
        Care_200350 = 1e+4 * ((input$userCare - input$userTx - input$userVs) / input$userPLHIV) * 0.16,
        Care_200 = 1e+4 * ((input$userCare - input$userTx - input$userVs) / input$userPLHIV) * 0.03,

        Tx_500 = 1e+4 * ((input$userTx - input$userVs) / input$userPLHIV) * 0.58,
        Tx_350500 = 1e+4 * ((input$userTx - input$userVs) / input$userPLHIV) * 0.23,
        Tx_200350 = 1e+4 * ((input$userTx - input$userVs) / input$userPLHIV) * 0.16,
        Tx_200 = 1e+4 * ((input$userTx - input$userVs) / input$userPLHIV) * 0.03,

        Vs_500 = 1e+4 * (input$userVs / input$userPLHIV) * 0.58,
        Vs_350500 = 1e+4 * (input$userVs / input$userPLHIV) * 0.23,
        Vs_200350 = 1e+4 * (input$userVs / input$userPLHIV) * 0.16,
        Vs_200 = 1e+4 * (input$userVs / input$userPLHIV) * 0.03,

        Ltfu_500 = 1e+4 * (input$userLtfu / input$userPLHIV) * 0.58,
        Ltfu_350500 = 1e+4 * (input$userLtfu / input$userPLHIV) * 0.23,
        Ltfu_200350 = 1e+4 * (input$userLtfu / input$userPLHIV) * 0.16,
        Ltfu_200 = 1e+4 * (input$userLtfu / input$userPLHIV) * 0.03,

        NewInf = 0,

        HivMortality = 0,

        NaturalMortality = 0
    )})

    observeEvent(input$demoInput, {
        randPLHIV <- round(runif(1,100,1e+04),0)
        updateNumericInput(session,"userPLHIV",value=randPLHIV)
        updateNumericInput(session,"userDx",value=round(randPLHIV * 0.8,0))
        updateNumericInput(session,"userCare",value=round(randPLHIV * 0.6,0))
        updateNumericInput(session,"userTx",value=round(randPLHIV * 0.3,0))
        updateNumericInput(session,"userVs",value=round(randPLHIV * 0.25,0))
        updateNumericInput(session,"userLtfu",value=round(randPLHIV * 0.1,0))
    })

    out <- reactive({
        Time <- seq(0,5,0.02)
        out <- data.frame(ode(times=Time, y=Initial(), func=ComplexCascade, parms=Parameters()))
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
        p <- ggplot(out(), aes_string(x="time",y=input$y)) + geom_line() + theme_classic()
        print(p)
        }, 
        height=700
    )

    output$plotTwo <- renderPlot({
        out <- out()
        a <- ggplot(out,aes(x=time,y=UnDx)) +
        geom_line() +
        theme_classic()

        b <- ggplot(out,aes(x=time,y=Dx)) +
        geom_line() +
        theme_classic()

        c <- ggplot(out,aes(x=time,y=Tx)) +
        geom_line() +
        theme_classic()

        d <- ggplot(out,aes(x=time,y=Vs)) +
        geom_line() +
        theme_classic()

        e <- ggplot(out,aes(x=time,y=Ltfu)) +
        geom_line() +
        theme_classic()

        f <- ggplot(out,aes(x=time,y=N)) +
        geom_line() +
        theme_classic()

        g <- ggplot(out,aes(x=time,y=NewInf)) +
        geom_line() +
        theme_classic()

        h <- ggplot(out,aes(x=time,y=NaturalMortalityProp)) +
        geom_line() +
        theme_classic()

        i <- ggplot(out,aes(x=time,y=HivMortalityProp)) +
        geom_line() +
        theme_classic()

        AllPlot <- grid.arrange(a,b,c,d,e,f,g,h,i,nrow=3,ncol=3)

        print(AllPlot)

        },
        height=700
    )

    output$plotCascadeNow <- renderPlot({
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
        o <- o + theme(axis.title=element_blank())
        o <- o + theme(axis.text.x=element_text(size=12))
        o <- o + theme(axis.text.y=element_text(size=10))
        o <- o + theme(legend.position="none")
        print(o)

        },
        height=700
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
        o <- o + theme(axis.title=element_blank())
        o <- o + theme(axis.text.x=element_text(size=12))
        o <- o + theme(axis.text.y=element_text(size=10))
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
        o <- o + theme(axis.title=element_blank())
        o <- o + theme(axis.text.x=element_text(size=12))
        o <- o + theme(axis.text.y=element_text(size=10))
        o <- o + theme(legend.position="none")
        print(o)
        },
        height=700
    )

    output$outputTable <- DT::renderDataTable({
        return(out())

        },
        options=list(autoWidth=TRUE,pageLength=100)
    )

    # Saving input values from setup tab.
    saveData <- function(data) {
        # Grab the Google Sheet
        sheet <- gs_title("WHO-Cascade-Data")
        # Add the data as a new row
        gs_add_row(sheet, input = data)
    }

    observeEvent(input$saveInput, {
        theResult <- c(input$userCountry,
            as.integer(input$userPLHIV),
            as.integer(input$userDx),
            as.integer(input$userCare),
            as.integer(input$userTx),
            as.integer(input$userVs),
            as.integer(input$userLtfu))
        print(theResult)
        saveData(theResult)
    })

    # Reset button stuff.
    observeEvent(input$resetInput, {
        shinyjs::reset("setup-panel")
    })

}