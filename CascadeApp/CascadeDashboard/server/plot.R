output$plotOne <- renderPlot({
    p <- ggplot(out(), aes_string(x="time",y=input$y)) +
    geom_line(size=2) +
    theme_classic() +
    # theme_economist() +
    theme(axis.text.x=element_text(size=18)) +
    theme(axis.text.y=element_text(size=18)) +
    theme(axis.title=element_text(size=18)) +
    xlab("Year") +
    scale_x_continuous(limits=c(0,5),breaks=seq(0,5,1),labels=seq(2015,2020,1))
    print(p)
    },
    height=500,
    width='auto'
)

output$plotTwo <- renderPlot({
    out <- out()
    a <- ggplot(out,aes(x=time,y=UnDx)) +
        geom_line() +
        theme(axis.text.x=element_text(size=18)) +
        theme(axis.text.y=element_text(size=18)) +
        theme(axis.title=element_text(size=18)) +
        xlab("Year") +
        theme_classic()

    b <- ggplot(out,aes(x=time,y=Dx)) +
        geom_line() +
        theme(axis.text.x=element_text(size=18)) +
        theme(axis.text.y=element_text(size=18)) +
        theme(axis.title=element_text(size=18)) +
        xlab("Year") +
        theme_classic()

    c <- ggplot(out,aes(x=time,y=Care)) +
        geom_line() +
        theme(axis.text.x=element_text(size=18)) +
        theme(axis.text.y=element_text(size=18)) +
        theme(axis.title=element_text(size=18)) +
        xlab("Year") +
        theme_classic()

    d <- ggplot(out,aes(x=time,y=PreLtfu)) +
        geom_line() +
        theme(axis.text.x=element_text(size=18)) +
        theme(axis.text.y=element_text(size=18)) +
        theme(axis.title=element_text(size=18)) +
        xlab("Year") +
        theme_classic()

    e <- ggplot(out,aes(x=time,y=Tx)) +
        geom_line() +
        theme(axis.text.x=element_text(size=18)) +
        theme(axis.text.y=element_text(size=18)) +
        theme(axis.title=element_text(size=18)) +
        xlab("Year") +
        theme_classic()

    f <- ggplot(out,aes(x=time,y=Vs)) +
        geom_line() +
        theme(axis.text.x=element_text(size=18)) +
        theme(axis.text.y=element_text(size=18)) +
        theme(axis.title=element_text(size=18)) +
        xlab("Year") +
        theme_classic()

    g <- ggplot(out,aes(x=time,y=Ltfu)) +
        geom_line() +
        theme(axis.text.x=element_text(size=18)) +
        theme(axis.text.y=element_text(size=18)) +
        theme(axis.title=element_text(size=18)) +
        xlab("Year") +
        theme_classic()

    h <- ggplot(out,aes(x=time,y=N)) +
        geom_line() +
        theme(axis.text.x=element_text(size=18)) +
        theme(axis.text.y=element_text(size=18)) +
        theme(axis.title=element_text(size=18)) +
        xlab("Year") +
        theme_classic()

    i <- ggplot(out,aes(x=time,y=NewInf)) +
        geom_line() +
        theme(axis.text.x=element_text(size=18)) +
        theme(axis.text.y=element_text(size=18)) +
        theme(axis.title=element_text(size=18)) +
        xlab("Year") +
        theme_classic()

    j <- ggplot(out,aes(x=time,y=TotalCost)) +
        geom_line() +
        theme(axis.text.x=element_text(size=18)) +
        theme(axis.text.y=element_text(size=18)) +
        theme(axis.title=element_text(size=18)) +
        xlab("Year") +
        theme_classic()

    k <- ggplot(out,aes(x=time,y=HivMortalityProp)) +
        geom_line() +
        theme(axis.text.x=element_text(size=18)) +
        theme(axis.text.y=element_text(size=18)) +
        theme(axis.title=element_text(size=18)) +
        xlab("Year") +
        theme_classic()

    l <- ggplot(out,aes(x=time,y=NaturalMortalityProp)) +
        geom_line() +
        theme(axis.text.x=element_text(size=18)) +
        theme(axis.text.y=element_text(size=18)) +
        theme(axis.title=element_text(size=18)) +
        xlab("Year") +
        theme_classic()

    AllPlot <- grid.arrange(a,b,c,d,e,f,g,h,i,j,k,l,nrow=4,ncol=3)

    print(AllPlot)

    },
    height='auto',
    width='auto'
)

GenerateCascadePlot <- function(highlight) {
        out <- out()
        t0_N = as.double(sum(filter(out,time == 0) %>% select(N)))
        t0_all = t0_N / t0_N
        t0_dx = as.double(sum(filter(out,time == 0) %>% select(c(Dx_500,Dx_350500,Dx_250350,Dx_200250,Dx_100200,Dx_50100,Dx_50,Care_500,Care_350500,Care_250350,Care_200250,Care_100200,Care_50100,Care_50,PreLtfu_500,PreLtfu_350500,PreLtfu_250350,PreLtfu_200250,PreLtfu_100200,PreLtfu_50100,PreLtfu_50,Tx_Na_500,Tx_Na_350500,Tx_Na_250350,Tx_Na_200250,Tx_Na_100200,Tx_Na_50100,Tx_Na_50,Tx_A_500,Tx_A_350500,Tx_A_250350,Tx_A_200250,Tx_A_100200,Tx_A_50100,Tx_A_50,Ltfu_500,Ltfu_350500,Ltfu_250350,Ltfu_200250,Ltfu_100200,Ltfu_50100,Ltfu_50)))) / t0_N
        t0_cx = as.double(sum(filter(out,time == 0) %>% select(c(Care_500,Care_350500,Care_250350,Care_200250,Care_100200,Care_50100,Care_50,Tx_A_500,Tx_A_350500,Tx_A_250350,Tx_A_200250,Tx_A_100200,Tx_A_50100,Tx_A_50,Tx_Na_500,Tx_Na_350500,Tx_Na_250350,Tx_Na_200250,Tx_Na_100200,Tx_Na_50100,Tx_Na_50)))) / t0_N
        t0_tx = as.double(sum(filter(out,time == 0) %>% select(c(Tx_A_500,Tx_A_350500,Tx_A_250350,Tx_A_200250,Tx_A_100200,Tx_A_50100,Tx_A_50,Tx_Na_500,Tx_Na_350500,Tx_Na_250350,Tx_Na_200250,Tx_Na_100200,Tx_Na_50100,Tx_Na_50)))) / t0_N
        t0_vs = as.double(sum(filter(out,time == 0) %>% select(c(Tx_A_500,Tx_A_350500,Tx_A_250350,Tx_A_200250,Tx_A_100200,Tx_A_50100,Tx_A_50)))) / t0_N
        t0_results <- c(t0_all,t0_dx,t0_cx,t0_tx,t0_vs)
        definition <- c("% PLHIV","% Diagnosed","% In Care","% Treatment","% Suppressed")
        t0 <- data.frame(definition,t0_results)
        t0$definition <- factor(t0$definition, levels=c("% PLHIV","% Diagnosed","% In Care","% Treatment","% Suppressed"))
        fill.coll <- rev(brewer.pal(9,"Blues")[3:8])
        fill.coll[highlight] <- "red"
        o <- ggplot(t0,aes(definition,t0_results))
        o <- o + geom_bar(aes(fill=definition),position='dodge',stat='identity')
        o <- o + scale_y_continuous(limits=c(0,1), breaks=seq(0,1,0.1),labels=percent, expand = c(0,0))
        o <- o + scale_fill_manual(values=fill.coll)
        o <- o + ggtitle("Care Cascade in 2015")
        o <- o + theme_classic()
        o <- o + theme(title=element_text(size=15))
        o <- o + theme(axis.title=element_blank())
        o <- o + theme(axis.text.x=element_text(size=11))
        o <- o + theme(axis.text.y=element_text(size=15))
        o <- o + theme(legend.position="none")
        o <- o + theme(plot.background=element_blank())
        o <- o + theme(panel.background=element_blank())
        return(o)
}

output$plotValidation_PLHIV <- renderPlot({print(GenerateCascadePlot(1))},
    height = 300, width = 'auto', bg = "transparent")

output$plotValidation_DIAG <- renderPlot({print(GenerateCascadePlot(2))},
    height = 300, width = 'auto', bg = "transparent")

output$plotValidation_CARE <- renderPlot({print(GenerateCascadePlot(3))},
    height = 300, width = 'auto', bg = "transparent")

output$plotValidation_ART <- renderPlot({print(GenerateCascadePlot(4))},
    height = 300, width = 'auto', bg = "transparent")

output$plotValidation_SUPP <- renderPlot({print(GenerateCascadePlot(5))},
    height = 300, width = 'auto', bg = "transparent")


output$plotValidation_LTFU <- renderPlot({
        out <- out()
        t0_N = as.double(sum(filter(out,time == 0) %>% select(N)))
        t0_ltfu = as.double(sum(filter(out,time == 0) %>% select(c(PreLtfu_500,PreLtfu_350500,PreLtfu_250350,PreLtfu_200250,PreLtfu_100200,PreLtfu_50100,PreLtfu_50,Ltfu_500,Ltfu_350500,Ltfu_250350,Ltfu_200250,Ltfu_100200,Ltfu_50100,Ltfu_50)))) / t0_N
        t0_results <- c(t0_ltfu)
        definition <- c("% LTFU")
        t0 <- data.frame(definition,t0_results)
        levels(t0$definition)
        t0$definition <- factor(t0$definition, levels=c("% LTFU"))

        fill.coll <- "red"

        o <- ggplot(t0,aes(definition,t0_results))
        o <- o + geom_bar(aes(fill=definition),position='dodge',stat='identity')
        o <- o + scale_y_continuous(limits=c(0,1), breaks=seq(0,1,0.1),labels=percent, expand = c(0,0))
        o <- o + scale_fill_manual(values=fill.coll)
        o <- o + ggtitle("Care Cascade in 2015")
        o <- o + theme_classic()
        o <- o + theme(title=element_text(size=15))
        o <- o + theme(axis.title=element_blank())
        o <- o + theme(axis.text.x=element_text(size=11))
        o <- o + theme(axis.text.y=element_text(size=15))
        o <- o + theme(legend.position="none")
        o <- o + theme(plot.background=element_blank())
        o <- o + theme(panel.background=element_blank())
        return(o)
        },
        height=300,
        width='auto',
        bg="transparent"
    )

output$plotCascade <- renderPlot({
    out <- out()

    t0_N = as.double(sum(filter(out,time == 0) %>% select(N)))
    t0_all = t0_N / t0_N
    t0_dx = as.double(sum(filter(out,time == 0) %>% select(c(Dx_500,Dx_350500,Dx_250350,Dx_200250,Dx_100200,Dx_50100,Dx_50,Care_500,Care_350500,Care_250350,Care_200250,Care_100200,Care_50100,Care_50,PreLtfu_500,PreLtfu_350500,PreLtfu_250350,PreLtfu_200250,PreLtfu_100200,PreLtfu_50100,PreLtfu_50,Tx_Na_500,Tx_Na_350500,Tx_Na_250350,Tx_Na_200250,Tx_Na_100200,Tx_Na_50100,Tx_Na_50,Tx_A_500,Tx_A_350500,Tx_A_250350,Tx_A_200250,Tx_A_100200,Tx_A_50100,Tx_A_50,Ltfu_500,Ltfu_350500,Ltfu_250350,Ltfu_200250,Ltfu_100200,Ltfu_50100,Ltfu_50)))) / t0_N
    t0_cx = as.double(sum(filter(out,time == 0) %>% select(c(Care_500,Care_350500,Care_250350,Care_200250,Care_100200,Care_50100,Care_50,Tx_A_500,Tx_A_350500,Tx_A_250350,Tx_A_200250,Tx_A_100200,Tx_A_50100,Tx_A_50,Tx_Na_500,Tx_Na_350500,Tx_Na_250350,Tx_Na_200250,Tx_Na_100200,Tx_Na_50100,Tx_Na_50)))) / t0_N
    t0_tx = as.double(sum(filter(out,time == 0) %>% select(c(Tx_A_500,Tx_A_350500,Tx_A_250350,Tx_A_200250,Tx_A_100200,Tx_A_50100,Tx_A_50,Tx_Na_500,Tx_Na_350500,Tx_Na_250350,Tx_Na_200250,Tx_Na_100200,Tx_Na_50100,Tx_Na_50)))) / t0_N
    t0_vs = as.double(sum(filter(out,time == 0) %>% select(c(Tx_A_500,Tx_A_350500,Tx_A_250350,Tx_A_200250,Tx_A_100200,Tx_A_50100,Tx_A_50)))) / t0_N

    t0_results <- c(t0_all,t0_dx,t0_cx,t0_tx,t0_vs)

    definition <- c("% PLHIV","% Diagnosed","% In Care","% Treatment","% Suppressed")
    t0 <- data.frame(definition,t0_results)

    levels(t0$definition)
    t0$definition <- factor(t0$definition, levels=c("% PLHIV","% Diagnosed","% In Care","% Treatment","% Suppressed"))

    fill.coll <- rev(brewer.pal(9,"Blues")[3:8])

    o <- ggplot(t0,aes(definition,t0_results))
    o <- o + geom_bar(aes(fill=definition),position='dodge',stat='identity')
    o <- o + scale_y_continuous(limits=c(0,1), breaks=seq(0,1,0.1),labels=percent, expand = c(0,0))
    o <- o + scale_fill_manual(values=fill.coll)
    o <- o + ggtitle("Care Cascade in 2015")
    o <- o + theme_classic()
    o <- o + theme(title=element_text(size=18))
    o <- o + theme(axis.title=element_blank())
    o <- o + theme(axis.text.x=element_text(size=15))
    o <- o + theme(axis.text.y=element_text(size=18))
    o <- o + theme(legend.position="none")

    t5_N = as.double(sum(filter(out,time == 5) %>% select(N)))
    t5_all = t5_N / t5_N
    t5_dx = as.double(sum(filter(out,time == 5) %>% select(c(Dx_500,Dx_350500,Dx_250350,Dx_200250,Dx_100200,Dx_50100,Dx_50,Care_500,Care_350500,Care_250350,Care_200250,Care_100200,Care_50100,Care_50,PreLtfu_500,PreLtfu_350500,PreLtfu_250350,PreLtfu_200250,PreLtfu_100200,PreLtfu_50100,PreLtfu_50,Tx_Na_500,Tx_Na_350500,Tx_Na_250350,Tx_Na_200250,Tx_Na_100200,Tx_Na_50100,Tx_Na_50,Tx_A_500,Tx_A_350500,Tx_A_250350,Tx_A_200250,Tx_A_100200,Tx_A_50100,Tx_A_50,Ltfu_500,Ltfu_350500,Ltfu_250350,Ltfu_200250,Ltfu_100200,Ltfu_50100,Ltfu_50)))) / t5_N
    t5_cx = as.double(sum(filter(out,time == 5) %>% select(c(Care_500,Care_350500,Care_250350,Care_200250,Care_100200,Care_50100,Care_50,Tx_A_500,Tx_A_350500,Tx_A_250350,Tx_A_200250,Tx_A_100200,Tx_A_50100,Tx_A_50,Tx_Na_500,Tx_Na_350500,Tx_Na_250350,Tx_Na_200250,Tx_Na_100200,Tx_Na_50100,Tx_Na_50)))) / t5_N
    t5_tx = as.double(sum(filter(out,time == 5) %>% select(c(Tx_A_500,Tx_A_350500,Tx_A_250350,Tx_A_200250,Tx_A_100200,Tx_A_50100,Tx_A_50,Tx_Na_500,Tx_Na_350500,Tx_Na_250350,Tx_Na_200250,Tx_Na_100200,Tx_Na_50100,Tx_Na_50)))) / t5_N
    t5_vs = as.double(sum(filter(out,time == 5) %>% select(c(Tx_A_500,Tx_A_350500,Tx_A_250350,Tx_A_200250,Tx_A_100200,Tx_A_50100,Tx_A_50)))) / t5_N

    t5_results <- c(t5_all,t5_dx,t5_cx,t5_tx,t5_vs)

    definition <- c("% PLHIV","% Diagnosed","% In Care","% Treatment","% Suppressed")
    t5 <- data.frame(definition,t5_results)

    levels(t5$definition)
    t5$definition <- factor(t5$definition, levels=c("% PLHIV","% Diagnosed","% In Care","% Treatment","% Suppressed"))

    fill.coll <- rev(brewer.pal(9,"Blues")[3:8])

    p <- ggplot(t5,aes(definition,t5_results))
    p <- p + geom_bar(aes(fill=definition),position='dodge',stat='identity')
    p <- p + scale_y_continuous(limits=c(0,1), breaks=seq(0,1,0.1),labels=percent, expand = c(0,0))
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
    width='auto'
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
                rep("Care",3),
                rep("Treatment",2),
                rep("Suppressed",1),
                rep("LTFU",2))

    t0 <- data.frame(State,tResult,tOrder)

    levels(t0$tOrder)
    t0$tOrder <- factor(t0$tOrder, levels=c("All","Diagnosed","Care","Treatment","Suppressed","LTFU"))

    levels(t0$State)
    t0$State <- factor(t0$State, levels=c("% Suppressed","% On Treatment (non-adherent)","% In Care","% Diagnosed","% Undiagnosed","% pre-ART LTFU","% LTFU"))

    cols <- brewer.pal(9,"Set1")
    power.col <- c(cols[3],cols[2],cols[4],cols[5],cols[1],cols[9],cols[8])

    o <- ggplot(t0,aes(x=tOrder,y=tResult,fill=State))
    o <- o + geom_bar(stat='identity')
    o <- o + scale_y_continuous(breaks=seq(0,1,0.1),labels=percent, expand = c(0,0))
    o <- o + scale_fill_manual(values=power.col)
    o <- o + ggtitle("Care Cascade in 2015")
    o <- o + theme_classic()
    o <- o + theme(title=element_text(size=18))
    o <- o + theme(axis.title=element_blank())
    o <- o + theme(axis.text.x=element_text(size=13))
    o <- o + theme(axis.text.y=element_text(size=15))
    o <- o + theme(legend.text=element_text(size=15))
    o <- o + theme(legend.title=element_text(size=15))
    o <- o + theme(legend.position="right")

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
                rep("Care",3),
                rep("Treatment",2),
                rep("Suppressed",1),
                rep("LTFU",2))

    t5 <- data.frame(State,tResult,tOrder)

    levels(t5$tOrder)
    t5$tOrder <- factor(t5$tOrder, levels=c("All","Diagnosed","Care","Treatment","Suppressed","LTFU"))

    levels(t5$State)
    t5$State <- factor(t5$State, levels=c("% Suppressed","% On Treatment (non-adherent)","% In Care","% Diagnosed","% Undiagnosed","% pre-ART LTFU","% LTFU"))

    power.col <- c(cols[3],cols[2],cols[4],cols[5],cols[1],cols[9],cols[8])

    p <- ggplot(t5,aes(x=tOrder,y=tResult,fill=State))
    p <- p + geom_bar(stat='identity')
    p <- p + scale_y_continuous(breaks=seq(0,1,0.1),labels=percent, expand = c(0,0))
    p <- p + scale_fill_manual(values=power.col)
    p <- p + ggtitle("Care Cascade in 2020")
    p <- p + theme_classic()
    p <- p + theme(title=element_text(size=18))
    p <- p + theme(axis.title=element_blank())
    p <- p + theme(axis.text.x=element_text(size=13))
    p <- p + theme(axis.text.y=element_text(size=15))
    p <- p + theme(legend.text=element_text(size=15))

    g_legend<-function(a.gplot) {
        tmp <- ggplot_gtable(ggplot_build(a.gplot))
        leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
        legend <- tmp$grobs[[leg]]
        return(legend)
    }

    mylegend <- g_legend(o)
    lwidth <- sum(mylegend$width)

    thePlot <- grid.arrange(
        arrangeGrob(
            o + theme(legend.position="none"),
            p + theme(legend.position="none"),
            ncol=2),
        mylegend,widths=grid::unit.c(unit(1, "npc") - lwidth, lwidth),nrow=1)

    return(thePlot)
},
height=400,
width='auto'
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

    red <- rgb(red = 223, green = 74, blue = 50, max = 255)
    yellow <- rgb(red = 245, green = 157, blue = 0, max = 255)
    green <- rgb(red = 0, green = 167, blue = 87, max = 255)
    fill.coll <- c(red,yellow,green)

    o <- ggplot(the909090,aes(definition,results))
    o <- o + geom_bar(aes(fill=definition),position='dodge',stat='identity')
    o <- o + scale_y_continuous(limits=c(0,1), breaks=seq(0,1,0.1),labels=percent, expand = c(0,0))
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
    width='auto'
)


output$plotNewInf <- renderPlot({
    p <- ggplot(out(), aes(x=time,y=NewInfProp)) +
        geom_line(size=2) +
        theme_classic() +
        theme(axis.text.x=element_text(size=18)) +
        theme(axis.text.y=element_text(size=18)) +
        theme(axis.title=element_text(size=18)) +
        xlab("Year") +
        ylab("# new infections / total infected population") +
        scale_x_continuous(limits=c(0,5),breaks=seq(0,5,1),labels=seq(2015,2020,1))
    print(p)
    },
    height=500,
    width='auto'
)

output$plotAidsDeaths <- renderPlot({
    p <- ggplot(out(), aes(x=time,y=HivMortalityProp)) +
        geom_line(size=2) +
        theme_classic() +
        theme(axis.text.x=element_text(size=18)) +
        theme(axis.text.y=element_text(size=18)) +
        theme(axis.title=element_text(size=18)) +
        xlab("Year") +
        ylab("# AIDS deaths / total infected population") +
        scale_x_continuous(limits=c(0,5),breaks=seq(0,5,1),labels=seq(2015,2020,1))
    print(p)
    },
    height=500,
    width='auto'
)


# Plot 1
output$plotOpt909090 <- renderPlot({
    input$optimiseInput

    Legend.Labels <- c()
    for(i in 1:length(levels(as.factor(Result_909090[[input$userStratPoint]])))) {
        Legend.Labels[i] <- round(as.double(levels(as.factor(Result_909090[[input$userStratPoint]]))[i]),2)
    }

    # Determining which interventions achieved 90-90-90
    theResult <- mutate(Result_909090, the909090 = 0)

    for(i in 1:dim(theResult)[1]) {
        if(theResult[i,1] >= 0.9) {
            if(theResult[i,2] >= 0.9) {
                if(theResult[i,3] >= 0.9) {
                    theResult$the909090[i] <- 1
                }
            }
        }
    }
    theStratPoint <<- input$userStratPoint
    ggplot(theResult,aes(x=VS,y=Cost)) +
    geom_point(aes(color=as.factor(get(theStratPoint)),size=as.factor(the909090))) +
    theme_classic() +
    scale_color_discrete(name=input$userStratPoint,labels = Legend.Labels) +
    scale_size_discrete(name="Achieves 90-90-90",range = c(3,6),labels = c("no","yes")) +
    guides(colour = guide_legend(override.aes = list(size=4))) +
    theme(legend.title=element_text(size=15)) +
    theme(legend.text=element_text(size=13)) +
    theme(axis.text.x=element_text(size=18)) +
    theme(axis.text.y=element_text(size=18)) +
    theme(axis.title=element_text(size=18)) +
    geom_vline(xintercept = 0.9^3) +
    xlab("Proportion achieving viral suppression by 2020") +
    ylab("Additional cost of care (2013 USD)") +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(labels = comma) +
    coord_cartesian(xlim = plotOpt_909090.ranges$x, ylim = plotOpt_909090.ranges$y)
    },
    height=400,
    width='auto'
)

# Plot 2
output$plotOptDALYs <- renderPlot({
    input$optimiseInput

    Legend.Labels <- c()
    for(i in 1:length(levels(as.factor(Result_DALYs[[input$userStratPoint]])))) {
        Legend.Labels[i] <- round(as.double(levels(as.factor(Result_DALYs[[input$userStratPoint]]))[i]),2)
    }
    theStratPoint <<- input$userStratPoint
    ggplot(Result_DALYs,aes(x=DALYs,y=Cost)) +
    geom_point(aes(color=as.factor(get(theStratPoint))),size=5) +
    theme_classic() +
    scale_color_discrete(name=input$userStratPoint,labels = Legend.Labels) +
    guides(colour = guide_legend(override.aes = list(size=4))) +
    theme(legend.title=element_text(size=15)) +
    theme(legend.text=element_text(size=13)) +
    theme(axis.text.x=element_text(size=18)) +
    theme(axis.text.y=element_text(size=18)) +
    theme(axis.title=element_text(size=18)) +
    xlab("DALYs Averted (between 2015 and 2020)") +
    ylab("Additional cost of care (2013 USD)") +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(labels = comma) +
    coord_cartesian(xlim = plotOpt_DALYs.ranges$x, ylim = plotOpt_DALYs.ranges$y)
    },
    height=400,
    width='auto'
)

# Plot 3
output$plotOptDALYs909090 <- renderPlot({
    input$optimiseInput

    Legend.Labels <- c()
    for(i in 1:length(levels(as.factor(Result_DALYs_909090[[input$userStratPoint]])))) {
        Legend.Labels[i] <- round(as.double(levels(as.factor(Result_DALYs_909090[[input$userStratPoint]]))[i]),2)
    }
    theStratPoint <<- input$userStratPoint
    ggplot(Result_DALYs_909090,aes(x=DALYs,y=Cost)) +
    geom_point(aes(color=as.factor(get(theStratPoint))),size=5) +
    theme_classic() +
    scale_color_discrete(name=input$userStratPoint,labels = Legend.Labels) +
    guides(colour = guide_legend(override.aes = list(size=4))) +
    theme(legend.title=element_text(size=15)) +
    theme(legend.text=element_text(size=13)) +
    theme(axis.text.x=element_text(size=18)) +
    theme(axis.text.y=element_text(size=18)) +
    theme(axis.title=element_text(size=18)) +
    xlab("DALYs Averted (between 2015 and 2020)") +
    ylab("Additional cost of care (2013 USD)") +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(labels = comma) +
    coord_cartesian(xlim = plotOpt_DALYs_909090.ranges$x, ylim = plotOpt_DALYs_909090.ranges$y)
    },
    height=400,
    width='auto'
)

##############
# WIZARD FUN #
##############

output$plotCascade_wizard <- renderPlot({
    out <- out()

    t5_N = as.double(sum(filter(out,time == 5) %>% select(N)))
    t5_all = t5_N / t5_N
    t5_dx = as.double(sum(filter(out,time == 5) %>% select(c(Dx_500,Dx_350500,Dx_250350,Dx_200250,Dx_100200,Dx_50100,Dx_50,Care_500,Care_350500,Care_250350,Care_200250,Care_100200,Care_50100,Care_50,PreLtfu_500,PreLtfu_350500,PreLtfu_250350,PreLtfu_200250,PreLtfu_100200,PreLtfu_50100,PreLtfu_50,Tx_Na_500,Tx_Na_350500,Tx_Na_250350,Tx_Na_200250,Tx_Na_100200,Tx_Na_50100,Tx_Na_50,Tx_A_500,Tx_A_350500,Tx_A_250350,Tx_A_200250,Tx_A_100200,Tx_A_50100,Tx_A_50,Ltfu_500,Ltfu_350500,Ltfu_250350,Ltfu_200250,Ltfu_100200,Ltfu_50100,Ltfu_50)))) / t5_N
    t5_cx = as.double(sum(filter(out,time == 5) %>% select(c(Care_500,Care_350500,Care_250350,Care_200250,Care_100200,Care_50100,Care_50,Tx_A_500,Tx_A_350500,Tx_A_250350,Tx_A_200250,Tx_A_100200,Tx_A_50100,Tx_A_50,Tx_Na_500,Tx_Na_350500,Tx_Na_250350,Tx_Na_200250,Tx_Na_100200,Tx_Na_50100,Tx_Na_50)))) / t5_N
    t5_tx = as.double(sum(filter(out,time == 5) %>% select(c(Tx_A_500,Tx_A_350500,Tx_A_250350,Tx_A_200250,Tx_A_100200,Tx_A_50100,Tx_A_50,Tx_Na_500,Tx_Na_350500,Tx_Na_250350,Tx_Na_200250,Tx_Na_100200,Tx_Na_50100,Tx_Na_50)))) / t5_N
    t5_vs = as.double(sum(filter(out,time == 5) %>% select(c(Tx_A_500,Tx_A_350500,Tx_A_250350,Tx_A_200250,Tx_A_100200,Tx_A_50100,Tx_A_50)))) / t5_N

    t5_results <- c(t5_all,t5_dx,t5_cx,t5_tx,t5_vs)

    definition <- c("% PLHIV","% Diagnosed","% In Care","% Treatment","% Suppressed")
    t5 <- data.frame(definition,t5_results)

    levels(t5$definition)
    t5$definition <- factor(t5$definition, levels = c("% PLHIV","% Diagnosed","% In Care","% Treatment","% Suppressed"))

    fill.coll <- rev(brewer.pal(9,"Blues")[3:8])

    p <- ggplot(t5,aes(definition,t5_results))
    p <- p + geom_bar(aes(fill = definition), position = 'dodge', stat = 'identity')
    p <- p + scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.1), labels = percent, expand = c(0,0))
    p <- p + scale_fill_manual(values = fill.coll)
    p <- p + ggtitle("Care Cascade in 2020")
    p <- p + theme_classic()
    p <- p + theme(title = element_text(size = 18))
    p <- p + theme(axis.title = element_blank())
    p <- p + theme(axis.text.x = element_text(size = 15))
    p <- p + theme(axis.text.y = element_text(size = 18))
    p <- p + theme(legend.position = "none")
    print(p)
    },
    height = 'auto',
    width = 'auto'
)

output$plotNewInf_wizard <- renderPlot({
    p <- ggplot(out(), aes(x = time,y = NewInf)) +
        geom_line(size = 2) +
        theme_classic() +
        theme(axis.text.x = element_text(size = 10)) +
        theme(axis.text.y = element_text(size = 10)) +
        theme(axis.title = element_text(size = 10)) +
        xlab("Year") +
        ylab("# new infections") +
        scale_x_continuous(limits = c(0,5), breaks = seq(0,5,1), labels = seq(2015,2020,1))
    print(p)
    },
    height = 240,
    width = 'auto'
)

output$plotAidsDeaths_wizard <- renderPlot({
    p <- ggplot(out(), aes(x = time,y = HivMortality)) +
        geom_line(size = 2) +
        theme_classic() +
        theme(axis.text.x = element_text(size = 10)) +
        theme(axis.text.y = element_text(size = 10)) +
        theme(axis.title = element_text(size = 10)) +
        xlab("Year") +
        ylab("# AIDS deaths") +
        scale_x_continuous(limits = c(0,5), breaks = seq(0,5,1), labels = seq(2015,2020,1))
    print(p)
    },
    height = 240,
    width = 'auto'
)
