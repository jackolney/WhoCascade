GenYourCascadePlot <- function(h) {
    t0 <- ExtractCascadeData(1)

    c.fill <- rev(brewer.pal(9, "Blues")[3:8])
    c.fill[h] <- "red"

    output$outPLHIV <- renderInfoBox({
        infoBox(
            title = "Mean number of PLHIV",
            value = scales::comma(round(t0[t0$def == "# PLHIV", "res"], 0)),
            subtitle = paste(scales::comma(round(t0[t0$def == "# PLHIV", "min"], 0)), "-", scales::comma(round(t0[t0$def == "# PLHIV", "max"], 0))),
            color = "blue",
            fill = TRUE,
            icon = icon("hashtag", lib = "font-awesome")
        )
    })

    output$outDIAG <- renderInfoBox({
        infoBox(
            title = "Mean number of PLHIV who have been diagnosed",
            value = scales::comma(round(t0[t0$def == "# Diagnosed", "res"], 0)),
            subtitle = paste(scales::comma(round(t0[t0$def == "# Diagnosed", "min"], 0)), "-", scales::comma(round(t0[t0$def == "# Diagnosed", "max"], 0))),
            color = "blue",
            fill = TRUE,
            icon = icon("hashtag", lib = "font-awesome")
        )
    })

    output$outCARE <- renderInfoBox({
        infoBox(
            title = "Mean number of PLHIV in HIV care (including ART)",
            value = scales::comma(round(t0[t0$def == "# In Care", "res"], 0)),
            subtitle = paste(scales::comma(round(t0[t0$def == "# In Care", "min"], 0)), "-", scales::comma(round(t0[t0$def == "# In Care", "max"], 0))),
            color = "blue",
            fill = TRUE,
            icon = icon("hashtag", lib = "font-awesome")
        )
    })

    output$outART <- renderInfoBox({
        infoBox(
            title = "Mean number of PLHIV in HIV care and on ART",
            value = scales::comma(round(t0[t0$def == "# Treatment", "res"], 0)),
            subtitle = paste(scales::comma(round(t0[t0$def == "# Treatment", "min"], 0)), "-", scales::comma(round(t0[t0$def == "# Treatment", "max"], 0))),
            color = "blue",
            fill = TRUE,
            icon = icon("hashtag", lib = "font-awesome")
        )
    })

    output$outSUPP <- renderInfoBox({
        infoBox(
            title = "Mean number of PLHIV in HIV care, on ART and virally suppressed",
            value = scales::comma(round(t0[t0$def == "# Suppressed", "res"], 0)),
            subtitle = paste(scales::comma(round(t0[t0$def == "# Suppressed", "min"], 0)), "-", scales::comma(round(t0[t0$def == "# Suppressed", "max"], 0))),
            color = "blue",
            fill = TRUE,
            icon = icon("hashtag", lib = "font-awesome")
        )
    })

    ggOut <- ggplot(t0, aes(x = def, y = res))
    ggOut <- ggOut + geom_bar(aes(fill = def), position = 'dodge', stat = 'identity')
    ggOut <- ggOut + geom_errorbar(mapping = aes(x = def, ymin = min, ymax = max), width = 0.2, size = 1)
    ggOut <- ggOut + scale_y_continuous(labels = scales::comma, expand = c(0, 0), breaks = scales::pretty_breaks(n = 5))
    ggOut <- ggOut + expand_limits(y = round(max(t0$max), digits = -4) + 1e5)
    ggOut <- ggOut + scale_fill_manual(values = c.fill)
    ggOut <- ggOut + ggtitle("Care Cascade in 2015")
    ggOut <- ggOut + theme_classic()
    ggOut <- ggOut + theme(plot.title = element_text(hjust = 0.5))
    ggOut <- ggOut + theme(title = element_text(size = 18))
    ggOut <- ggOut + theme(axis.title = element_blank())
    ggOut <- ggOut + theme(axis.text.x = element_text(size = 17))
    ggOut <- ggOut + theme(axis.text.y = element_text(size = 18))
    ggOut <- ggOut + theme(legend.position = "none")
    ggOut <- ggOut + theme(plot.background = element_blank())
    ggOut <- ggOut + theme(panel.background = element_blank())
    ggOut <- ggOut + theme(axis.line.y = element_line())
    ggOut <- ggOut + theme(text = element_text(family = figFont))
    ggOut
}

GenCascadePlot <- function() {
    t0 <- ExtractCascadeData(1)   # t0 = 1
    t5 <- ExtractCascadeData(251) # t5 = (5 / 0.02) + 1 [t0]

    c.fill <- rev(brewer.pal(9,"Blues")[3:8])

    t0$year <- 2015
    t5$year <- 2020
    out <- rbind(t0, t5)

    ggOne <- ggplot(out, aes(x = def, y = res))
    ggOne <- ggOne + geom_bar(aes(fill = as.factor(year)), position = 'dodge', stat = 'identity')
    ggOne <- ggOne + geom_errorbar(mapping = aes(x = def, ymin = min, ymax = max, fill = as.factor(year)), position = position_dodge(width = 0.9), stat = "identity", width = 0.2, size = 1)
    ggOne <- ggOne + scale_y_continuous(labels = scales::comma, expand = c(0, 0), breaks = scales::pretty_breaks(n = 5))
    ggOne <- ggOne + scale_fill_manual(values = c(c.fill[2],c.fill[5]), guide = guide_legend(title = "Year"))
    ggOne <- ggOne + theme_classic()
    ggOne <- ggOne + theme(title = element_text(size = 18))
    ggOne <- ggOne + theme(axis.title = element_blank())
    ggOne <- ggOne + theme(axis.text.x = element_text(size = 17))
    ggOne <- ggOne + theme(axis.text.y = element_text(size = 18))
    ggOne <- ggOne + theme(axis.ticks.x = element_blank())
    ggOne <- ggOne + theme(legend.position = "right")
    ggOne <- ggOne + theme(legend.title = element_text(size = 17))
    ggOne <- ggOne + theme(legend.text = element_text(size = 15))
    ggOne <- ggOne + theme(plot.background = element_blank())
    ggOne <- ggOne + theme(panel.background = element_blank())
    ggOne <- ggOne + theme(axis.line.y = element_line())
    ggOne <- ggOne + theme(text = element_text(family = figFont))
    ggOne <- ggOne + expand_limits(y = round(max(out$max), digits = -4) + 1e5)
    ggOne
}

GrabLegend <- function(a.ggplot) {
    tmp <- ggplot_gtable(ggplot_build(a.ggplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
}

GenPowersCascadePlot <- function() {
    t0 <- ExtractPowersCascadeData(1)
    t5 <- ExtractPowersCascadeData(251) # t5 = (5 / 0.02) + 1 [t0]

    cols <- brewer.pal(9,"Set1")
    p.col <- c(cols[3], cols[2], cols[4], cols[5], cols[1], cols[9], cols[8])

    ggOne <- ggplot(t0, aes(x = order, y = res, fill = state))
    ggOne <- ggOne + geom_bar(stat = 'identity')
    ggOne <- ggOne + scale_y_continuous(labels = scales::comma, expand = c(0, 0), breaks = scales::pretty_breaks(n = 5))
    ggOne <- ggOne + scale_fill_manual(values = p.col)
    ggOne <- ggOne + ggtitle("2015")
    ggOne <- ggOne + theme_classic()
    ggOne <- ggOne + theme(plot.title = element_text(hjust = 0.5))
    ggOne <- ggOne + theme(title = element_text(size = 18))
    ggOne <- ggOne + theme(axis.title = element_blank())
    ggOne <- ggOne + theme(axis.text.x = element_text(size = 13))
    ggOne <- ggOne + theme(axis.text.y = element_text(size = 15))
    ggOne <- ggOne + theme(legend.text = element_text(size = 13))
    ggOne <- ggOne + theme(legend.title = element_text(size = 15))
    ggOne <- ggOne + theme(legend.position = "right")
    ggOne <- ggOne + theme(plot.background = element_blank())
    ggOne <- ggOne + theme(panel.background = element_blank())

    cols <- brewer.pal(9,"Set1")
    p.col <- c(cols[3], cols[2], cols[4], cols[5], cols[1], cols[9], cols[8])

    ggTwo <- ggplot(t5, aes(x = order, y = res, fill = state))
    ggTwo <- ggTwo + geom_bar(stat = 'identity')
    ggTwo <- ggTwo + scale_y_continuous(labels = scales::comma, expand = c(0, 0), breaks = scales::pretty_breaks(n = 5))
    ggTwo <- ggTwo + scale_fill_manual(values = p.col)
    ggTwo <- ggTwo + ggtitle("2020")
    ggTwo <- ggTwo + theme_classic()
    ggTwo <- ggTwo + theme(plot.title = element_text(hjust = 0.5))
    ggTwo <- ggTwo + theme(title = element_text(size = 18))
    ggTwo <- ggTwo + theme(axis.title = element_blank())
    ggTwo <- ggTwo + theme(axis.text.x = element_text(size = 13))
    ggTwo <- ggTwo + theme(axis.text.y = element_text(size = 15))
    ggTwo <- ggTwo + theme(legend.text = element_text(size = 13))
    ggTwo <- ggTwo + theme(legend.title = element_text(size = 15))
    ggTwo <- ggTwo + theme(legend.position = "right")
    ggTwo <- ggTwo + theme(plot.background = element_blank())
    ggTwo <- ggTwo + theme(panel.background = element_blank())

    my.legend <- GrabLegend(ggOne)
    l.width <- sum(my.legend$width)

    grid.arrange(
        arrangeGrob(
            ggOne + theme(legend.position = "none"),
            ggTwo + theme(legend.position = "none"),
            ncol = 2),
        my.legend,
        widths = grid::unit.c(unit(1, "npc") - l.width, l.width),
        nrow = 1)
}

Gen909090Plot <- function() {
    out    <- Extract909090Data()

    red    <- rgb(red = 223, green = 74,  blue = 50, max = 255)
    yellow <- rgb(red = 245, green = 157, blue = 0,  max = 255)
    green  <- rgb(red = 0,   green = 167, blue = 87, max = 255)
    cfill  <- c(red, yellow, green)

    ranking <- c(
        which(sort(out$res) == out$res[1]),
        which(sort(out$res) == out$res[2]),
        which(sort(out$res) == out$res[3])
    )

    vbCol <- c("red", "yellow", "green")

    vbOut1 <- round(out[out$def == "% Diagnosed",    "res"] * 100, digits = 0)
    vbOut2 <- round(out[out$def == "% On Treatment", "res"] * 100, digits = 0)
    vbOut3 <- round(out[out$def == "% Suppressed",   "res"] * 100, digits = 0)

    output$vb_90            <- renderValueBox({ valueBox(paste0(vbOut1, "%"), "Diagnosed",          color = vbCol[ranking][1], icon = icon("medkit", lib = "font-awesome")) })
    output$vb_9090          <- renderValueBox({ valueBox(paste0(vbOut2, "%"), "On Treatment",       color = vbCol[ranking][2], icon = icon("medkit", lib = "font-awesome")) })
    output$vb_909090        <- renderValueBox({ valueBox(paste0(vbOut3, "%"), "Virally Suppressed", color = vbCol[ranking][3], icon = icon("medkit", lib = "font-awesome")) })
    output$vb_90_wizard     <- renderValueBox({ valueBox(paste0(vbOut1, "%"), "Diagnosed",          color = vbCol[ranking][1], icon = icon("medkit", lib = "font-awesome")) })
    output$vb_9090_wizard   <- renderValueBox({ valueBox(paste0(vbOut2, "%"), "On Treatment",       color = vbCol[ranking][2], icon = icon("medkit", lib = "font-awesome")) })
    output$vb_909090_wizard <- renderValueBox({ valueBox(paste0(vbOut3, "%"), "Virally Suppressed", color = vbCol[ranking][3], icon = icon("medkit", lib = "font-awesome")) })

    ggOut <- ggplot(out, aes(x = def, y = res))
    ggOut <- ggOut + geom_bar(aes(fill = def), position = 'dodge', stat = 'identity')
    ggOut <- ggOut + geom_errorbar(mapping = aes(x = def, ymin = min, ymax = max), width = 0.2, size = 1)
    ggOut <- ggOut + scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1), labels = scales::percent, expand = c(0, 0))
    ggOut <- ggOut + scale_fill_manual(values = cfill[ranking])
    ggOut <- ggOut + geom_abline(intercept = 0.9, slope = 0)
    ggOut <- ggOut + theme_classic()
    ggOut <- ggOut + theme(plot.title = element_text(hjust = 0.5))
    ggOut <- ggOut + theme(title = element_text(size = 20))
    ggOut <- ggOut + theme(axis.title = element_blank())
    ggOut <- ggOut + theme(axis.ticks.x = element_blank())
    ggOut <- ggOut + theme(axis.text.x = element_text(size = 18))
    ggOut <- ggOut + theme(axis.text.y = element_text(size = 18))
    ggOut <- ggOut + theme(legend.position = "none")
    ggOut <- ggOut + theme(plot.background = element_blank())
    ggOut <- ggOut + theme(panel.background = element_blank())
    ggOut <- ggOut + theme(axis.line.y = element_line())
    ggOut <- ggOut + theme(text = element_text(family = figFont))
    ggOut <- ggOut + geom_label(aes(x = def, label = scales::percent(round(out$res, digits = 2))), size = 8)
    ggOut
}

GenNewInfPlot <- function(wizard) {
    result <- CallModel()

    out <- c()
    min <- c()
    max <- c()
    for (j in 1:251) {
        dat <- Quantile_95(unlist(lapply(result, function(x) sum(x$NewInf[j]))))
        out[j] <- dat[["mean"]]
        min[j] <- dat[["lower"]]
        max[j] <- dat[["upper"]]
    }

    timeOne <- seq(0, 5, 0.02)
    NewInfOne <- out / timeOne
    minOne <- min / timeOne
    maxOne <- max / timeOne

    time <- c(2, seq(51, 251, 1 * (1/0.02)))
    NewInf <- NewInfOne[time]
    min <- minOne[time]
    max <- maxOne[time]

    timeOut <- seq(2015, 2020, 1)
    df <- data.frame(timeOut, NewInf, min, max)

    c.fill <- rev(brewer.pal(9,"Blues")[3:8])

    ggOut <- ggplot(df, aes(x = timeOut, NewInf))
    ggOut <- ggOut + geom_bar(stat = "identity", size = 2, fill = c.fill)
    ggOut <- ggOut + geom_errorbar(mapping = aes(x = timeOut, ymin = min, ymax = max), width = 0.2, size = 1)
    ggOut <- ggOut + expand_limits(y = round(max(df$max), digits = -4))
    ggOut <- ggOut + theme_classic()
    ggOut <- ggOut + scale_y_continuous(labels = scales::comma, expand = c(0, 0), breaks = scales::pretty_breaks(n = 5))
    ggOut <- ggOut + theme(axis.line.y = element_line())
    ggOut <- ggOut + scale_x_continuous(breaks = seq(2015, 2020, 1), labels = seq(2015, 2020, 1))
    ggOut <- ggOut + theme(text = element_text(family = figFont))
    ggOut <- ggOut + theme(axis.ticks.x = element_blank())
    if (wizard) {
        ggOut <- ggOut + theme(axis.text.x = element_text(size = 12))
        ggOut <- ggOut + theme(axis.text.y = element_text(size = 12))
        ggOut <- ggOut + theme(axis.title =  element_blank())
    } else {
        ggOut <- ggOut + theme(axis.text.x = element_text(size = 18))
        ggOut <- ggOut + theme(axis.text.y = element_text(size = 18))
        ggOut <- ggOut + theme(axis.title =  element_blank())
    }
    ggOut
}

GenAidsDeathsPlot <- function(wizard) {
    result <- CallModel()

    out <- c()
    min <- c()
    max <- c()
    for (j in 1:251) {
        dat <- Quantile_95(unlist(lapply(result, function(x) sum(x$HivMortality[j]))))
        out[j] <- dat[["mean"]]
        min[j] <- dat[["lower"]]
        max[j] <- dat[["upper"]]
    }

    timeOne <- seq(0, 5, 0.02)
    HivMortalityOne <- out / timeOne
    minOne <- min / timeOne
    maxOne <- max / timeOne

    time <- c(2, seq(51, 251, 1 * (1/0.02)))
    HivMortality <- HivMortalityOne[time]
    min <- minOne[time]
    max <- maxOne[time]

    timeOut <- seq(2015, 2020, 1)
    df <- data.frame(timeOut, HivMortality, min, max)

    c.fill <- rev(brewer.pal(9,"Blues")[3:8])

    ggOut <- ggplot(df, aes(x = timeOut, HivMortality))
    ggOut <- ggOut + geom_bar(stat = "identity", size = 2, fill = c.fill)
    ggOut <- ggOut + geom_errorbar(mapping = aes(x = timeOut, ymin = min, ymax = max), width = 0.2, size = 1)
    ggOut <- ggOut + expand_limits(y = round(max(df$max), digits = -4))
    ggOut <- ggOut + theme_classic()
    ggOut <- ggOut + scale_y_continuous(labels = scales::comma, expand = c(0, 0), breaks = scales::pretty_breaks(n = 5))
    ggOut <- ggOut + theme(axis.line.y = element_line())
    ggOut <- ggOut + scale_x_continuous(breaks = seq(2015, 2020, 1), labels = seq(2015, 2020, 1))
    ggOut <- ggOut + theme(text = element_text(family = figFont))
    ggOut <- ggOut + theme(axis.ticks.x = element_blank())
    if (wizard) {
        ggOut <- ggOut + theme(axis.text.x = element_text(size = 12))
        ggOut <- ggOut + theme(axis.text.y = element_text(size = 12))
        ggOut <- ggOut + theme(axis.title =  element_blank())
    } else {
        ggOut <- ggOut + theme(axis.text.x = element_text(size = 18))
        ggOut <- ggOut + theme(axis.text.y = element_text(size = 18))
        ggOut <- ggOut + theme(axis.title =  element_blank())
    }
    ggOut
}

GenSinglePlot <- function() {
    result <- CallModel()

    out <- c()
    min <- c()
    max <- c()
    for (j in 1:251) {
        out[j] <-  mean(unlist(lapply(result, function(x) sum(x[j,input$y]))))
        min[j] <- range(unlist(lapply(result, function(x) sum(x[j,input$y]))))[1]
        max[j] <- range(unlist(lapply(result, function(x) sum(x[j,input$y]))))[2]
    }

    time <- seq(0, 5, 0.02)
    df <- data.frame(time, out, min, max)

    ggOut <- ggplot(df, aes(x = time, y = out))
    ggOut <- ggOut + geom_ribbon(aes(ymin = min, ymax = max), alpha = 0.2, fill = ggColorHue(3)[3])
    ggOut <- ggOut + geom_line(size = 1.5, color = ggColorHue(1))
    ggOut <- ggOut + theme_classic()
    ggOut <- ggOut + scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n = 5))
    ggOut <- ggOut + scale_x_continuous(limits = c(0, 5), breaks = seq(0, 5, 1), labels = seq(2015, 2020, 1))
    ggOut <- ggOut + theme(axis.text.x = element_text(size = 18))
    ggOut <- ggOut + theme(axis.text.y = element_text(size = 18))
    ggOut <- ggOut + theme(axis.title = element_text(size = 18))
    ggOut <- ggOut + theme(axis.line.x = element_line())
    ggOut <- ggOut + theme(axis.line.y = element_line())
    ggOut <- ggOut + theme(axis.title.y = element_blank())
    ggOut <- ggOut + xlab("Year")
    ggOut
}

GenAllPlot <- function() {
    result <- CallModel()

    UnDx                 <- c()
    Dx                   <- c()
    Care                 <- c()
    PreLtfu              <- c()
    Tx                   <- c()
    Vs                   <- c()
    Ltfu                 <- c()
    N                    <- c()
    NewInf               <- c()
    TotalCost            <- c()
    HivMortalityProp     <- c()
    NaturalMortalityProp <- c()

    for (j in 1:251) {
        UnDx[j]                 <- mean(unlist(lapply(result, function(x) sum(x$UnDx[j]))))
        Dx[j]                   <- mean(unlist(lapply(result, function(x) sum(x$Dx[j]))))
        Care[j]                 <- mean(unlist(lapply(result, function(x) sum(x$Care[j]))))
        PreLtfu[j]              <- mean(unlist(lapply(result, function(x) sum(x$PreLtfu[j]))))
        Tx[j]                   <- mean(unlist(lapply(result, function(x) sum(x$Tx[j]))))
        Vs[j]                   <- mean(unlist(lapply(result, function(x) sum(x$Vs[j]))))
        Ltfu[j]                 <- mean(unlist(lapply(result, function(x) sum(x$Ltfu[j]))))
        N[j]                    <- mean(unlist(lapply(result, function(x) sum(x$N[j]))))
        NewInf[j]               <- mean(unlist(lapply(result, function(x) sum(x$NewInf[j]))))
        TotalCost[j]            <- mean(unlist(lapply(result, function(x) sum(x$TotalCost[j]))))
        HivMortalityProp[j]     <- mean(unlist(lapply(result, function(x) sum(x$HivMortalityProp[j]))))
        NaturalMortalityProp[j] <- mean(unlist(lapply(result, function(x) sum(x$NaturalMortalityProp[j]))))
    }

    time <- seq(0, 5, 0.02)

    gridExtra::grid.arrange(

        ggplot(data.frame(time, UnDx), aes(x = time, y = UnDx)) + geom_line(color = ggColorHue(1)) +
        theme_classic() + theme(axis.text.x = element_text(size = 18)) + theme(axis.text.y = element_text(size = 18)) +
        theme(axis.title = element_text(size = 18)) + xlab("Year") + theme(axis.line.x = element_line()) +
        theme(axis.line.y = element_line()),

        ggplot(data.frame(time, Dx), aes(x = time, y = Dx)) + geom_line(color = ggColorHue(1)) +
        theme_classic() + theme(axis.text.x = element_text(size = 18)) + theme(axis.text.y = element_text(size = 18)) +
        theme(axis.title = element_text(size = 18)) + xlab("Year") + theme(axis.line.x = element_line()) +
        theme(axis.line.y = element_line()),

        ggplot(data.frame(time, Care), aes(x = time, y = Care)) + geom_line(color = ggColorHue(1)) +
        theme_classic() + theme(axis.text.x = element_text(size = 18)) + theme(axis.text.y = element_text(size = 18)) +
        theme(axis.title = element_text(size = 18)) + xlab("Year") + theme(axis.line.x = element_line()) +
        theme(axis.line.y = element_line()),

        ggplot(data.frame(time, PreLtfu), aes(x = time, y = PreLtfu)) + geom_line(color = ggColorHue(1)) +
        theme_classic() + theme(axis.text.x = element_text(size = 18)) + theme(axis.text.y = element_text(size = 18)) +
        theme(axis.title = element_text(size = 18)) + xlab("Year") + theme(axis.line.x = element_line()) +
        theme(axis.line.y = element_line()),

        ggplot(data.frame(time, Tx), aes(x = time, y = Tx)) + geom_line(color = ggColorHue(1)) +
        theme_classic() + theme(axis.text.x = element_text(size = 18)) + theme(axis.text.y = element_text(size = 18)) +
        theme(axis.title = element_text(size = 18)) + xlab("Year") + theme(axis.line.x = element_line()) +
        theme(axis.line.y = element_line()),

        ggplot(data.frame(time, Vs), aes(x = time, y = Vs)) + geom_line(color = ggColorHue(1)) +
        theme_classic() + theme(axis.text.x = element_text(size = 18)) + theme(axis.text.y = element_text(size = 18)) +
        theme(axis.title = element_text(size = 18)) + xlab("Year") + theme(axis.line.x = element_line()) +
        theme(axis.line.y = element_line()),

        ggplot(data.frame(time, Ltfu), aes(x = time, y = Ltfu)) + geom_line(color = ggColorHue(1)) +
        theme_classic() + theme(axis.text.x = element_text(size = 18)) + theme(axis.text.y = element_text(size = 18)) +
        theme(axis.title = element_text(size = 18)) + xlab("Year") + theme(axis.line.x = element_line()) +
        theme(axis.line.y = element_line()),

        ggplot(data.frame(time, N), aes(x = time, y = N)) + geom_line(color = ggColorHue(1)) +
        theme_classic() + theme(axis.text.x = element_text(size = 18)) + theme(axis.text.y = element_text(size = 18)) +
        theme(axis.title = element_text(size = 18)) + xlab("Year") + theme(axis.line.x = element_line()) +
        theme(axis.line.y = element_line()),

        ggplot(data.frame(time, NewInf), aes(x = time, y = NewInf)) + geom_line(color = ggColorHue(1)) +
        theme_classic() + theme(axis.text.x = element_text(size = 18)) + theme(axis.text.y = element_text(size = 18)) +
        theme(axis.title = element_text(size = 18)) + xlab("Year") + theme(axis.line.x = element_line()) +
        theme(axis.line.y = element_line()),

        ggplot(data.frame(time, TotalCost), aes(x = time, y = TotalCost)) + geom_line(color = ggColorHue(1)) +
        theme_classic() + theme(axis.text.x = element_text(size = 18)) + theme(axis.text.y = element_text(size = 18)) +
        theme(axis.title = element_text(size = 18)) + xlab("Year") + theme(axis.line.x = element_line()) +
        theme(axis.line.y = element_line()),

        ggplot(data.frame(time, HivMortalityProp), aes(x = time, y = HivMortalityProp)) + geom_line(color = ggColorHue(1)) +
        theme_classic() + theme(axis.text.x = element_text(size = 18)) + theme(axis.text.y = element_text(size = 18)) +
        theme(axis.title = element_text(size = 18)) + xlab("Year") + theme(axis.line.x = element_line()) +
        theme(axis.line.y = element_line()),

        ggplot(data.frame(time, NaturalMortalityProp), aes(x = time, y = NaturalMortalityProp)) + geom_line(color = ggColorHue(1)) +
        theme_classic() + theme(axis.text.x = element_text(size = 18)) + theme(axis.text.y = element_text(size = 18)) +
        theme(axis.title = element_text(size = 18)) + xlab("Year") + theme(axis.line.x = element_line()) +
        theme(axis.line.y = element_line()),

        nrow = 4,
        ncol = 3
    )
}
