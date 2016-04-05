GenYourCascadePlot <- function(h) {
    t0 <- ExtractCascadeData(1)

    c.fill <- rev(brewer.pal(9, "Blues")[3:8])
    c.fill[h] <- "red"

    ggplot(t0, aes(def, res)) +
    geom_bar(aes(fill = def), position = 'dodge', stat = 'identity') +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1), labels = percent, expand = c(0, 0)) +
    scale_fill_manual(values = c.fill) +
    ggtitle("Care Cascade in 2015") +
    theme_classic() +
    theme(title = element_text(size = 15)) +
    theme(axis.title = element_blank()) +
    theme(axis.text.x = element_text(size = 11)) +
    theme(axis.text.y = element_text(size = 15)) +
    theme(legend.position = "none") +
    theme(plot.background = element_blank()) +
    theme(panel.background = element_blank())
}

GenLtfuPlot <- function() {
    result <- CallModel()
    res <- result$Ltfu[1]
    def <- c("% LTFU")
    df <- data.frame(def, res)
    df$def <- factor(df$def, levels = "% LTFU")

    ggplot(df, aes(def, res)) +
    geom_bar(aes(fill = def), position = 'dodge', stat = 'identity') +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1), labels = percent, expand = c(0, 0)) +
    scale_fill_manual(values = "red") +
    ggtitle("Care Cascade in 2015") +
    theme_classic() +
    theme(title = element_text(size = 15)) +
    theme(axis.title = element_blank()) +
    theme(axis.text.x = element_text(size = 11)) +
    theme(axis.text.y = element_text(size = 15)) +
    theme(legend.position = "none") +
    theme(plot.background = element_blank()) +
    theme(panel.background = element_blank())
}

GenCascadePlot <- function() {
    t0 <- ExtractCascadeData(1) # t0 = 1
    t5 <- ExtractCascadeData(251) # t5 = (5 / 0.02) + 1 [t0]

    c.fill <- rev(brewer.pal(9,"Blues")[3:8])

    plot.one <- ggplot(t0, aes(def, res)) +
    geom_bar(aes(fill = def), position = 'dodge', stat = 'identity') +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1), labels = percent, expand = c(0, 0)) +
    scale_fill_manual(values=c.fill) +
    ggtitle("Care Cascade in 2015") +
    theme_classic() +
    theme(title = element_text(size = 18)) +
    theme(axis.title = element_blank()) +
    theme(axis.text.x = element_text(size = 15)) +
    theme(axis.text.y = element_text(size = 18)) +
    theme(legend.position = "none")

    plot.two <- ggplot(t5, aes(def, res)) +
    geom_bar(aes(fill = def), position = 'dodge', stat = 'identity') +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1), labels = percent, expand = c(0, 0)) +
    scale_fill_manual(values=c.fill) +
    ggtitle("Care Cascade in 2020") +
    theme_classic() +
    theme(title = element_text(size = 18)) +
    theme(axis.title = element_blank()) +
    theme(axis.text.x = element_text(size = 15)) +
    theme(axis.text.y = element_text(size = 18)) +
    theme(legend.position = "none")

    grid.arrange(plot.one, plot.two, nrow = 1, ncol = 2)
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
    p.col <- c(cols[3],cols[2],cols[4],cols[5],cols[1],cols[9],cols[8])

    o <- ggplot(t0, aes(x = order, y = res, fill = state)) +
    geom_bar(stat = 'identity') +
    scale_y_continuous(breaks = seq(0, 1, 0.1), labels = percent, expand = c(0, 0)) +
    scale_fill_manual(values = p.col) +
    ggtitle("Care Cascade in 2015") +
    theme_classic() +
    theme(title = element_text(size = 18)) +
    theme(axis.title = element_blank()) +
    theme(axis.text.x = element_text(size = 13)) +
    theme(axis.text.y = element_text(size = 15)) +
    theme(legend.text = element_text(size = 15)) +
    theme(legend.title = element_text(size = 15)) +
    theme(legend.position = "right")

    p <- ggplot(t5, aes(x = order, y = res, fill = state)) +
    geom_bar(stat = 'identity') +
    scale_y_continuous(breaks = seq(0, 1, 0.1), labels = percent, expand = c(0, 0)) +
    scale_fill_manual(values = p.col) +
    ggtitle("Care Cascade in 2020") +
    theme_classic() +
    theme(title = element_text(size = 18)) +
    theme(axis.title = element_blank()) +
    theme(axis.text.x = element_text(size = 13)) +
    theme(axis.text.y = element_text(size = 15)) +
    theme(legend.text = element_text(size = 15)) +
    theme(legend.title = element_text(size = 15)) +
    theme(legend.position = "right")

    my.legend <- GrabLegend(o)
    l.width <- sum(my.legend$width)

    grid.arrange(
        arrangeGrob(
            o + theme(legend.position = "none"),
            p + theme(legend.position = "none"),
            ncol = 2),
        my.legend,
        widths = grid::unit.c(unit(1, "npc") - l.width, l.width),
        nrow = 1)
}

Gen909090Plot <- function() {
    df <- Extract909090Data()

    red <- rgb(red = 223, green = 74, blue = 50, max = 255)
    yellow <- rgb(red = 245, green = 157, blue = 0, max = 255)
    green <- rgb(red = 0, green = 167, blue = 87, max = 255)
    c.fill <- c(red, yellow, green)

    ggplot(df, aes(def, res)) +
    geom_bar(aes(fill = def), position = 'dodge', stat = 'identity') +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1), labels = percent, expand = c(0, 0)) +
    scale_fill_manual(values = c.fill) +
    geom_abline(intercept = 0.9, slope = 0) +
    theme_classic() +
    theme(title = element_text(size = 20)) +
    theme(axis.title = element_blank()) +
    theme(axis.text.x = element_text(size = 18)) +
    theme(axis.text.y = element_text(size = 18)) +
    theme(legend.position = "none")
}

GenNewInfPlot <- function() {
    ggplot(CallModel(), aes(x = time, y = NewInfProp)) +
    geom_line(size = 2) +
    theme_classic() +
    theme(axis.text.x = element_text(size = 18)) +
    theme(axis.text.y = element_text(size = 18)) +
    theme(axis.title = element_text(size = 18)) +
    xlab("Year") +
    ylab("# new infections / total infected population") +
    scale_x_continuous(limits = c(0, 5), breaks = seq(0, 5, 1), labels = seq(2015, 2020, 1))
}

GenAidsDeathsPlot <- function() {
    ggplot(CallModel(), aes(x = time, y = HivMortalityProp)) +
    geom_line(size = 2) +
    theme_classic() +
    theme(axis.text.x = element_text(size = 18)) +
    theme(axis.text.y = element_text(size = 18)) +
    theme(axis.title = element_text(size = 18)) +
    xlab("Year") +
    ylab("# AIDS deaths / total infected population") +
    scale_x_continuous(limits = c(0, 5), breaks = seq(0, 5, 1), labels = seq(2015, 2020, 1))
}

GenSinglePlot <- function() {
    ggplot(CallModel(), aes_string(x = "time", y = input$y)) +
    geom_line(size = 2) +
    theme_classic() +
    theme(axis.text.x = element_text(size = 18)) +
    theme(axis.text.y = element_text(size = 18)) +
    theme(axis.title = element_text(size = 18)) +
    xlab("Year") +
    scale_x_continuous(limits = c(0, 5), breaks = seq(0, 5, 1), labels = seq(2015, 2020, 1))
}

GenAllPlot <- function() {
    result <- CallModel()
    grid.arrange(

        ggplot(result, aes(x = time, y = UnDx)) + geom_line() +
        theme(axis.text.x = element_text(size = 18)) + theme(axis.text.y = element_text(size = 18)) + theme(axis.title = element_text(size = 18)) + xlab("Year") + theme_classic(),

        ggplot(result, aes(x = time, y = Dx)) + geom_line() +
        theme(axis.text.x = element_text(size = 18)) + theme(axis.text.y = element_text(size = 18)) + theme(axis.title = element_text(size = 18)) + xlab("Year") + theme_classic(),

        ggplot(result, aes(x = time, y = Care)) + geom_line() +
        theme(axis.text.x = element_text(size = 18)) + theme(axis.text.y = element_text(size = 18)) + theme(axis.title = element_text(size = 18)) + xlab("Year") + theme_classic(),

        ggplot(result, aes(x = time, y = PreLtfu)) + geom_line() +
        theme(axis.text.x = element_text(size = 18)) + theme(axis.text.y = element_text(size = 18)) + theme(axis.title = element_text(size = 18)) + xlab("Year") + theme_classic(),

        ggplot(result, aes(x = time, y = Tx)) + geom_line() +
        theme(axis.text.x = element_text(size = 18)) + theme(axis.text.y = element_text(size = 18)) + theme(axis.title = element_text(size = 18)) + xlab("Year") + theme_classic(),

        ggplot(result, aes(x = time, y = Vs)) + geom_line() +
        theme(axis.text.x = element_text(size = 18)) + theme(axis.text.y = element_text(size = 18)) + theme(axis.title = element_text(size = 18)) + xlab("Year") + theme_classic(),

        ggplot(result, aes(x = time, y = Ltfu)) + geom_line() +
        theme(axis.text.x = element_text(size = 18)) + theme(axis.text.y = element_text(size = 18)) + theme(axis.title = element_text(size = 18)) + xlab("Year") + theme_classic(),

        ggplot(result, aes(x = time, y = N)) + geom_line() +
        theme(axis.text.x = element_text(size = 18)) + theme(axis.text.y = element_text(size = 18)) + theme(axis.title = element_text(size = 18)) + xlab("Year") + theme_classic(),

        ggplot(result, aes(x = time, y = NewInf)) + geom_line() +
        theme(axis.text.x = element_text(size = 18)) + theme(axis.text.y = element_text(size = 18)) + theme(axis.title = element_text(size = 18)) + xlab("Year") + theme_classic(),

        ggplot(result, aes(x = time, y = TotalCost)) + geom_line() +
        theme(axis.text.x = element_text(size = 18)) + theme(axis.text.y = element_text(size = 18)) + theme(axis.title = element_text(size = 18)) + xlab("Year") + theme_classic(),

        ggplot(result, aes(x = time, y = HivMortalityProp)) + geom_line() +
        theme(axis.text.x = element_text(size = 18)) + theme(axis.text.y = element_text(size = 18)) + theme(axis.title = element_text(size = 18)) + xlab("Year") + theme_classic(),

        ggplot(result, aes(x = time, y = NaturalMortalityProp)) + geom_line() +
        theme(axis.text.x = element_text(size = 18)) + theme(axis.text.y = element_text(size = 18)) + theme(axis.title = element_text(size = 18)) + xlab("Year") + theme_classic(),

        nrow = 4,
        ncol = 3
    )
}
