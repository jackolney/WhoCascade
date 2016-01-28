ExtractCascadeData <- function(year) {
        result <- CallModel()

        DX <- sum(result$Dx[year], result$Care[year], result$PreLtfu[year], result$ART[year], result$Ltfu[year])
        CX <- sum(result$Care[year], result$ART[year])
        TX <- result$ART[year]
        VS <- result$Vs[year]
        res <- c(1, DX, CX, TX, VS)
        def <- c("% PLHIV", "% Diagnosed", "% In Care", "% Treatment", "% Suppressed")
        df <- data.frame(def, res)
        df$def <- factor(df$def, levels = c("% PLHIV", "% Diagnosed", "% In Care", "% Treatment", "% Suppressed"))
        df
}

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
        geom_bar(aes(fill = definition), position = 'dodge', stat = 'identity') +
        scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1), labels = percent, expand = c(0, 0)) +
        scale_fill_manual(values = "red") +
        ggtitle("Care Cascade in 2015") +
        theme_classic() +
        theme(title = element_text(size = 15)) +
        theme(axis.title = element_blank()) +
        theme(axis.text.x = element_text(size = 11)) +
        theme(axis.text.y = element_text(size = 15)) +
        theme(legend.position = "none") +
        theme(plot.background = element_blank()) +
        theme(panel.background = element_blank()) +
}

GenCascadePlot <- function() {
    t0 <- ExtractCascadeData(1) # t0 = 1
    t5 <- ExtractCascadeData(250) # t5 = 5 / 0.02

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

    grid.arrange(plot.one, plot.two, nrow = 1, ncol = 2))
}
