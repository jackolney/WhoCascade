BuildEditCascadePlot <- function(data) {
    data <- subset(data, !is.na(data$value))
    if (dim(data)[1] != 0) {
        data$indicator <- factor(data$indicator, levels = c("PLHIV", "PLHIV Diagnosed", "PLHIV in Care", "PLHIV on ART", "PLHIV Suppressed"))
        ggOut <- ggplot(data, aes(x = year, y = value))
        ggOut <- ggOut + geom_bar(aes(fill = indicator), stat = "identity", position = "dodge")
        ggOut <- ggOut + expand_limits(y = round(max(data$value), digits = -5))
        ggOut <- ggOut + theme_classic()
        ggOut <- ggOut + scale_y_continuous(
            labels = scales::comma,
            breaks = scales::pretty_breaks(n = 5),
            expand = c(0, 0))
        ggOut <- ggOut + scale_x_continuous(breaks = seq(2010, 2015, 1), labels = seq(2010, 2015, 1))
        ggOut <- ggOut + theme(axis.text.x = element_text(size = 10))
        ggOut <- ggOut + theme(axis.text.y = element_text(size = 10))
        ggOut <- ggOut + theme(axis.title = element_text(size = 10))
        ggOut <- ggOut + theme(legend.text = element_text(size = 10))
        ggOut <- ggOut + theme(legend.position = "top")
        ggOut <- ggOut + theme(axis.ticks.x = element_blank())
        ggOut <- ggOut + theme(axis.line.x = element_line())
        ggOut <- ggOut + theme(axis.line.y = element_line())
        ggOut <- ggOut + theme(axis.title = element_blank())
        ggOut <- ggOut + theme(legend.title = element_blank())
        ggOut <- ggOut + theme(plot.background = element_blank())
        ggOut <- ggOut + theme(legend.background = element_blank())
        ggOut <- ggOut + theme(panel.background = element_blank())
        ggOut <- ggOut + theme(text = element_text(family = figFont))
        ggOut
    }
}

BuildEditCD42010Plot <- function(data) {
    if (!isReallyEmpty(data)) {
        if (any(!is.na(data[1:7,"Proportion"]))) {
            DF_Off <- data[1:7,]
            DF_Off$pos <- cumsum(DF_Off$Proportion) - DF_Off$Proportion / 2
            DF_Off$Category <- factor(DF_Off$Category, levels = c("<500", "350-500", "250-350", "200-250", "100-200", "50-100", "<50"))
            ggOff <- ggplot(DF_Off, aes(x = "", y = Proportion, fill = Category))
            ggOff <- ggOff + geom_bar(width = 1, stat = "identity")
            ggOff <- ggOff + theme_classic()
            ggOff <- ggOff + coord_polar(theta = "y")
            ggOff <- ggOff + geom_label_repel(aes(y = pos, label = scales::percent(round(Proportion, digits = 2))), size = 5, family = figFont, show.legend = FALSE)
            ggOff <- ggOff + theme(text = element_text(family = figFont))
            ggOff <- ggOff + scale_fill_manual(values = rev(brewer.pal(7, "RdYlGn")))
            ggOff <- ggOff + theme(axis.title = element_blank())
            ggOff <- ggOff + theme(legend.title = element_blank())
            ggOff <- ggOff + theme(axis.text = element_blank())
            ggOff <- ggOff + theme(axis.ticks = element_blank())
            ggOff <- ggOff + theme(plot.background = element_blank())
            ggOff <- ggOff + theme(legend.background = element_blank())
            ggOff <- ggOff + theme(panel.background = element_blank())
            ggOff <- ggOff + ggtitle("Off ART", subtitle = "2010")
            ggOff <- ggOff + theme(plot.title = element_text(hjust = 0.5))
            ggOff <- ggOff + theme(plot.subtitle = element_text(hjust = 0.5))
            ggOff
        } else {
            ggOff <- ggplot() + geom_blank()
            ggOff <- ggOff + theme(plot.background = element_blank())
            ggOff <- ggOff + theme(legend.background = element_blank())
            ggOff <- ggOff + theme(panel.background = element_blank())
            ggOff
        }
        if (any(!is.na(data[8:14,"Proportion"]))) {
            DF_On <- data[8:14,]
            DF_On$pos <- cumsum(DF_On$Proportion) - DF_On$Proportion / 2
            DF_On$Category <- factor(DF_On$Category, levels = c("<500", "350-500", "250-350", "200-250", "100-200", "50-100", "<50"))
            ggOn <- ggplot(DF_On, aes(x = "", y = Proportion, fill = Category))
            ggOn <- ggOn + geom_bar(width = 1, stat = "identity")
            ggOn <- ggOn + theme_classic()
            ggOn <- ggOn + coord_polar(theta = "y")
            ggOn <- ggOn + geom_label_repel(aes(y = pos, label = scales::percent(round(Proportion, digits = 2))), size = 5, family = figFont, show.legend = FALSE)
            ggOn <- ggOn + theme(text = element_text(family = figFont))
            ggOn <- ggOn + scale_fill_manual(values = rev(brewer.pal(7, "RdYlGn")))
            ggOn <- ggOn + theme(axis.title = element_blank())
            ggOn <- ggOn + theme(legend.title = element_blank())
            ggOn <- ggOn + theme(axis.text = element_blank())
            ggOn <- ggOn + theme(axis.ticks = element_blank())
            ggOn <- ggOn + theme(plot.background = element_blank())
            ggOn <- ggOn + theme(legend.background = element_blank())
            ggOn <- ggOn + theme(panel.background = element_blank())
            ggOn <- ggOn + ggtitle("On ART", subtitle = "2010")
            ggOn <- ggOn + theme(plot.title = element_text(hjust = 0.5))
            ggOn <- ggOn + theme(plot.subtitle = element_text(hjust = 0.5))
            ggOn
        } else {
            ggOn <- ggplot() + geom_blank()
            ggOn <- ggOn + theme(plot.background = element_blank())
            ggOn <- ggOn + theme(legend.background = element_blank())
            ggOn <- ggOn + theme(panel.background = element_blank())
            ggOn
        }
        suppressWarnings(GridArrangeSharedLegend(ggOff, ggOn, ncol = 2, nrow = 1, position = "right"))
    }
}

BuildEditCD42015Plot <- function(data) {
    if (!isReallyEmpty(data)) {
        if (any(!is.na(data[1:7,"Proportion"]))) {
            DF_Off <- data[1:7,]
            DF_Off$pos <- cumsum(DF_Off$Proportion) - DF_Off$Proportion / 2
            DF_Off$Category <- factor(DF_Off$Category, levels = c("<500", "350-500", "250-350", "200-250", "100-200", "50-100", "<50"))
            ggOff <- ggplot(DF_Off, aes(x = "", y = Proportion, fill = Category))
            ggOff <- ggOff + geom_bar(width = 1, stat = "identity")
            ggOff <- ggOff + theme_classic()
            ggOff <- ggOff + coord_polar(theta = "y")
            ggOff <- ggOff + geom_label_repel(aes(y = pos, label = scales::percent(round(Proportion, digits = 2))), size = 5, family = figFont, show.legend = FALSE)
            ggOff <- ggOff + theme(text = element_text(family = figFont))
            ggOff <- ggOff + scale_fill_manual(values = rev(brewer.pal(7, "RdYlGn")))
            ggOff <- ggOff + theme(axis.title = element_blank())
            ggOff <- ggOff + theme(legend.title = element_blank())
            ggOff <- ggOff + theme(axis.text = element_blank())
            ggOff <- ggOff + theme(axis.ticks = element_blank())
            ggOff <- ggOff + theme(plot.background = element_blank())
            ggOff <- ggOff + theme(legend.background = element_blank())
            ggOff <- ggOff + theme(panel.background = element_blank())
            ggOff <- ggOff + ggtitle("Off ART", subtitle = "2015")
            ggOff <- ggOff + theme(plot.title = element_text(hjust = 0.5))
            ggOff <- ggOff + theme(plot.subtitle = element_text(hjust = 0.5))
            ggOff
        } else {
            ggOff <- ggplot() + geom_blank()
            ggOff <- ggOff + theme(plot.background = element_blank())
            ggOff <- ggOff + theme(legend.background = element_blank())
            ggOff <- ggOff + theme(panel.background = element_blank())
            ggOff
        }
        if (any(!is.na(data[8:14,"Proportion"]))) {
            DF_On <- data[8:14,]
            DF_On$pos <- cumsum(DF_On$Proportion) - DF_On$Proportion / 2
            DF_On$Category <- factor(DF_On$Category, levels = c("<500", "350-500", "250-350", "200-250", "100-200", "50-100", "<50"))
            ggOn <- ggplot(DF_On, aes(x = "", y = Proportion, fill = Category))
            ggOn <- ggOn + geom_bar(width = 1, stat = "identity")
            ggOn <- ggOn + theme_classic()
            ggOn <- ggOn + coord_polar(theta = "y")
            ggOn <- ggOn + geom_label_repel(aes(y = pos, label = scales::percent(round(Proportion, digits = 2))), size = 5, family = figFont, show.legend = FALSE)
            ggOn <- ggOn + theme(text = element_text(family = figFont))
            ggOn <- ggOn + scale_fill_manual(values = rev(brewer.pal(7, "RdYlGn")))
            ggOn <- ggOn + theme(axis.title = element_blank())
            ggOn <- ggOn + theme(legend.title = element_blank())
            ggOn <- ggOn + theme(axis.text = element_blank())
            ggOn <- ggOn + theme(axis.ticks = element_blank())
            ggOn <- ggOn + theme(plot.background = element_blank())
            ggOn <- ggOn + theme(legend.background = element_blank())
            ggOn <- ggOn + theme(panel.background = element_blank())
            ggOn <- ggOn + ggtitle("On ART", subtitle = "2015")
            ggOn <- ggOn + theme(plot.title = element_text(hjust = 0.5))
            ggOn <- ggOn + theme(plot.subtitle = element_text(hjust = 0.5))
            ggOn
        } else {
            ggOn <- ggplot() + geom_blank()
            ggOn <- ggOn + theme(plot.background = element_blank())
            ggOn <- ggOn + theme(legend.background = element_blank())
            ggOn <- ggOn + theme(panel.background = element_blank())
            ggOn
        }
        suppressWarnings(GridArrangeSharedLegend(ggOff, ggOn, ncol = 2, nrow = 1, position = "right"))
    }
}

BuildEditIncidencePlot <- function(data) {
    if(any(!is.na(data[,3:9]))) {
        dat <- reshape2::melt(data)
        theData <- subset(dat, dat$type == "Median")
        theData$lower <- subset(dat$value, dat$type == "Lower")
        theData$upper <- subset(dat$value, dat$type == "Upper")
        ggOut <- ggplot(data = theData, aes(x = variable, y = value, group = type))
        ggOut <- ggOut + geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, fill = "#4F8ABA")
        ggOut <- ggOut + geom_point(col = "black", size = 5)
        ggOut <- ggOut + geom_line(col = "black")
        ggOut <- ggOut + theme_classic()
        ggOut <- ggOut + scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n = 5))
        ggOut <- ggOut + expand_limits(y = round(max(theData$value), digits = -3))
        ggOut <- ggOut + theme(axis.line.x = element_line())
        ggOut <- ggOut + theme(axis.line.y = element_line())
        ggOut <- ggOut + theme(legend.position = "none")
        ggOut <- ggOut + theme(text = element_text(family = figFont))
        ggOut <- ggOut + theme(axis.title = element_blank())
        ggOut <- ggOut + theme(axis.text = element_text(size = 14))
        ggOut <- ggOut + theme(plot.background = element_blank())
        ggOut <- ggOut + theme(legend.background = element_blank())
        ggOut <- ggOut + theme(panel.background = element_blank())
        ggOut
    }
}
