# ------------------ #
# OPTIMISATION PLOTS #
# ------------------ #

# Plot 1
output$plotOpt909090 <- renderPlot({
    # dependency on optimiseInput
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

    ggplot(theResult, aes(x = VS, y = Cost)) +
    geom_point(aes(color = as.factor(get(theStratPoint)), size = as.factor(the909090))) +
    theme_classic() +
    scale_color_discrete(name = input$userStratPoint, labels = Legend.Labels) +
    scale_size_discrete(name = "Achieves 90-90-90", range = c(3, 6), labels = c("no","yes")) +
    guides(colour = guide_legend(override.aes = list(size = 4))) +
    theme(legend.title = element_text(size = 15)) +
    theme(legend.text = element_text(size = 13)) +
    theme(axis.text.x = element_text(size = 18)) +
    theme(axis.text.y = element_text(size = 18)) +
    theme(axis.title = element_text(size = 18)) +
    geom_vline(xintercept = 0.9^3) +
    xlab("Proportion achieving viral suppression by 2020") +
    ylab("Additional cost of care (2013 USD)") +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(labels = comma) +
    coord_cartesian(xlim = plotOpt_909090.ranges$x, ylim = plotOpt_909090.ranges$y)
    },
    height = 400,
    width = 'auto'
)

# Plot 2
output$plotOptDALYs <- renderPlot({
    # dependency on optimiseInput
    input$optimiseInput

    Legend.Labels <- c()
    for(i in 1:length(levels(as.factor(Result_DALYs[[input$userStratPoint]])))) {
        Legend.Labels[i] <- round(as.double(levels(as.factor(Result_DALYs[[input$userStratPoint]]))[i]),2)
    }

    theStratPoint <<- input$userStratPoint

    ggplot(Result_DALYs,aes(x = DALYs, y = Cost)) +
    geom_point(aes(color = as.factor(get(theStratPoint))), size = 5) +
    theme_classic() +
    scale_color_discrete(name = input$userStratPoint, labels = Legend.Labels) +
    guides(colour = guide_legend(override.aes = list(size=4))) +
    theme(legend.title = element_text(size = 15)) +
    theme(legend.text = element_text(size = 13)) +
    theme(axis.text.x = element_text(size = 18)) +
    theme(axis.text.y = element_text(size = 18)) +
    theme(axis.title = element_text(size = 18)) +
    xlab("DALYs Averted (between 2015 and 2020)") +
    ylab("Additional cost of care (2013 USD)") +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(labels = comma) +
    coord_cartesian(xlim = plotOpt_DALYs.ranges$x, ylim = plotOpt_DALYs.ranges$y)
    },
    height = 400,
    width = 'auto'
)

# Plot 3
output$plotOptDALYs909090 <- renderPlot({
    # dependency on optimiseInput
    input$optimiseInput

    Legend.Labels <- c()
    for(i in 1:length(levels(as.factor(Result_DALYs_909090[[input$userStratPoint]])))) {
        Legend.Labels[i] <- round(as.double(levels(as.factor(Result_DALYs_909090[[input$userStratPoint]]))[i]),2)
    }

    theStratPoint <<- input$userStratPoint

    ggplot(Result_DALYs_909090,aes(x = DALYs, y = Cost)) +
    geom_point(aes(color = as.factor(get(theStratPoint))), size = 5) +
    theme_classic() +
    scale_color_discrete(name = input$userStratPoint, labels = Legend.Labels) +
    guides(colour = guide_legend(override.aes = list(size = 4))) +
    theme(legend.title = element_text(size = 15)) +
    theme(legend.text = element_text(size = 13)) +
    theme(axis.text.x = element_text(size = 18)) +
    theme(axis.text.y = element_text(size = 18)) +
    theme(axis.title = element_text(size = 18)) +
    xlab("DALYs Averted (between 2015 and 2020)") +
    ylab("Additional cost of care (2013 USD)") +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(labels = comma) +
    coord_cartesian(xlim = plotOpt_DALYs_909090.ranges$x, ylim = plotOpt_DALYs_909090.ranges$y)
    },
    height = 400,
    width = 'auto'
)

##############
# WIZARD FUN #
##############

output$plotCascade_wizard <- renderPlot({
    t5 <- ExtractCascadeData(251) # t5 = (5 / 0.02) + 1 [t0]

    p.col <- rev(brewer.pal(9,"Blues")[3:8])

    ggplot(t5, aes(def, res)) +
    geom_bar(aes(fill = def), position = 'dodge', stat = 'identity') +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1), labels = percent, expand = c(0, 0)) +
    scale_fill_manual(values = p.col) +
    ggtitle("Care Cascade in 2020") +
    theme_classic() +
    theme(title = element_text(size = 18)) +
    theme(axis.title = element_blank()) +
    theme(axis.text.x = element_text(size = 15)) +
    theme(axis.text.y = element_text(size = 18)) +
    theme(legend.position = "none")
    },
    height = 'auto',
    width = 'auto'
)
