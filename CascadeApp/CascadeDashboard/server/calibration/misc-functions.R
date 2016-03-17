# Simple function calls for various permutations of the model

RunBaselineModel <- function() {

    KenyaData <- GetMasterDataSet("Kenya")

    time <- seq(0, 5, 1)
    p <- parameters(
        prop_preART_500    = KenyaData[["cd4"]][1,"prop.Off.ART.500"][[1]],
        prop_preART_350500 = KenyaData[["cd4"]][1,"prop.Off.ART.350500"][[1]],
        prop_preART_250350 = KenyaData[["cd4"]][1,"prop.Off.ART.250350"][[1]],
        prop_preART_200250 = KenyaData[["cd4"]][1,"prop.Off.ART.200250"][[1]],
        prop_preART_100200 = KenyaData[["cd4"]][1,"prop.Off.ART.100200"][[1]],
        prop_preART_50100  = KenyaData[["cd4"]][1,"prop.Off.ART.50100"][[1]],
        prop_preART_50     = KenyaData[["cd4"]][1,"prop.Off.ART.50"][[1]],
        t_1 = ConvertYear(KenyaData[["treatment_guidelines"]][["more500"]]),
        t_2 = ConvertYear(KenyaData[["treatment_guidelines"]][["less500"]]),
        t_3 = ConvertYear(KenyaData[["treatment_guidelines"]][["less350"]]),
        t_4 = ConvertYear(KenyaData[["treatment_guidelines"]][["less250"]]),
        t_5 = ConvertYear(KenyaData[["treatment_guidelines"]][["less200"]])
        )
    y <- GetCalibInitial(p, KenyaData)
    i <- incidence(as.double(KenyaData[["incidence"]]))


    # THE GUTS #
    result <- CallCalibModel(time, y, p, i)
    df <- AssembleComparisonDataFrame(country = "Kenya", model = result, data = KenyaData)
    original <- SSE(df)
    # THE END #

    # p1 <- ggplot(dplyr::filter(error, indicator == "PLHIV"), aes(x = year, y = value, group = source)) +
    #     geom_line() + geom_point(aes(color = indicator, shape = source), size = 3)

    # p2 <- ggplot(dplyr::filter(error, indicator == "PLHIV Diagnosed"), aes(x = year, y = value, group = source)) +
    #     geom_line() + geom_point(aes(color = indicator, shape = source), size = 3)

    # p3 <- ggplot(dplyr::filter(error, indicator == "PLHIV in Care"), aes(x = year, y = value, group = source)) +
    #     geom_line() + geom_point(aes(color = indicator, shape = source), size = 3)

    # p4 <- ggplot(dplyr::filter(error, indicator == "PLHIV on ART"), aes(x = year, y = value, group = source)) +
    #     geom_line() + geom_point(aes(color = indicator, shape = source), size = 3)

    # graphics.off()
    # quartz.options(w = 10, h = 5)
    # gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)


    # Without Error in data.frame #
    error2 <- dplyr::filter(original, source != "error")
    p1 <- ggplot(dplyr::filter(error2, indicator == "PLHIV"), aes(x = year, y = value, group = source)) +
        geom_line() + geom_point(aes(color = source), size = 3) +
        ggtitle("PLHIV")

    p2 <- ggplot(dplyr::filter(error2, indicator == "PLHIV Diagnosed"), aes(x = year, y = value, group = source)) +
        geom_line() + geom_point(aes(color = source), size = 3) +
        ggtitle("PLHIV Diagnosed")

    p3 <- ggplot(dplyr::filter(error2, indicator == "PLHIV in Care"), aes(x = year, y = value, group = source)) +
        geom_line() + geom_point(aes(color = source), size = 3) +
        ggtitle("PLHIV in Care")

    p4 <- ggplot(dplyr::filter(error2, indicator == "PLHIV on ART"), aes(x = year, y = value, group = source)) +
        geom_line() + geom_point(aes(color = source), size = 3) +
        ggtitle("PLHIV on ART")

    # graphics.off()
    # quartz.options(w = 10, h = 5)
    gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)
}

RunCalibration <- function(iterations) {
    # iterations = 100
    KenyaData <- GetMasterDataSet("Kenya")

    time <- seq(0, 5, 1)
    p <- parameters(
        prop_preART_500    = KenyaData[["cd4"]][1,"prop.Off.ART.500"][[1]],
        prop_preART_350500 = KenyaData[["cd4"]][1,"prop.Off.ART.350500"][[1]],
        prop_preART_250350 = KenyaData[["cd4"]][1,"prop.Off.ART.250350"][[1]],
        prop_preART_200250 = KenyaData[["cd4"]][1,"prop.Off.ART.200250"][[1]],
        prop_preART_100200 = KenyaData[["cd4"]][1,"prop.Off.ART.100200"][[1]],
        prop_preART_50100  = KenyaData[["cd4"]][1,"prop.Off.ART.50100"][[1]],
        prop_preART_50     = KenyaData[["cd4"]][1,"prop.Off.ART.50"][[1]],
        t_1 = ConvertYear(KenyaData[["treatment_guidelines"]][["more500"]]),
        t_2 = ConvertYear(KenyaData[["treatment_guidelines"]][["less500"]]),
        t_3 = ConvertYear(KenyaData[["treatment_guidelines"]][["less350"]]),
        t_4 = ConvertYear(KenyaData[["treatment_guidelines"]][["less250"]]),
        t_5 = ConvertYear(KenyaData[["treatment_guidelines"]][["less200"]])
        )
    y <- GetCalibInitial(p, KenyaData)
    i <- incidence(as.double(KenyaData[["incidence"]]))

    min_term = 5
    max_term = 0.1

    parRange <- data.frame(
        min = c(
            rho     = p[["Rho"]]     * min_term,
            epsilon = p[["Epsilon"]] * min_term,
            kappa   = p[["Kappa"]]   * min_term,
            gamma   = p[["Gamma"]]   * min_term,
            theta   = p[["Theta"]]   * min_term,
            omega   = p[["Omega"]]   * min_term,
            mu      = p[["Mu"]]      * 1,
            q       = if(p[["q"]] * min_term > 1) {1} else {p[["q"]] * min_term}
        ),
        max = c(
            rho     = p[["Rho"]]     * max_term,
            epsilon = p[["Epsilon"]] * max_term,
            kappa   = p[["Kappa"]]   * max_term,
            gamma   = p[["Gamma"]]   * max_term,
            theta   = p[["Theta"]]   * max_term,
            omega   = p[["Omega"]]   * max_term,
            mu      = p[["Mu"]]      * 0,
            q       = if(p[["q"]] * max_term > 1) {1} else {p[["q"]] * max_term}
        )
    )
    parRange

    lhs <- FME::Latinhyper(parRange, num = iterations)

    error <- c()
    for(k in 1:dim(lhs)[1]) {
        # print(k)
        p[["Rho"]]     <- lhs[,"rho"][k]
        p[["Epsilon"]] <- lhs[,"epsilon"][k]
        p[["Kappa"]]   <- lhs[,"kappa"][k]
        p[["Gamma"]]   <- lhs[,"gamma"][k]
        p[["Theta"]]   <- lhs[,"theta"][k]
        p[["Omega"]]   <- lhs[,"omega"][k]
        p[["Mu"]]      <- lhs[,"mu"][k]
        p[["q"]]       <- lhs[,"q"][k]

        out <- SSE(AssembleComparisonDataFrame(country = "Kenya", model = CallCalibModel(time, y, p, i), data = KenyaData))
        error[k] <- sum(out[out$source == "error","value"])
    }

    best_10percent <- order(error)[1:(iterations * 0.1)]

    out <- c()
    for(l in 1:(iterations * 0.1)) {

        p[["Rho"]]     <- lhs[,"rho"][best_10percent[l]]
        p[["Epsilon"]] <- lhs[,"epsilon"][best_10percent[l]]
        p[["Kappa"]]   <- lhs[,"kappa"][best_10percent[l]]
        p[["Gamma"]]   <- lhs[,"gamma"][best_10percent[l]]
        p[["Theta"]]   <- lhs[,"theta"][best_10percent[l]]
        p[["Omega"]]   <- lhs[,"omega"][best_10percent[l]]
        p[["Mu"]]      <- lhs[,"mu"][best_10percent[l]]
        p[["q"]]       <- lhs[,"q"][best_10percent[l]]

        iOut <- SSE(AssembleComparisonDataFrame(country = "Kenya", model = CallCalibModel(time, y, p, i), data = KenyaData))
        out <- rbind(out, iOut)
    }

    # Need to create a dataframe containg all parameter values, THEN max and minimum on them.
    # Just apply over a col (MARGIN = 2)

    pOut <- data.frame(
        rho = 0,
        epsilon = 0,
        kappa = 0,
        gamma = 0,
        theta = 0,
        omega = 0,
        mu = 0,
        q = 0
        )

    for(l in 1:(iterations * 0.1)) {
        # Fill that shit out.
        pOut[l,"rho"]     = lhs[,"rho"][best_10percent[l]]
        pOut[l,"epsilon"] = lhs[,"epsilon"][best_10percent[l]]
        pOut[l,"kappa"]   = lhs[,"kappa"][best_10percent[l]]
        pOut[l,"gamma"]   = lhs[,"gamma"][best_10percent[l]]
        pOut[l,"theta"]   = lhs[,"theta"][best_10percent[l]]
        pOut[l,"omega"]   = lhs[,"omega"][best_10percent[l]]
        pOut[l,"mu"]      = lhs[,"mu"][best_10percent[l]]
        pOut[l,"q"]       = lhs[,"q"][best_10percent[l]]
    }


    # Calculate the min and max of the parameters simulated.
    param <- data.frame(
        min = apply(pOut, 2, min),
        max = apply(pOut, 2, max)
        )
    # These need to be returned to box() in the app.

    outdata <- dplyr::filter(out, source == "data")

    # Find Minimums & Maximums & Mean of data.
    out_details <- AppendMinMaxMean(out[out$source == "model",])

    # Create some pretty output plots
    p1 <- ggplot(data = outdata[outdata$indicator == "PLHIV",], aes(x = year, y = value, group = source)) +
        geom_ribbon(data = out_details[out_details$indicator == "PLHIV",], aes(x = year, ymin = min, ymax = max, group = source), fill = "grey70") +
        geom_line() + geom_point(aes(color = source), size = 3) +
        ggtitle("PLHIV", subtitle = "Points are data, shading shows upper and lower model estimates") +
        theme(legend.position = "none", text = element_text(family = "OpenSans-CondensedLight"))

    p2 <- ggplot(data = outdata[outdata$indicator == "PLHIV Diagnosed",], aes(x = year, y = value, group = source)) +
        geom_ribbon(data = out_details[out_details$indicator == "PLHIV Diagnosed",], aes(x = year, ymin = min, ymax = max, group = source), fill = "grey70") +
        geom_line() + geom_point(aes(color = source), size = 3) +
        ggtitle("PLHIV Diagnosed", subtitle = "Points are data, shading shows upper and lower model estimates") +
        theme(legend.position = "none", text = element_text(family = "OpenSans-CondensedLight"))

    p3 <- ggplot(data = outdata[outdata$indicator == "PLHIV in Care",], aes(x = year, y = value, group = source)) +
        geom_ribbon(data = out_details[out_details$indicator == "PLHIV in Care",], aes(x = year, ymin = min, ymax = max, group = source), fill = "grey70") +
        geom_line() + geom_point(aes(color = source), size = 3) +
        ggtitle("PLHIV in Care", subtitle = "Points are data, shading shows upper and lower model estimates") +
        theme(legend.position = "none", text = element_text(family = "OpenSans-CondensedLight"))

    p4 <- ggplot(data = outdata[outdata$indicator == "PLHIV on ART",], aes(x = year, y = value, group = source)) +
        geom_ribbon(data = out_details[out_details$indicator == "PLHIV on ART",], aes(x = year, ymin = min, ymax = max, group = source), fill = "grey70") +
        geom_line() + geom_point(aes(color = source), size = 3) +
        ggtitle("PLHIV on ART", subtitle = "Points are data, shading shows upper and lower model estimates") +
        theme(legend.position = "none", text = element_text(family = "OpenSans-CondensedLight"))

    p5 <- ggplot(out_details[out_details$year == 2010,][1:5,], aes(x = indicator, y = mean)) +
        geom_bar(aes(fill = indicator), stat = "identity") +
        ggtitle("Cascade in 2010") +
        theme_classic() +
        theme(legend.position = "none", text = element_text(family = "OpenSans-CondensedLight"))

    p6 <- ggplot(out_details[out_details$year == 2015,][1:5,], aes(x = indicator, y = mean)) +
        geom_bar(aes(fill = indicator), stat = "identity") +
        geom_errorbar(mapping = aes(x = indicator, ymin = min, ymax = max), width = 0.2, size = 0.5) +
        ggtitle("Cascade in 2015", subtitle = "Error bars illustrate result ranges from best 10% of model fits") +
        theme_classic() +
        theme(legend.position = "none", text = element_text(family = "OpenSans-CondensedLight"))

    gridExtra::grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 2, nrow = 3)

    # Return parameter data.frame that contains the max and min values of all parameters used in the simulations.
    # Also a ggplot (four plots) should be printed to the screen.
    return(param)
}

AppendMinMaxMean <- function(data) {
    uniqueIndicators <- unique(data$indicator)
    uniqueYear <- unique(data$year)

    for(m in 1:length(uniqueIndicators)) {
        for(l in 1:length(uniqueYear)) {
            data[data$year == uniqueYear[l] & data$indicator == uniqueIndicators[m],"min"] <- min(data[data$year == uniqueYear[l] & data$indicator == uniqueIndicators[m],"value"])
            data[data$year == uniqueYear[l] & data$indicator == uniqueIndicators[m],"max"] <- max(data[data$year == uniqueYear[l] & data$indicator == uniqueIndicators[m],"value"])
            data[data$year == uniqueYear[l] & data$indicator == uniqueIndicators[m],"mean"] <- mean(data[data$year == uniqueYear[l] & data$indicator == uniqueIndicators[m],"value"])
            }
    }
    data
}
