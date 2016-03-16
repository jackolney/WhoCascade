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

    # For a given length of the lhs (sample):
        # - Update the parameter() [dont need to update initials or incidence vectors]
        # - pass parameters() to CallModel -> AssembleDF -> SSE.
        # - TIME THAT SHIT!
    # p[["Mu"]] <- 0
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
    # plot(error)

    a <- order(error)[1:(iterations * 0.1)]

    out <- c()
    for(l in 1:(iterations * 0.1)) {

        p[["Rho"]]     <- lhs[,"rho"][a[l]]
        p[["Epsilon"]] <- lhs[,"epsilon"][a[l]]
        p[["Kappa"]]   <- lhs[,"kappa"][a[l]]
        p[["Gamma"]]   <- lhs[,"gamma"][a[l]]
        p[["Theta"]]   <- lhs[,"theta"][a[l]]
        p[["Omega"]]   <- lhs[,"omega"][a[l]]
        p[["Mu"]]      <- lhs[,"mu"][a[l]]
        p[["q"]]       <- lhs[,"q"][a[l]]

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
        pOut[l,"rho"]     = lhs[,"rho"][a[l]]
        pOut[l,"epsilon"] = lhs[,"epsilon"][a[l]]
        pOut[l,"kappa"]   = lhs[,"kappa"][a[l]]
        pOut[l,"gamma"]   = lhs[,"gamma"][a[l]]
        pOut[l,"theta"]   = lhs[,"theta"][a[l]]
        pOut[l,"omega"]   = lhs[,"omega"][a[l]]
        pOut[l,"mu"]      = lhs[,"mu"][a[l]]
        pOut[l,"q"]       = lhs[,"q"][a[l]]
    }


    # Calculate the min and max of the parameters simulated.
    param <- data.frame(
        min = apply(pOut, 2, min),
        max = apply(pOut, 2, max)
        )
    # These need to be returned to box() in the app.

    # Find Minimums & Maximums
    # OF ONLY MODEL
    # FOR EACH INDICATORS
    AppendMinMax <- function(data) {
        uniqueIndicators <- unique(data$indicator)
        uniqueYear <- unique(data$year)

        for(m in 1:length(uniqueIndicators)) {
            for(l in 1:length(uniqueYear)) {
                data[data$year == uniqueYear[l] & data$indicator == uniqueIndicators[m],"min"] <- min(data[data$year == uniqueYear[l] & data$indicator == uniqueIndicators[m],]$value)
                data[data$year == uniqueYear[l] & data$indicator == uniqueIndicators[m],"max"] <- max(data[data$year == uniqueYear[l] & data$indicator == uniqueIndicators[m],]$value)
                }
        }
        data
    }

    outdata <- dplyr::filter(out, source == "data")

    out_min_max <- AppendMinMax(out[out$source == "model",])

    p1 <- ggplot(data = outdata[outdata$indicator == "PLHIV",], aes(x = year, y = value, group = source)) +
        geom_ribbon(data = out_min_max[out_min_max$indicator == "PLHIV",], aes(x = year, ymin = min, ymax = max, group = source), fill = "grey70") +
        geom_line() + geom_point(aes(color = source), size = 3) +
        ggtitle("PLHIV")

    p2 <- ggplot(data = outdata[outdata$indicator == "PLHIV Diagnosed",], aes(x = year, y = value, group = source)) +
        geom_ribbon(data = out_min_max[out_min_max$indicator == "PLHIV Diagnosed",], aes(x = year, ymin = min, ymax = max, group = source), fill = "grey70") +
        geom_line() + geom_point(aes(color = source), size = 3) +
        ggtitle("PLHIV Diagnosed")


    p3 <- ggplot(data = outdata[outdata$indicator == "PLHIV in Care",], aes(x = year, y = value, group = source)) +
        geom_ribbon(data = out_min_max[out_min_max$indicator == "PLHIV in Care",], aes(x = year, ymin = min, ymax = max, group = source), fill = "grey70") +
        geom_line() + geom_point(aes(color = source), size = 3) +
        ggtitle("PLHIV in Care")


    p4 <- ggplot(data = outdata[outdata$indicator == "PLHIV on ART",], aes(x = year, y = value, group = source)) +
        geom_ribbon(data = out_min_max[out_min_max$indicator == "PLHIV on ART",], aes(x = year, ymin = min, ymax = max, group = source), fill = "grey70") +
        geom_line() + geom_point(aes(color = source), size = 3) +
        ggtitle("PLHIV on ART")


    gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)

    # Return parameter data.frame that contains the max and min values of all parameters used in the simulations.
    # Also a ggplot (four plots) should be printed to the screen.
    return(param)
}
