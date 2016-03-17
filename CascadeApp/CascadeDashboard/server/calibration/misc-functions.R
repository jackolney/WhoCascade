# Simple function calls for various permutations of the model

RunBaselineModel <- function() {

    # Get country master data set
    KenyaData <- GetMasterDataSet("Kenya")

    # Set important parameters
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

    # Run C++ model
    result <- CallCalibModel(time, y, p, i)

    # Assemble output data.frame
    df <- AssembleComparisonDataFrame(country = "Kenya", model = result, data = KenyaData)

    # Calculate mean squared error between model and data
    original <- SSE(df)

    # A plot of error (commented out)
    # p1 <- ggplot(original[original$indicator == "PLHIV",], aes(x = year, y = value, group = source)) +
    #     geom_line() + geom_point(aes(color = indicator, shape = source), size = 3)

    # p2 <- ggplot(original[original$indicator == "PLHIV Diagnosed",], aes(x = year, y = value, group = source)) +
    #     geom_line() + geom_point(aes(color = indicator, shape = source), size = 3)

    # p3 <- ggplot(original[original$indicator == "PLHIV in Care",], aes(x = year, y = value, group = source)) +
    #     geom_line() + geom_point(aes(color = indicator, shape = source), size = 3)

    # p4 <- ggplot(original[original$indicator == "PLHIV on ART",], aes(x = year, y = value, group = source)) +
    #     geom_line() + geom_point(aes(color = indicator, shape = source), size = 3)

    # gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)

    # col_pairs <- brewer.pal(12, "Paired")
    # cols <- c(test[4],test[8],test[6])
    cols <- c(ggColorHue(10)[1],ggColorHue(10)[2],ggColorHue(10)[4])
    names(cols) <- c("red", "amber", "green")
    mycol <- scale_colour_manual(name = "weight", values = cols)

    # A plot of model vs. data
    error2 <- original[original$source != "error",]
    p1 <- ggplot(error2[error2$indicator == "PLHIV",], aes(x = year, y = value, group = weight)) +
        geom_line() + geom_point(aes(color = weight), size = 3) +
        mycol +
        ggtitle("PLHIV")

    p2 <- ggplot(error2[error2$indicator == "PLHIV Diagnosed",], aes(x = year, y = value, group = weight)) +
        geom_line() + geom_point(aes(color = weight), size = 3) +
        mycol +
        ggtitle("PLHIV Diagnosed")

    p3 <- ggplot(error2[error2$indicator == "PLHIV in Care",], aes(x = year, y = value, group = weight)) +
        geom_line() + geom_point(aes(color = weight), size = 3) +
        mycol +
        ggtitle("PLHIV in Care")

    p4 <- ggplot(error2[error2$indicator == "PLHIV on ART",], aes(x = year, y = value, group = weight)) +
        geom_line() + geom_point(aes(color = weight), size = 3) +
        mycol +
        ggtitle("PLHIV on ART")

    gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)
}

RunCalibration <- function(iterations = 100) {

    # iterations = 100
    # Load country master dataset
    KenyaData <- GetMasterDataSet("Kenya")

    # Set important parameters
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

    ## Parameter Sampling
    # Pick maximum and minimum terms
    min_term = 5
    max_term = 0.1

    # Put together max and min parRange data.frame
    parRange <- data.frame(
        min = c(
            rho     = p[["Rho"]]     * min_term,
            epsilon = p[["Epsilon"]] * min_term,
            kappa   = p[["Kappa"]]   * min_term,
            gamma   = p[["Gamma"]]   * min_term,
            theta   = p[["Theta"]]   * min_term,
            omega   = p[["Omega"]]   * min_term,
            mu      = p[["Mu"]]      * 1,
            p       = if(p[["p"]] * min_term > 1) {1} else {p[["p"]] * min_term},
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
            p       = if(p[["p"]] * max_term > 1) {1} else {p[["p"]] * max_term},
            q       = if(p[["q"]] * max_term > 1) {1} else {p[["q"]] * max_term}
        )
    )
    parRange

    # Use Latin Hypercube Sampling to randomly sample from parRange n times
    lhs <- FME::Latinhyper(parRange, num = iterations)

    # For each draw, update parameter vector (p), run model, calculate error and store it.
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
        p[["p"]]       <- lhs[,"p"][k]
        p[["q"]]       <- lhs[,"q"][k]

        out <- SSE(AssembleComparisonDataFrame(country = "Kenya", model = CallCalibModel(time, y, p, i), data = KenyaData))
        error[k] <- sum(out[out$source == "error","value"])
    }

    # Order sum of total error from lowest to highest and pick the lowest 10%
    best_10percent <- order(error)[1:(iterations * 0.1)]

    ## For the best 10%, update the parameter vector (p), re-run simulations and store results
    # Faster than storing ALL results in the first place (I think)
    out <- c()
    for(l in 1:(iterations * 0.1)) {

        p[["Rho"]]     <- lhs[,"rho"][best_10percent[l]]
        p[["Epsilon"]] <- lhs[,"epsilon"][best_10percent[l]]
        p[["Kappa"]]   <- lhs[,"kappa"][best_10percent[l]]
        p[["Gamma"]]   <- lhs[,"gamma"][best_10percent[l]]
        p[["Theta"]]   <- lhs[,"theta"][best_10percent[l]]
        p[["Omega"]]   <- lhs[,"omega"][best_10percent[l]]
        p[["Mu"]]      <- lhs[,"mu"][best_10percent[l]]
        p[["p"]]       <- lhs[,"p"][best_10percent[l]]
        p[["q"]]       <- lhs[,"q"][best_10percent[l]]

        iOut <- SSE(AssembleComparisonDataFrame(country = "Kenya", model = CallCalibModel(time, y, p, i), data = KenyaData))
        out <- rbind(out, iOut)
    }

    # Create data.frame to hold all parameter values used by top 10%
    pOut <- data.frame(
        rho = 0,
        epsilon = 0,
        kappa = 0,
        gamma = 0,
        theta = 0,
        omega = 0,
        mu = 0,
        p = 0,
        q = 0
        )

    # Loop through all iterations and fill out pOut
    for(l in 1:(iterations * 0.1)) {
        pOut[l,"rho"]     = lhs[,"rho"][best_10percent[l]]
        pOut[l,"epsilon"] = lhs[,"epsilon"][best_10percent[l]]
        pOut[l,"kappa"]   = lhs[,"kappa"][best_10percent[l]]
        pOut[l,"gamma"]   = lhs[,"gamma"][best_10percent[l]]
        pOut[l,"theta"]   = lhs[,"theta"][best_10percent[l]]
        pOut[l,"omega"]   = lhs[,"omega"][best_10percent[l]]
        pOut[l,"mu"]      = lhs[,"mu"][best_10percent[l]]
        pOut[l,"p"]       = lhs[,"p"][best_10percent[l]]
        pOut[l,"q"]       = lhs[,"q"][best_10percent[l]]
    }

    # Calculate min and max values used by parameter set
    param <- data.frame(
        min = apply(pOut, 2, min),
        max = apply(pOut, 2, max)
    )

    # Subset data to show only 'data'
    outdata <- out[out$source == "data",]

    # Find Minimums & Maximums & Mean of data.
    out_details <- AppendMinMaxMean(out[out$source == "model",])

    # Set Colors
    cols <- c(ggColorHue(10)[1],ggColorHue(10)[2],ggColorHue(10)[4])
    names(cols) <- c("red", "amber", "green")
    mycol <- scale_colour_manual(name = "weight", values = cols)

    # Create some pretty output plots
    p1 <- ggplot(data = outdata[outdata$indicator == "PLHIV",], aes(x = year, y = value, group = weight)) +
        geom_ribbon(data = out_details[out_details$indicator == "PLHIV",], aes(x = year, ymin = min, ymax = max, group = weight), fill = "grey70") +
        geom_line() + geom_point(aes(color = weight), size = 3) +
        mycol +
        ggtitle("PLHIV", subtitle = "Points are data, shading shows upper and lower model estimates") +
        theme(legend.position = "none", text = element_text(family = "OpenSans-CondensedLight"))

    p2 <- ggplot(data = outdata[outdata$indicator == "PLHIV Diagnosed",], aes(x = year, y = value, group = weight)) +
        geom_ribbon(data = out_details[out_details$indicator == "PLHIV Diagnosed",], aes(x = year, ymin = min, ymax = max, group = weight), fill = "grey70") +
        geom_line() + geom_point(aes(color = weight), size = 3) +
        mycol +
        ggtitle("PLHIV Diagnosed", subtitle = "Points are data, shading shows upper and lower model estimates") +
        theme(legend.position = "none", text = element_text(family = "OpenSans-CondensedLight"))

    p3 <- ggplot(data = outdata[outdata$indicator == "PLHIV in Care",], aes(x = year, y = value, group = weight)) +
        geom_ribbon(data = out_details[out_details$indicator == "PLHIV in Care",], aes(x = year, ymin = min, ymax = max, group = weight), fill = "grey70") +
        geom_line() + geom_point(aes(color = weight), size = 3) +
        mycol +
        ggtitle("PLHIV in Care", subtitle = "Points are data, shading shows upper and lower model estimates") +
        theme(legend.position = "none", text = element_text(family = "OpenSans-CondensedLight"))

    p4 <- ggplot(data = outdata[outdata$indicator == "PLHIV on ART",], aes(x = year, y = value, group = weight)) +
        geom_ribbon(data = out_details[out_details$indicator == "PLHIV on ART",], aes(x = year, ymin = min, ymax = max, group = weight), fill = "grey70") +
        geom_line() + geom_point(aes(color = weight), size = 3) +
        mycol +
        ggtitle("PLHIV on ART", subtitle = "Points are data, shading shows upper and lower model estimates") +
        theme(legend.position = "none", text = element_text(family = "OpenSans-CondensedLight"))

    p5 <- ggplot(out_details[out_details$year == 2010,][1:5,], aes(x = indicator, y = mean)) +
        geom_bar(aes(fill = indicator), stat = "identity") +
        ggtitle("Cascade in 2010") +
        theme_classic() +
        theme(legend.position = "none",
            text = element_text(family = "OpenSans-CondensedLight"),
            axis.title = element_blank())

    p6 <- ggplot(out_details[out_details$year == 2015,][1:5,], aes(x = indicator, y = mean)) +
        geom_bar(aes(fill = indicator), stat = "identity") +
        geom_errorbar(mapping = aes(x = indicator, ymin = min, ymax = max), width = 0.2, size = 0.5) +
        geom_point(data = KenyaData[["calib"]][KenyaData[["calib"]]$year == 2015 & KenyaData[["calib"]]$indicator != "PLHIV Retained",], aes(x = indicator, y = value), color = "black", size = 2) +
        ggtitle("Cascade in 2015", subtitle = "Error bars illustrate result ranges from best 10% of model fits, points are data") +
        theme_classic() +
        theme(legend.position = "none",
            text = element_text(family = "OpenSans-CondensedLight"),
            axis.title = element_blank())

    # Throw together in a big plot window
    gridExtra::grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 2, nrow = 3)

    # Return min and max values used for all parameters.
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

ggColorHue <- function(n) {
    hues = seq(15, 375, length = n+1)
    hcl(h = hues, l = 65, c = 100)[1:n]
}
