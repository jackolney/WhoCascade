# Simple function calls for various permutations of the model

RunBaselineModel <- function(data) {

    # Set important parameters
    time <- seq(0, 5, 1)
    p <- parameters(
        prop_preART_500    = data[["cd4"]][1,"prop.Off.ART.500"][[1]],
        prop_preART_350500 = data[["cd4"]][1,"prop.Off.ART.350500"][[1]],
        prop_preART_250350 = data[["cd4"]][1,"prop.Off.ART.250350"][[1]],
        prop_preART_200250 = data[["cd4"]][1,"prop.Off.ART.200250"][[1]],
        prop_preART_100200 = data[["cd4"]][1,"prop.Off.ART.100200"][[1]],
        prop_preART_50100  = data[["cd4"]][1,"prop.Off.ART.50100"][[1]],
        prop_preART_50     = data[["cd4"]][1,"prop.Off.ART.50"][[1]],
        t_1 = ConvertYear(data[["treatment_guidelines"]][["more500"]]),
        t_2 = ConvertYear(data[["treatment_guidelines"]][["less500"]]),
        t_3 = ConvertYear(data[["treatment_guidelines"]][["less350"]]),
        t_4 = ConvertYear(data[["treatment_guidelines"]][["less250"]]),
        t_5 = ConvertYear(data[["treatment_guidelines"]][["less200"]])
        )
    y <- GetCalibInitial(p, data)
    i <- incidence(as.double(data[["incidence"]]))

    # Run C++ model
    result <- CallCalibModel(time, y, p, i)

    # Assemble output data.frame
    df <- AssembleComparisonDataFrame(country = "Kenya", model = result, data = data)

    # Calculate mean squared error between model and data
    original <- SSE(df)

    # Build Plots
    # BuildBaselinePlots(original)
    BuildBaselineErrorPlots(original)
}

RunCalibration <- function(data, iterations = 100) {

    # iterations = 100

    # Set important parameters
    time <- seq(0, 5, 1)
    p <- parameters(
        prop_preART_500    = data[["cd4"]][1,"prop.Off.ART.500"][[1]],
        prop_preART_350500 = data[["cd4"]][1,"prop.Off.ART.350500"][[1]],
        prop_preART_250350 = data[["cd4"]][1,"prop.Off.ART.250350"][[1]],
        prop_preART_200250 = data[["cd4"]][1,"prop.Off.ART.200250"][[1]],
        prop_preART_100200 = data[["cd4"]][1,"prop.Off.ART.100200"][[1]],
        prop_preART_50100  = data[["cd4"]][1,"prop.Off.ART.50100"][[1]],
        prop_preART_50     = data[["cd4"]][1,"prop.Off.ART.50"][[1]],
        t_1 = ConvertYear(data[["treatment_guidelines"]][["more500"]]),
        t_2 = ConvertYear(data[["treatment_guidelines"]][["less500"]]),
        t_3 = ConvertYear(data[["treatment_guidelines"]][["less350"]]),
        t_4 = ConvertYear(data[["treatment_guidelines"]][["less250"]]),
        t_5 = ConvertYear(data[["treatment_guidelines"]][["less200"]])
        )
    y <- GetCalibInitial(p, data)
    i <- incidence(as.double(data[["incidence"]]))

    ## Parameter Sampling
    parRange <- DefineParmRange(param = p, min = 5, max = 0.1)

    # Use Latin Hypercube Sampling to randomly sample from parRange n times
    lhs <- FME::Latinhyper(parRange, num = iterations)

    ## For each draw, update parameter vector (p), run model, calculate error and store it.
    # Haven't put into a function as probably too many arguements.
    error <- c()
    for(k in 1:dim(lhs)[1]) {

        p[["Rho"]]     <- lhs[,"rho"][k]
        p[["Epsilon"]] <- lhs[,"epsilon"][k]
        p[["Kappa"]]   <- lhs[,"kappa"][k]
        p[["Gamma"]]   <- lhs[,"gamma"][k]
        p[["Theta"]]   <- lhs[,"theta"][k]
        p[["Omega"]]   <- lhs[,"omega"][k]
        p[["Mu"]]      <- lhs[,"mu"][k]
        p[["p"]]       <- lhs[,"p"][k]
        p[["q"]]       <- lhs[,"q"][k]

        out <- SSE(AssembleComparisonDataFrame(country = "Kenya", model = CallCalibModel(time, y, p, i), data = data))
        error[k] <- sum(out[out$source == "error","value"])
    }

    # Order sum of total error from lowest to highest and pick the lowest 10%
    bestTenPercent <- order(error)[1:(iterations * 0.1)]

    ## For the best 10%, update the parameter vector (p), re-run simulations and store results
    # Faster than storing ALL results in the first place (I think)
    out <- c()
    for(l in 1:(iterations * 0.1)) {

        p[["Rho"]]     <- lhs[,"rho"][bestTenPercent[l]]
        p[["Epsilon"]] <- lhs[,"epsilon"][bestTenPercent[l]]
        p[["Kappa"]]   <- lhs[,"kappa"][bestTenPercent[l]]
        p[["Gamma"]]   <- lhs[,"gamma"][bestTenPercent[l]]
        p[["Theta"]]   <- lhs[,"theta"][bestTenPercent[l]]
        p[["Omega"]]   <- lhs[,"omega"][bestTenPercent[l]]
        p[["Mu"]]      <- lhs[,"mu"][bestTenPercent[l]]
        p[["p"]]       <- lhs[,"p"][bestTenPercent[l]]
        p[["q"]]       <- lhs[,"q"][bestTenPercent[l]]

        iOut <- SSE(AssembleComparisonDataFrame(country = "Kenya", model = CallCalibModel(time, y, p, i), data = data))
        out <- rbind(out, iOut)
    }

    # Create data.frame to hold all parameter values used by top 10%
    pOut <- FillParValues(samples = lhs, positions = bestTenPercent, iterations = iterations)

    # Calculate min and max values used by parameter set
    param <- data.frame(
        min = apply(pOut, 2, min),
        max = apply(pOut, 2, max)
    )

    # Plots
    BuildCalibrationPlots(data = out, originalData = data)

    # Return min and max values used for all parameters.
    return(param)
}
