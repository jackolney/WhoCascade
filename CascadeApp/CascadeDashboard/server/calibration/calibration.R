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
    withProgress(message = 'Running Calibration:', value = 0, {

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
        setProgress(value = 0/1, detail = "Defining parameter space.")
        parRange <- DefineParmRange(param = p, min = 5, max = 0.1)

        # Use Latin Hypercube Sampling to randomly sample from parRange n times
        setProgress(value = 0/1, detail = "LHS 1000 parameter sets.")
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
            setProgress(value = k/dim(lhs)[1], detail = paste("Run",k))
        }

        # Order sum of total error from lowest to highest and pick the lowest 10%
        setProgress(value = 0/1, detail = "Sampling best runs.")
        bestTenPercent <- order(error)[1:(iterations * 0.1)]

        ## For the best 10%, update the parameter vector (p), re-run simulations and store results
        # Faster than storing ALL results in the first place (I think)
        CalibOut <<- c()
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
            CalibOut <<- rbind(out, iOut)
            setProgress(value = l/(iterations * 0.1), detail = paste("Resample",l))
        }

        # Create data.frame to hold all parameter values used by top 10%
        CalibParamOut <<- FillParValues(samples = lhs, positions = bestTenPercent, iterations = iterations)

        # Calculate min and max values used by parameter set
        CalibParamMaxMin <<- data.frame(
            min = apply(CalibParamOut, 2, min),
            max = apply(CalibParamOut, 2, max)
        )
        # Plots
        # Then comment this out and call it elsewhere.
        setProgress(value = 1, detail = "Building figures.")
        BuildCalibrationPlots(data = CalibOut, originalData = data)
    })

    # Return min and max values used for all parameters.
    return(CalibParamMaxMin)
}
