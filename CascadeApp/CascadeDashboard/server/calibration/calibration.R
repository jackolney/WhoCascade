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

RunCalibration <- function(data, maxIterations, maxError, limit) {
    # limit = 100
    # maxIterations = 1e4
    # maxError = 1e11

    # maxError entered as a string so must be converted
    maxError <- as.numeric(maxError)

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
        # y <- GetCalibInitial(p, data)
        i <- incidence(as.double(data[["incidence"]]))

        ## Parameter Sampling
        setProgress(value = 0 / 1, detail = "Defining parameter space.")
        intParRange <- DefineParmRange(param = p, min = 5, max = 0.1)

        # Need a function here that over-rides the ranges if a value has been entered by the user.
        parRange <- UserOverRide(intParRange)
        print(parRange)

        # Use Latin Hypercube Sampling to randomly sample from parRange n times
        lhs <- FME::Latinhyper(parRange, num = maxIterations)

        # Sample initial states
        # Need a vector containing all the initial states too and their max / min ranges too.
        # We COULD account for how reliable the 2010 data is in our estimates?
        # This function, needs to be clever enough to calculate and fill in any gaps in the data.
        initRange <- DefineInitRange(data = data, min = 0.9, max = 1.1)

        # LHS Sample
        # Scope to modify this to pick normally distributed values with a mean of 2010, sd = ??
        lhsInitial <- FME::Latinhyper(initRange, num = maxIterations)

## HERE (TUESDAY EVENING)
# We now need to take check that ALL samples make sense.
# Throw away any that do not, resample.
# ONLY SIMULATE samples that are SENSICAL (n = 1e4) of those

        head(lhsInitial)

# Create a function like the below.
        # FindSense(samples = lhsInitial)
# Should return all sensical results.

        lhsInitial[1,]

        lhsInitial[[1,1]] - lhsInitial[[1,2]]
        lhsInitial[[1,2]] - lhsInitial[[1,3]]
        lhsInitial[[1,3]] - lhsInitial[[1,4]]

        ## For each draw, update parameter vector (p), run model, calculate error and store it.
        # Haven't put into a function as probably too many arguements.
        v = 0
        selectedRuns <- c()
        error <- c()
        for (k in 1:dim(lhs)[1]) {

            p[["Rho"]]     <- lhs[,"rho"][k]
            p[["Epsilon"]] <- lhs[,"epsilon"][k]
            p[["Kappa"]]   <- lhs[,"kappa"][k]
            p[["Gamma"]]   <- lhs[,"gamma"][k]
            p[["Theta"]]   <- lhs[,"theta"][k]
            p[["Omega"]]   <- lhs[,"omega"][k]
            p[["Mu"]]      <- lhs[,"mu"][k]
            p[["p"]]       <- lhs[,"p"][k]
            p[["q"]]       <- lhs[,"q"][k]

            y <- GetCalibInitial(p, data, init2010 = lhsInitial[k,])
            out <- SSE(AssembleComparisonDataFrame(country = "Kenya", model = CallCalibModel(time, y, p, i), data = data))
            error[k] <- sum(out[out$source == "error", "value"])

            # If error <= maxError then store value of k
            if (error[k] <= maxError) {
                v <- v + 1
                selectedRuns[v] <- k
                setProgress(value = v / 100, detail = paste0(v, "%"))
                if (v == limit) break;
            }
            # setProgress(value = k/dim(lhs)[1], detail = paste0((k/dim(lhs)[1])*100,"%"))
        }

        # Order sum of total error from lowest to highest and pick the lowest 10%
        setProgress(value = 0 / 1, detail = "Sampling best runs.")
        # bestTenPercent <- order(error)[1:(maxIterations * 0.1)]

        ## For the best 10%, update the parameter vector (p), re-run simulations and store results
        # Faster than storing ALL results in the first place (I think)
        CalibOut <<- c()
        for (l in 1:limit) {

            p[["Rho"]]     <- lhs[,"rho"][selectedRuns[l]]
            p[["Epsilon"]] <- lhs[,"epsilon"][selectedRuns[l]]
            p[["Kappa"]]   <- lhs[,"kappa"][selectedRuns[l]]
            p[["Gamma"]]   <- lhs[,"gamma"][selectedRuns[l]]
            p[["Theta"]]   <- lhs[,"theta"][selectedRuns[l]]
            p[["Omega"]]   <- lhs[,"omega"][selectedRuns[l]]
            p[["Mu"]]      <- lhs[,"mu"][selectedRuns[l]]
            p[["p"]]       <- lhs[,"p"][selectedRuns[l]]
            p[["q"]]       <- lhs[,"q"][selectedRuns[l]]

            y <- GetCalibInitial(p, data, init2010 = lhsInitial[selectedRuns[l],])
            iOut <- SSE(AssembleComparisonDataFrame(country = "Kenya", model = CallCalibModel(time, y, p, i), data = data))
            CalibOut <<- rbind(CalibOut, iOut)
            setProgress(value = l / limit, detail = paste0("Resample ", l, "%"))
        }

        # Create data.frame to hold all parameter values used by top 10%
        CalibParamOut <<- FillParValues(samples = lhs, positions = selectedRuns, limit = limit)

        # Will need a CalibInitOut
        CalibInitOut <<- FillInitValues(samples = lhsInitial, positions = selectedRuns, limit = limit)

        # Calculate min and max values used by parameter set
        ParamMaxMin <<- data.frame(
            min = apply(CalibParamOut, 2, min),
            max = apply(CalibParamOut, 2, max)
        )

        # Copy over to reactiveValues
        CalibParamMaxMin$rho_MAX     <- ParamMaxMin["rho"   , "max"]
        CalibParamMaxMin$rho_MIN     <- ParamMaxMin["rho"   , "min"]
        CalibParamMaxMin$q_MAX       <- ParamMaxMin["q"     , "max"]
        CalibParamMaxMin$q_MIN       <- ParamMaxMin["q"     , "min"]
        CalibParamMaxMin$gamma_MAX   <- ParamMaxMin["gamma" , "max"]
        CalibParamMaxMin$gamma_MIN   <- ParamMaxMin["gamma" , "min"]
        CalibParamMaxMin$theta_MAX   <- ParamMaxMin["theta" , "max"]
        CalibParamMaxMin$theta_MIN   <- ParamMaxMin["theta" , "min"]
        CalibParamMaxMin$kappa_MAX   <- ParamMaxMin["kappa" , "max"]
        CalibParamMaxMin$kappa_MIN   <- ParamMaxMin["kappa" , "min"]
        CalibParamMaxMin$omega_MAX   <- ParamMaxMin["omega" , "max"]
        CalibParamMaxMin$omega_MIN   <- ParamMaxMin["omega" , "min"]
        CalibParamMaxMin$mu_MAX      <- ParamMaxMin["mu"    , "max"]
        CalibParamMaxMin$mu_MIN      <- ParamMaxMin["mu"    , "min"]
        CalibParamMaxMin$p_MAX       <- ParamMaxMin["p"     , "max"]
        CalibParamMaxMin$p_MIN       <- ParamMaxMin["p"     , "min"]


        # Plots
        # Then comment this out and call it elsewhere.
        setProgress(value = 1, detail = "Building figures")
        BuildCalibrationPlots(data = CalibOut, originalData = data)
    })

    # Return min and max values used for all parameters.
    return(ParamMaxMin)
}
