GetOptPar <- function(masterCD4, data, iterationParam, calibParamOut) {
    p <- parameters(
        prop_preART_500    = masterCD4[1,"prop.Off.ART.500"][[1]],
        prop_preART_350500 = masterCD4[1,"prop.Off.ART.350500"][[1]],
        prop_preART_250350 = masterCD4[1,"prop.Off.ART.250350"][[1]],
        prop_preART_200250 = masterCD4[1,"prop.Off.ART.200250"][[1]],
        prop_preART_100200 = masterCD4[1,"prop.Off.ART.100200"][[1]],
        prop_preART_50100  = masterCD4[1,"prop.Off.ART.50100"][[1]],
        prop_preART_50     = masterCD4[1,"prop.Off.ART.50"][[1]],
        t_1 = ConvertYear2015(data[["treatment_guidelines"]][["more500"]]),
        t_2 = ConvertYear2015(data[["treatment_guidelines"]][["less500"]]),
        t_3 = ConvertYear2015(data[["treatment_guidelines"]][["less350"]]),
        t_4 = ConvertYear2015(data[["treatment_guidelines"]][["less250"]]),
        t_5 = ConvertYear2015(data[["treatment_guidelines"]][["less200"]]),

        # These guys still need to be set by the model.
        Theta = lapply(CalibParamOut, function(x) {return(mean(x))})[["theta"]],
        Mu    = lapply(CalibParamOut, function(x) {return(mean(x))})[["mu"]],
        p     = lapply(CalibParamOut, function(x) {return(mean(x))})[["p"]],

        # MODIFYING #
        Rho   = iterationParam[["rho"]],
        Kappa = iterationParam[["kappa"]],
        Gamma = iterationParam[["gamma"]],
        Sigma = iterationParam[["sigma"]],
        Omega = iterationParam[["omega"]],
        q     = iterationParam[["q"]]
    )
    p
}

GetMeanPar <- function(masterCD4, data, calibParamOut) {
    p <- parameters(
        prop_preART_500    = masterCD4[1,"prop.Off.ART.500"][[1]],
        prop_preART_350500 = masterCD4[1,"prop.Off.ART.350500"][[1]],
        prop_preART_250350 = masterCD4[1,"prop.Off.ART.250350"][[1]],
        prop_preART_200250 = masterCD4[1,"prop.Off.ART.200250"][[1]],
        prop_preART_100200 = masterCD4[1,"prop.Off.ART.100200"][[1]],
        prop_preART_50100  = masterCD4[1,"prop.Off.ART.50100"][[1]],
        prop_preART_50     = masterCD4[1,"prop.Off.ART.50"][[1]],
        t_1 = ConvertYear2015(data[["treatment_guidelines"]][["more500"]]),
        t_2 = ConvertYear2015(data[["treatment_guidelines"]][["less500"]]),
        t_3 = ConvertYear2015(data[["treatment_guidelines"]][["less350"]]),
        t_4 = ConvertYear2015(data[["treatment_guidelines"]][["less250"]]),
        t_5 = ConvertYear2015(data[["treatment_guidelines"]][["less200"]]),

        # Just using mean parameter values for the momen #
        Theta = lapply(CalibParamOut, function(x) {return(mean(x))})[["theta"]],
        Mu    = lapply(CalibParamOut, function(x) {return(mean(x))})[["mu"]],
        p     = lapply(CalibParamOut, function(x) {return(mean(x))})[["p"]],
        Rho   = lapply(CalibParamOut, function(x) {return(mean(x))})[["rho"]],
        Kappa = lapply(CalibParamOut, function(x) {return(mean(x))})[["kappa"]],
        Gamma = lapply(CalibParamOut, function(x) {return(mean(x))})[["gamma"]],
        Sigma = lapply(CalibParamOut, function(x) {return(mean(x))})[["sigma"]],
        Omega = lapply(CalibParamOut, function(x) {return(mean(x))})[["omega"]],
        q     = lapply(CalibParamOut, function(x) {return(mean(x))})[["q"]]
    )
    p
}
