DefineParmRange <- function(param, min, max) {
    parRange <- data.frame(
        min = c(
            rho     = param[["Rho"]]     * min,
            epsilon = param[["Epsilon"]] * min,
            kappa   = param[["Kappa"]]   * min,
            gamma   = param[["Gamma"]]   * min,
            theta   = param[["Theta"]]   * min,
            omega   = param[["Omega"]]   * min,
            mu      = param[["Mu"]]      * 10,
            p       = if (param[["p"]] * min > 1) {1} else {param[["p"]] * min},
            q       = if (param[["q"]] * min > 1) {1} else {param[["q"]] * min}
        ),
        max = c(
            rho     = param[["Rho"]]     * max,
            epsilon = param[["Epsilon"]] * max,
            kappa   = param[["Kappa"]]   * max,
            gamma   = param[["Gamma"]]   * max,
            theta   = param[["Theta"]]   * max,
            omega   = param[["Omega"]]   * max,
            mu      = param[["Mu"]]      * 0,
            p       = if (param[["p"]] * max > 1) {1} else {param[["p"]] * max},
            q       = if (param[["q"]] * max > 1) {1} else {param[["q"]] * max}
        )
    )
    parRange
}

DefineInitRange <- function(data, min, max) {
    i2010 <- data[["calib"]][data[["calib"]]$year == 2010,]

    initRange <- data.frame(
        min = c(
            plhiv =      i2010[i2010$indicator == "PLHIV",           "value"] * min,
            plhiv_diag = i2010[i2010$indicator == "PLHIV Diagnosed", "value"] * min,
            plhiv_care = i2010[i2010$indicator == "PLHIV in Care",   "value"] * min,
            plhiv_art =  i2010[i2010$indicator == "PLHIV on ART",    "value"] * min
            ),
        max = c(
            plhiv =      i2010[i2010$indicator == "PLHIV",           "value"] * max,
            plhiv_diag = i2010[i2010$indicator == "PLHIV Diagnosed", "value"] * max,
            plhiv_care = i2010[i2010$indicator == "PLHIV in Care",   "value"] * max,
            plhiv_art =  i2010[i2010$indicator == "PLHIV on ART",    "value"] * max
            )
    )
    initRange
}

AppendMinMaxMean <- function(data) {
    uniqueIndicators <- unique(data$indicator)
    uniqueYear <- unique(data$year)

    for(m in 1:length(uniqueIndicators)) {
        for(l in 1:length(uniqueYear)) {
            data[data$year == uniqueYear[l] & data$indicator == uniqueIndicators[m],"min"]  <-  min(data[data$year == uniqueYear[l] & data$indicator == uniqueIndicators[m],"value"])
            data[data$year == uniqueYear[l] & data$indicator == uniqueIndicators[m],"max"]  <-  max(data[data$year == uniqueYear[l] & data$indicator == uniqueIndicators[m],"value"])
            data[data$year == uniqueYear[l] & data$indicator == uniqueIndicators[m],"mean"] <- mean(data[data$year == uniqueYear[l] & data$indicator == uniqueIndicators[m],"value"])
            }
    }
    data
}

ggColorHue <- function(n) {
    hues = seq(15, 375, length = n+1)
    hcl(h = hues, l = 65, c = 100)[1:n]
}

FillParValues <- function(samples, positions, limit) {
        out <- data.frame(rho = 0, epsilon = 0, kappa = 0, gamma = 0, theta = 0, omega = 0, mu = 0, p = 0, q = 0)

        # Loop through all iterations and fill out data.frame
        for(l in 1:limit) {
            out[l,"rho"]     = samples[,"rho"][positions[l]]
            out[l,"epsilon"] = samples[,"epsilon"][positions[l]]
            out[l,"kappa"]   = samples[,"kappa"][positions[l]]
            out[l,"gamma"]   = samples[,"gamma"][positions[l]]
            out[l,"theta"]   = samples[,"theta"][positions[l]]
            out[l,"omega"]   = samples[,"omega"][positions[l]]
            out[l,"mu"]      = samples[,"mu"][positions[l]]
            out[l,"p"]       = samples[,"p"][positions[l]]
            out[l,"q"]       = samples[,"q"][positions[l]]
        }
        out
    }

FillInitValues <- function(samples, positions, limit) {
        out <- data.frame(plhiv = 0, plhiv_diag = 0, plhiv_care = 0, plhiv_art = 0)

        # Loop through all iterations and fill out data.frame
        for(l in 1:limit) {
            out[l,"plhiv"]      = samples[,"plhiv"][positions[l]]
            out[l,"plhiv_diag"] = samples[,"plhiv_diag"][positions[l]]
            out[l,"plhiv_care"] = samples[,"plhiv_care"][positions[l]]
            out[l,"plhiv_art"]  = samples[,"plhiv_art"][positions[l]]
        }
        out
    }

UserOverRide <- function(param) {
    if (!is.na(userParRange$rho) & userParRange$rho >= 0) {
        param[which(row.names(param) == "rho"),"min"]     <- userParRange$rho
        param[which(row.names(param) == "rho"),"max"]     <- userParRange$rho
    }
    if (!is.na(userParRange$epsilon) & userParRange$epsilon >= 0) {
        param[which(row.names(param) == "epsilon"),"min"] <- userParRange$epsilon
        param[which(row.names(param) == "epsilon"),"max"] <- userParRange$epsilon
    }
    if (!is.na(userParRange$kappa) & userParRange$kappa >= 0) {
        param[which(row.names(param) == "kappa"),"min"]   <- userParRange$kappa
        param[which(row.names(param) == "kappa"),"max"]   <- userParRange$kappa
    }
    if (!is.na(userParRange$gamma) & userParRange$gamma >= 0) {
        param[which(row.names(param) == "gamma"),"min"]   <- userParRange$gamma
        param[which(row.names(param) == "gamma"),"max"]   <- userParRange$gamma
    }
    if (!is.na(userParRange$theta) & userParRange$theta >= 0) {
        param[which(row.names(param) == "theta"),"min"]   <- userParRange$theta
        param[which(row.names(param) == "theta"),"max"]   <- userParRange$theta
    }
    if (!is.na(userParRange$omega) & userParRange$omega >= 0) {
        param[which(row.names(param) == "omega"),"min"]   <- userParRange$omega
        param[which(row.names(param) == "omega"),"max"]   <- userParRange$omega
    }
    if (!is.na(userParRange$mu) & userParRange$mu >= 0) {
        param[which(row.names(param) == "mu"),"min"]      <- userParRange$mu
        param[which(row.names(param) == "mu"),"max"]      <- userParRange$mu
    }
    if (!is.na(userParRange$p) & userParRange$p >= 0) {
        param[which(row.names(param) == "p"),"min"]       <- userParRange$p
        param[which(row.names(param) == "p"),"max"]       <- userParRange$p
    }
    if (!is.na(userParRange$q) & userParRange$q >= 0) {
        param[which(row.names(param) == "q"),"min"]       <- userParRange$q
        param[which(row.names(param) == "q"),"max"]       <- userParRange$q
    }
    param
}
