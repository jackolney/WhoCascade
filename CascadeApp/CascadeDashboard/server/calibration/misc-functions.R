DefineParmRange <- function(param, min, max) {
    parRange <- data.frame(
        min = c(
            rho     = param[["Rho"]]     * min,
            epsilon = param[["Epsilon"]] * min,
            kappa   = param[["Kappa"]]   * min,
            gamma   = param[["Gamma"]]   * min,
            theta   = param[["Theta"]]   * min,
            omega   = param[["Omega"]]   * min,
            mu      = param[["Mu"]]      * 1,
            p       = if(param[["p"]] * min > 1) {1} else {param[["p"]] * min},
            q       = if(param[["q"]] * min > 1) {1} else {param[["q"]] * min}
        ),
        max = c(
            rho     = param[["Rho"]]     * max,
            epsilon = param[["Epsilon"]] * max,
            kappa   = param[["Kappa"]]   * max,
            gamma   = param[["Gamma"]]   * max,
            theta   = param[["Theta"]]   * max,
            omega   = param[["Omega"]]   * max,
            mu      = param[["Mu"]]      * 0,
            p       = if(param[["p"]] * max > 1) {1} else {param[["p"]] * max},
            q       = if(param[["q"]] * max > 1) {1} else {param[["q"]] * max}
        )
    )
    parRange
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

FillParValues <- function(samples, positions, iterations) {
        out <- data.frame(rho = 0, epsilon = 0, kappa = 0, gamma = 0, theta = 0, omega = 0, mu = 0, p = 0, q = 0)

        # Loop through all iterations and fill out data.frame
        for(l in 1:(iterations * 0.1)) {
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
