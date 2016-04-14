# non-shiny optimisation triggers and setup
# input stuff etc.

source("server/model/parameters.R",       local = FALSE)
source("server/model/initial.R",          local = FALSE)
source("server/model/beta.R",             local = FALSE)
source("server/model/CD4-distribution.R", local = FALSE)

# reactive input setup
input <- c()
input$TestingCheck      <- TRUE
input$LinkageCheck      <- FALSE
input$PreRetentionCheck <- FALSE
input$InitiationCheck   <- FALSE
input$AdherenceCheck    <- FALSE
input$RetentionCheck    <- FALSE
input$optimParamLength  <- 4


input$userOptRho_Range <- c(
    round(lapply(CalibParamOut, function(x) {return(mean(x))})[["rho"]], digits = 3),
    round(lapply(CalibParamOut, function(x) {return(mean(x))})[["rho"]], digits = 3) * 5
)

input$userOptq_Range <- c(
    round(lapply(CalibParamOut, function(x) {return(mean(x))})[["q"]], digits = 3),
    1
)

input$userOptKappa_Range <- c(
    round(lapply(CalibParamOut, function(x) {return(mean(x))})[["kappa"]], digits = 3),
    round(lapply(CalibParamOut, function(x) {return(mean(x))})[["kappa"]], digits = 3) * 5
)

input$userOptGamma_Range <- c(
    round(lapply(CalibParamOut, function(x) {return(mean(x))})[["gamma"]], digits = 3),
    round(lapply(CalibParamOut, function(x) {return(mean(x))})[["gamma"]], digits = 3) * 5
)

input$userOptSigma_Range <- c(0, 5)

input$userOptOmega_Range <- c(
    round(lapply(CalibParamOut, function(x) {return(mean(x))})[["omega"]], digits = 3),
    round(lapply(CalibParamOut, function(x) {return(mean(x))})[["omega"]], digits = 3) * 5
)


# Testing somem functions

test = GetParaMatrix(calibParamOut = CalibParamOut)

test
