# non-shiny optimisation triggers and setup
# input stuff etc.

source("server/model/beta.R",                  local = FALSE)
source("server/model/initial.R",               local = FALSE)
source("server/model/mean-model.R",            local = FALSE)
source("server/model/model.R",                 local = FALSE)
source("server/model/parameters.R",            local = FALSE)
source("server/optimisation/functions.R",      local = FALSE)
source("server/optimisation/sim.R",            local = FALSE)
source("server/projection/CD4-distribution.R", local = FALSE)

# reactive input setup
input <- c()
input$TestingCheck      <- FALSE
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

###########
## TESTS ##
###########

## TEST ONE ##

# Will need to be packaged properly.

# If we just test the first output of the below, it should give me the baseline parameter values
theTest <- GetParaMatrix(calibParamOut = CalibParamOut)[1,]

theComp <- data.frame(
    Rho =   lapply(calibParamOut, function(x) {return(mean(x))})[["rho"]],
    Q =     lapply(calibParamOut, function(x) {return(mean(x))})[["q"]],
    Kappa = lapply(calibParamOut, function(x) {return(mean(x))})[["kappa"]],
    Gamma = lapply(calibParamOut, function(x) {return(mean(x))})[["gamma"]],
    Sigma = 0,
    Omega = lapply(calibParamOut, function(x) {return(mean(x))})[["omega"]]
    )

a = round(theTest, digits = 3)
b = round(theComp, digits = 3)

testthat::expect_true(sum(a == b) == 6)

# all.equal doesn't work
# all.equal(round(theTest, 3), round(theComp, 3))


## TEST TWO ##

# A comparison between GetOptpar() and GetMeanPar()
# With all interventionChecks = FALSE
input$TestingCheck      <- FALSE
input$LinkageCheck      <- FALSE
input$PreRetentionCheck <- FALSE
input$InitiationCheck   <- FALSE
input$AdherenceCheck    <- FALSE
input$RetentionCheck    <- FALSE

theOpt <- GetOptPar(
    masterCD4 = MasterCD4_2015,
    data = MasterData,
    iterationParam = GetParaMatrix(calibParamOut = CalibParamOut)[1,],
    calibParamOut = CalibParamOut)

theMean <- GetMeanPar(
    masterCD4 = MasterCD4_2015,
    data = MasterData,
    calibParamOut = CalibParamOut)

# We should get equal
testthat::expect_equal(theOpt, theMean)


theOpt[7]
theMean[7]
theMean["Sigma"]

lapply(calibParamOut, function(x) {return(mean(x))})[["rho"]]
lapply(calibParamOut, function(x) {return(mean(x))})[["q"]]
lapply(calibParamOut, function(x) {return(mean(x))})[["kappa"]]
lapply(calibParamOut, function(x) {return(mean(x))})[["gamma"]]
lapply(calibParamOut, function(x) {return(mean(x))})[["sigma"]]
lapply(calibParamOut, function(x) {return(mean(x))})[["omega"]]

lapply(calibParamOut, function(x) {return(mean(x))})[["rho"]]
lapply(calibParamOut, function(x) {return(mean(x))})[["q"]]
lapply(calibParamOut, function(x) {return(mean(x))})[["kappa"]]
lapply(calibParamOut, function(x) {return(mean(x))})[["gamma"]]
lapply(calibParamOut, function(x) {return(mean(x))})[["sigma"]]
lapply(calibParamOut, function(x) {return(mean(x))})[["omega"]]
lapply(calibParamOut, function(x) {return(mean(x))})[["theta"]]
lapply(calibParamOut, function(x) {return(mean(x))})[["mu"]]
lapply(calibParamOut, function(x) {return(mean(x))})[["p"]]

CalibParamOut
ParamMaxMin


round(lapply(CalibParamOut, function(x) {return(mean(x))})[["rho"]], digits = 3)
round(lapply(CalibParamOut, function(x) {return(mean(x))})[["rho"]], digits = 3) * 5


dim(theList[[1]])

names(theList[[1]])

some <- CallMeanModel()

same <- theList[[1]]

dim(some)
dim(same)

testthat::expect_equal(some, same)

some$N == same$N

# Rounding issues - URGH.

