# Run.R -- 25/01/16 -- Jack Olney
require(deSolve)

setwd("~/git/WhoCascade/dev/benchmark")

source("TheModel.R")
source("Initial.R")
source("Parameters.R")


# The Model #
Run.Model <- function(x = 1) {
    for(i in 1:x) {
        Time <- seq(0, 5, 0.02)
        Beta <- 0.0275837
        theOut <- ode(times = Time, y = Initial, func = ComplexCascade, parms = Parameters, beta = Beta)
    }
}


Run.Model()

system.time(Run.Model())

# Benchmarking
require(lineprof)
t <- lineprof(Run.Model())
t

# shine(t)