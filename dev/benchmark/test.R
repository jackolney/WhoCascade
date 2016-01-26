library(testthat)
library(microbenchmark)

dyn.load("model.so")
# setwd("")
source("TheModel.R")
source("Parameters.R")
source("Initial.R")

p <- Parameters()
y <- Initial(p)

.Call("r_initmod", p, PACKAGE = "model")


result <- .Call("r_derivs", y, PACKAGE = "model")

ref <- ComplexCascade(0, y, p)[[1]]

expect_equal(result, ref)

microbenchmark(
c = .Call("r_derivs", y, PACKAGE = "model"),
r = ComplexCascade(0, y, p)[[1]]
    )

require(deSolve)
Time <- seq(0, 5, 0.02)
theref <- ode(times = Time, y = y, func = ComplexCascade, parms = p)

result <- ode(times = Time, y = y, func = "derivs", initfunc = "initmod", dllname = "model", parms = p)

all.equal(theref, result, check.attributes = FALSE, tolerance = 1e-16)

microbenchmark(
r = ode(times = Time, y = y, func = ComplexCascade, parms = p),
c = ode(times = Time, y = y, func = "derivs", initfunc = "initmod", dllname = "model", parms = p)
    )

Rprof(interval=0.002)
for (i in seq_len(1000))
xx <- ode(times = Time, y = y, func = "derivs", initfunc = "initmod", dllname = "model", parms = p)
Rprof(NULL)
