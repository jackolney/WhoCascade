# test script for R package 'cascade'
# WHO Cascade Model
rm(list=ls())

setwd("~/git/WhoCascade/cascade")

# Testing
devtools::load_all()
devtools::document()
devtools::test()

p <- parameters()
i <- initial(p)




library(cascade, lib.loc = "~/git/WhoCascade/cascade")

cascade::parameters()

p <- parameters()
y <- initial(p)
result <- cascade_derivs(y, p)

ref <- ComplexCascade(0, y, p)[[1]]
expect_equal(result, ref)

Time <- seq(0, 5, 0.02)
theref <- deSolve::ode(times = Time, y = y, func = ComplexCascade, parms = p)

result <- deSolve::ode(times = Time, y = y, func = "derivs", parms = p, initfunc = "initmod", dllname = "cascade")

head(result)

plot(result$time, result$N)

a <- as.data.frame(result)

plot(x = a$time, y = a$UnDx_500)

test <- parameters(beta = 2)


dyn.load("src/model.so")

p <- parameters()

.Call("r_initmod", p, PACKAGE = "model")