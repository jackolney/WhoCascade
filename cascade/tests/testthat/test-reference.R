context("Reference")

test_that("simple", {
    source("TheModel.R", local = TRUE)
    p <- Parameters()
    y <- Initial(p)
    result <- cascade_derivs(y, p)
    ref <- ComplexCascade(0, y, p)[[1]]
    expect_equal(result, ref)
    Time <- seq(0, 5, 0.02)
    theref <- deSolve::ode(times = Time, y = y, func = ComplexCascade, parms = p)
    result <- deSolve::ode(times = Time, y = y, func = "derivs", initfunc = "initmod", dllname = "cascade", parms = p)
    expect_equal(theref, result, check.attributes = FALSE, tolerance = 1e-16)
})