context("Reference")

test_that("Projection Model", {
    source("TheModel.R", local = TRUE)
    p <- parameters()
    y <- initial(p)
    result <- cascade_derivs(y, p)
    ref <- ComplexCascade(0, y, p)[[1]]
    expect_equal(result, ref, info = "Non-equal model function error.")
    Time <- seq(0, 5, 0.02)
    theref <- deSolve::ode(times = Time, y = y, func = ComplexCascade, parms = p)
    result <- deSolve::ode(times = Time, y = y, func = "derivs", parms = p, initfunc = "initmod", dllname = "cascade")
    expect_equal(theref, result, check.attributes = FALSE, tolerance = 1e-16, info = "Non-equal ode() return error.")
})

test_that("Calibration Model", {
    source("TheModel-Calib.R", local = TRUE)
    p <- parameters()
    y <- initial(p)
    i <- incidence()
    result <- cascade_calib_derivs(y, p, i)
    ref <- ComplexCascadeCalib(0, y, p, i)[[1]]
    expect_equal(result, ref, info = "Non-equal model function error.")
    Time <- seq(0, 5, 0.02)
    plist <- list(p, i)
    names(plist) <- c("r_par", "r_inc")
    theref <- deSolve::ode(times = Time, y = y, func = ComplexCascadeCalib, parms = p, inc = i)
    result <- deSolve::ode(times = Time, y = y, func = "calib_derivs", parms = plist, initfunc = "calib_initmod", dllname = "cascade")
    expect_equal(theref, result, check.attributes = FALSE, tolerance = 1e-16, info = "Non-equal ode() return error.")
})
