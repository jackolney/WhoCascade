context("Model")

test_that("Input validation", {
    p <- parameters()
    y <- initial(p)
    expect_error(cascade_derivs(y, p[-1]), info = "Invalid Parameters.")
})

test_that("Alter parameters", {
    testValue = 2
    expect_true(parameters(beta = 2)[["beta"]] == testValue, info = "Parameter Error.")
})

test_that("Incidence validation", {
    p <- parameters()
    y <- initial(p)
    expect_error(cascade_calib_derivs(y, p), info = "Incidence Error.")
})

test_that("Passing a list", {
    p <- parameters()
    y <- initial(p)
    i <- incidence()
    plist <- list(p, i)
    names(plist) <- c("r_par", "r_inc")
    expect_true(is.list(plist), info = "Not a list.")
})

test_that("Testing NULL", {
    p <- parameters()
    y <- initial(p)
    i <- incidence()
    plist <- list(p, i)
    names(plist) <- c("r_par", "r_inc")
    expect_null(.Call("r_calib_initmod", plist, PACKAGE = "cascade"), info = "Initmod Error.")
})

test_that("Testing deSolve call", {
    p <- parameters()
    y <- initial(p)
    i <- incidence()
    plist <- list(p, i)
    names(plist) <- c("r_par", "r_inc")
    time <- seq(0, 5, 1)
    expect_output(deSolve::ode(times = time, y = y, func = "calib_derivs", initfunc = "calib_initmod", dllname = "cascade", parms = plist), regexp = NA, info = "deSolve Error.")
})
