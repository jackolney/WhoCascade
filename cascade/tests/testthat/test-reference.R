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
    Time <- seq(0, 5, 1)
    plist <- list(p, i)
    names(plist) <- c("r_par", "r_inc")
    theref <- deSolve::ode(times = Time, y = y, func = ComplexCascadeCalib, parms = p, inc = i)
    result <- deSolve::ode(times = Time, y = y, func = "calib_derivs", parms = plist, initfunc = "calib_initmod", dllname = "cascade")
    expect_equal(theref, result, check.attributes = FALSE, tolerance = 1e-16, info = "Non-equal ode() return error.")
})

test_that("Incidence Test", {
    source("TheModel-Calib.R", local = TRUE)
    p <- parameters(
        Alpha_1 = 0,
        Alpha_2 = 0,
        Alpha_3 = 0,
        Alpha_4 = 0,
        Alpha_5 = 0,
        Alpha_6 = 0,
        Alpha_7 = 0,
        Tau_1 = 0,
        Tau_2 = 0,
        Tau_3 = 0,
        Tau_4 = 0,
        Tau_5 = 0,
        Tau_6 = 0,
        Tau_7 = 0,
        Mu = 0
        )
    y <- initial(p)
    i <- incidence()
    time <- seq(0, 5, 1)
    plist <- list(p, i)
    names(plist) <- c("r_par", "r_inc")
    theref <- as.data.frame(deSolve::ode(times = time, y = y, func = ComplexCascadeCalib, parms = p, inc = i))
    result <- as.data.frame(deSolve::ode(times = time, y = y, func = "calib_derivs", parms = plist, initfunc = "calib_initmod", dllname = "cascade"))
    expect_equal(theref[["NewInf"]], result[["NewInf"]], check.attributes = FALSE, tolerance = 1e-16, info = "Incidence error.")
})

test_that("Constant Population Test", {
    source("TheModel-Calib.R", local = TRUE)
    p <- parameters(
        Alpha_1 = 0,
        Alpha_2 = 0,
        Alpha_3 = 0,
        Alpha_4 = 0,
        Alpha_5 = 0,
        Alpha_6 = 0,
        Alpha_7 = 0,
        Tau_1 = 0,
        Tau_2 = 0,
        Tau_3 = 0,
        Tau_4 = 0,
        Tau_5 = 0,
        Tau_6 = 0,
        Tau_7 = 0,
        Mu = 0
        )
    y <- initial(p)
    i <- incidence(rep(0,7))
    time <- seq(0, 5, 1)
    plist <- list(p, i)
    names(plist) <- c("r_par", "r_inc")
    theref <- as.data.frame(deSolve::ode(times = time, y = y, func = ComplexCascadeCalib, parms = p, inc = i))
    result <- as.data.frame(deSolve::ode(times = time, y = y, func = "calib_derivs", parms = plist, initfunc = "calib_initmod", dllname = "cascade"))
    ref_size <- theref[["UnDx_500"]] + theref[["UnDx_350500"]] + theref[["UnDx_250350"]] + theref[["UnDx_200250"]] + theref[["UnDx_100200"]] + theref[["UnDx_50100"]] + theref[["UnDx_50"]] +
    theref[["Dx_500"]] + theref[["Dx_350500"]] + theref[["Dx_250350"]] + theref[["Dx_200250"]] + theref[["Dx_100200"]] + theref[["Dx_50100"]] + theref[["Dx_50"]] +
    theref[["Care_500"]] + theref[["Care_350500"]] + theref[["Care_250350"]] + theref[["Care_200250"]] + theref[["Care_100200"]] + theref[["Care_50100"]] + theref[["Care_50"]] +
    theref[["PreLtfu_500"]] + theref[["PreLtfu_350500"]] + theref[["PreLtfu_250350"]] + theref[["PreLtfu_200250"]] + theref[["PreLtfu_100200"]] + theref[["PreLtfu_50100"]] + theref[["PreLtfu_50"]] +
    theref[["Tx_Na_500"]] + theref[["Tx_Na_350500"]] + theref[["Tx_Na_250350"]] + theref[["Tx_Na_200250"]] + theref[["Tx_Na_100200"]] + theref[["Tx_Na_50100"]] + theref[["Tx_Na_50"]] +
    theref[["Tx_A_500"]] + theref[["Tx_A_350500"]] + theref[["Tx_A_250350"]] + theref[["Tx_A_200250"]] + theref[["Tx_A_100200"]] + theref[["Tx_A_50100"]] + theref[["Tx_A_50"]] +
    theref[["Ltfu_500"]] + theref[["Ltfu_350500"]] + theref[["Ltfu_250350"]] + theref[["Ltfu_200250"]] + theref[["Ltfu_100200"]] + theref[["Ltfu_50100"]] + theref[["Ltfu_50"]]
    result_size <- result[["UnDx_500"]] + result[["UnDx_350500"]] + result[["UnDx_250350"]] + result[["UnDx_200250"]] + result[["UnDx_100200"]] + result[["UnDx_50100"]] + result[["UnDx_50"]] +
    result[["Dx_500"]] + result[["Dx_350500"]] + result[["Dx_250350"]] + result[["Dx_200250"]] + result[["Dx_100200"]] + result[["Dx_50100"]] + result[["Dx_50"]] +
    result[["Care_500"]] + result[["Care_350500"]] + result[["Care_250350"]] + result[["Care_200250"]] + result[["Care_100200"]] + result[["Care_50100"]] + result[["Care_50"]] +
    result[["PreLtfu_500"]] + result[["PreLtfu_350500"]] + result[["PreLtfu_250350"]] + result[["PreLtfu_200250"]] + result[["PreLtfu_100200"]] + result[["PreLtfu_50100"]] + result[["PreLtfu_50"]] +
    result[["Tx_Na_500"]] + result[["Tx_Na_350500"]] + result[["Tx_Na_250350"]] + result[["Tx_Na_200250"]] + result[["Tx_Na_100200"]] + result[["Tx_Na_50100"]] + result[["Tx_Na_50"]] +
    result[["Tx_A_500"]] + result[["Tx_A_350500"]] + result[["Tx_A_250350"]] + result[["Tx_A_200250"]] + result[["Tx_A_100200"]] + result[["Tx_A_50100"]] + result[["Tx_A_50"]] +
    result[["Ltfu_500"]] + result[["Ltfu_350500"]] + result[["Ltfu_250350"]] + result[["Ltfu_200250"]] + result[["Ltfu_100200"]] + result[["Ltfu_50100"]] + result[["Ltfu_50"]]
    expect_equal(ref_size, result_size, check.attributes = FALSE, tolerance = 1e-16, info = "Population not constant with zero mortality and zero incidence.")
})
