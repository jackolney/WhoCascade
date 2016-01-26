context("Model")

test_that("Input validation", {
    p <- parameters()
    y <- initial(p)
    expect_error(cascade_derivs(y, p[-1]), "Invalid Parameters.")
})

test_that("Alter parameters", {
    testValue = 2
    expect_true(parameters(beta = 2)[["beta"]] == testValue, info = "Parameter error.")
})