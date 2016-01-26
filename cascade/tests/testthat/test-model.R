context("Model")

test_that("Input validation", {
    p <- Parameters()
    y <- Initial(p)
    expect_error(cascade_derivs(y, p[-1]), "Invalid Parameters.")
})