context("Model")

test_that("Input validation", {
    p <- parameters()
    y <- initial(p)
    expect_error(cascade_derivs(y, p[-1]), "Invalid Parameters.")
})