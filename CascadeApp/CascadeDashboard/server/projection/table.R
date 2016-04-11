output$optParTable_Rho <- renderTable({
    theData <- seq(from = input$userOptRho_Range[1], to = input$userOptRho_Range[2], length.out = input$userOptRho_LengthOf)
    tbl <- matrix(theData, nrow = 1, ncol = input$userOptRho_LengthOf)
    rownames(tbl) <- "Values:"
    colnames(tbl) <- paste("p", seq(1, input$userOptRho_LengthOf, 1), sep = '')
    return(tbl)
}, digits = 3)

output$optParTable_Q <- renderTable({
    theData <- seq(from = input$userOptq_Range[1], to = input$userOptq_Range[2], length.out = input$userOptq_LengthOf)
    tbl <- matrix(theData, nrow = 1, ncol = input$userOptq_LengthOf)
    rownames(tbl) <- "Values:"
    colnames(tbl) <- paste("p", seq(1, input$userOptq_LengthOf, 1), sep = '')
    return(tbl)
}, digits = 3)

output$optParTable_Kappa <- renderTable({
    theData <- seq(from = input$userOptKappa_Range[2], to = input$userOptKappa_Range[1], length.out = input$userOptKappa_LengthOf)
    tbl <- matrix(theData, nrow = 1, ncol = input$userOptKappa_LengthOf)
    rownames(tbl) <- "Values:"
    colnames(tbl) <- paste("p", seq(1, input$userOptKappa_LengthOf, 1), sep = '')
    return(tbl)
}, digits = 3)

output$optParTable_Gamma <- renderTable({
    theData <- seq(from = input$userOptGamma_Range[1], to = input$userOptGamma_Range[2], length.out = input$userOptGamma_LengthOf)
    tbl <- matrix(theData, nrow = 1, ncol = input$userOptGamma_LengthOf)
    rownames(tbl) <- "Values:"
    colnames(tbl) <- paste("p", seq(1, input$userOptGamma_LengthOf, 1), sep = '')
    return(tbl)
}, digits = 3)

output$optParTable_Sigma <- renderTable({
    theData <- seq(from = input$userOptSigma_Range[1], to = input$userOptSigma_Range[2], length.out = input$userOptSigma_LengthOf)
    tbl <- matrix(theData, nrow = 1, ncol = input$userOptSigma_LengthOf)
    rownames(tbl) <- "Values:"
    colnames(tbl) <- paste("p", seq(1, input$userOptSigma_LengthOf, 1), sep = '')
    return(tbl)
}, digits = 3)

output$optParTable_Omega <- renderTable({
    theData <- seq(from = input$userOptOmega_Range[2], to = input$userOptOmega_Range[1], length.out = input$userOptOmega_LengthOf)
    tbl <- matrix(theData, nrow = 1, ncol = input$userOptOmega_LengthOf)
    rownames(tbl) <- "Values:"
    colnames(tbl) <- paste("p", seq(1, input$userOptOmega_LengthOf, 1), sep = '')
    return(tbl)
}, digits = 3)

output$optIterationTable <- renderTable({
    print(paste("userOptRho_LengthOf =",input$userOptRho_LengthOf))
    print(paste("userOptq_LengthOf =",input$userOptq_LengthOf))
    print(paste("userOptKappa_LengthOf =",input$userOptKappa_LengthOf))
    print(paste("userOptGamma_LengthOf =",input$userOptGamma_LengthOf))
    print(paste("userOptSigma_LengthOf =",input$userOptSigma_LengthOf))
    print(paste("userOptOmega_LengthO =",input$userOptOmega_LengthO))
    ParInput <- expand.grid(
        Rho   = seq(from = input$userOptRho_Range[1],   to = input$userOptRho_Range[2],   length.out = input$userOptRho_LengthOf),
        Q     = seq(from = input$userOptq_Range[1],     to = input$userOptq_Range[2],     length.out = input$userOptq_LengthOf),
        Kappa = seq(from = input$userOptKappa_Range[1], to = input$userOptKappa_Range[2], length.out = input$userOptKappa_LengthOf),
        Gamma = seq(from = input$userOptGamma_Range[1], to = input$userOptGamma_Range[2], length.out = input$userOptGamma_LengthOf),
        Sigma = seq(from = input$userOptSigma_Range[1], to = input$userOptSigma_Range[2], length.out = input$userOptSigma_LengthOf),
        Omega = seq(from = input$userOptOmega_Range[1], to = input$userOptOmega_Range[2], length.out = input$userOptOmega_LengthOf)
    )
    tbl <- matrix(0, nrow = 2, ncol = 1)
    tbl[1,] <- dim(ParInput)[1]
    tbl[2,] <- (dim(ParInput)[1] * 0.9745) / 60
    colnames(tbl) <- "Value"
    rownames(tbl) <- c("Number of iterations:","Estimated completion time (min):")
    return(tbl)
})
