# Intervention Detail Page
# Prediction of results from the model

# Testing Intervention
output$opt_rho_baseline <- renderUI({
    baseline <- CallBestModel(CalibOut = CalibOut, minErrorRun = minErrorRun)
    answer <- cumsum(baseline$Dx)[251] - baseline$Dx[1]
    tags$code(scales::comma(round(answer, digits = 0)))
})

output$opt_rho_max <- renderUI({
    baseline <- CallBestModel(CalibOut = CalibOut, minErrorRun = minErrorRun)
    base_answer <- cumsum(baseline$Dx)[251] - baseline$Dx[1]

    alt <- CallBestModel(CalibOut = CalibOut, minErrorRun = minErrorRun, Rho = CalibParamOut[minErrorRun, "rho"] * input$opt_rho_factor)
    alt_answer <- cumsum(alt$Dx)[251] - alt$Dx[1]
    tags$code(scales::comma(round(alt_answer - base_answer, digits = 0)))
})

# Linkage Intervention
output$opt_q_maxProp <- renderUI({
    numericInput(inputId = "opt_q_factor",
        label = "Maximum Intervention Proportion:",
        value = 1,
        min = round(CalibParamOut[minErrorRun, "q"], digits = 3),
        max = 1,
        step = 0.001,
        width = "100%")
})

output$opt_q_baseline <- renderUI({
    baseline <- CallBestModel(CalibOut = CalibOut, minErrorRun = minErrorRun)
    answer <- cumsum(baseline$Care)[251] - baseline$Care[1]
    tags$code(scales::comma(round(answer, digits = 0)))
})

output$opt_q_max <- renderUI({
    baseline <- CallBestModel(CalibOut = CalibOut, minErrorRun = minErrorRun)
    base_answer <- cumsum(baseline$Care)[251] - baseline$Care[1]

    alt <- CallBestModel(CalibOut = CalibOut, minErrorRun = minErrorRun, q = input$opt_q_factor)
    alt_answer <- cumsum(alt$Care)[251] - alt$Care[1]
    tags$code(scales::comma(round(alt_answer - base_answer, digits = 0)))
})

# Pre-ART Retention Intervention
output$opt_kappa_baseline <- renderUI({
    baseline <- CallBestModel(CalibOut = CalibOut, minErrorRun = minErrorRun)
    answer <- cumsum(baseline$PreLtfu)[251] - baseline$PreLtfu[1]
    tags$code(scales::comma(round(answer, digits = 0)))
})

output$opt_kappa_max <- renderUI({
    baseline <- CallBestModel(CalibOut = CalibOut, minErrorRun = minErrorRun)
    base_answer <- cumsum(baseline$PreLtfu)[251] - baseline$PreLtfu[1]

    alt <- CallBestModel(CalibOut = CalibOut, minErrorRun = minErrorRun, Kappa = CalibParamOut[minErrorRun, "kappa"] / input$opt_kappa_factor)
    alt_answer <- cumsum(alt$PreLtfu)[251] - alt$PreLtfu[1]
    tags$code(scales::comma(round(base_answer - alt_answer, digits = 0)))
})

# ART Initiation Intervention
output$opt_gamma_baseline <- renderUI({
    baseline <- CallBestModel(CalibOut = CalibOut, minErrorRun = minErrorRun)
    answer <- cumsum(baseline$Tx)[251] - baseline$Tx[1]
    tags$code(scales::comma(round(answer, digits = 0)))
})

output$opt_gamma_max <- renderUI({
    baseline <- CallBestModel(CalibOut = CalibOut, minErrorRun = minErrorRun)
    base_answer <- cumsum(baseline$Tx)[251] - baseline$Tx[1]

    alt <- CallBestModel(CalibOut = CalibOut, minErrorRun = minErrorRun, Gamma = CalibParamOut[minErrorRun, "gamma"] * input$opt_gamma_factor)
    alt_answer <- cumsum(alt$Tx)[251] - alt$Tx[1]
    tags$code(scales::comma(round(alt_answer - base_answer, digits = 0)))
})

# Adherence Intervention
output$opt_sigma_baseline <- renderUI({
    baseline <- CallBestModel(CalibOut = CalibOut, minErrorRun = minErrorRun)
    answer <- cumsum(baseline$Vs)[251] - baseline$Vs[1]
    tags$code(scales::comma(round(answer, digits = 0)))
})

output$opt_sigma_max <- renderUI({
    baseline <- CallBestModel(CalibOut = CalibOut, minErrorRun = minErrorRun)
    base_answer <- cumsum(baseline$Vs)[251] - baseline$Vs[1]

    alt <- CallBestModel(CalibOut = CalibOut, minErrorRun = minErrorRun, Sigma = input$opt_sigma_factor)
    alt_answer <- cumsum(alt$Vs)[251] - alt$Vs[1]
    tags$code(scales::comma(round(alt_answer - base_answer, digits = 0)))
})

# ART Retention Intervention
output$opt_omega_baseline <- renderUI({
    baseline <- CallBestModel(CalibOut = CalibOut, minErrorRun = minErrorRun)
    answer <- cumsum(baseline$Ltfu)[251] - baseline$Ltfu[1]
    tags$code(scales::comma(round(answer, digits = 0)))
})

output$opt_omega_max <- renderUI({
    baseline <- CallBestModel(CalibOut = CalibOut, minErrorRun = minErrorRun)
    base_answer <- cumsum(baseline$Ltfu)[251] - baseline$Ltfu[1]

    alt <- CallBestModel(CalibOut = CalibOut, minErrorRun = minErrorRun, Omega = CalibParamOut[minErrorRun, "omega"] / input$opt_omega_factor)
    alt_answer <- cumsum(alt$Ltfu)[251] - alt$Ltfu[1]
    tags$code(scales::comma(round(base_answer - alt_answer, digits = 0)))
})