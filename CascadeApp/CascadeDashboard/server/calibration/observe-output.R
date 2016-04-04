# Diagnosis Rate = rho
output$calib_rho_max <- renderUI({
    numericInput("test_DiagRate_U","Upper:", value = round(CalibParamMaxMin$rho_MAX, 4), min = 0, max = 100, step = 1e-6, width = '100%')
})

output$calib_rho_min <- renderUI({
    numericInput("test_DiagRate_L","Lower:", value = round(CalibParamMaxMin$rho_MIN, 4), min = 0, max = 100, step = 1e-6, width = '100%')
})

# Linkage Proportion = q
output$calib_q_max <- renderUI({
    numericInput("test_LinkProp_U","Upper:", value = round(CalibParamMaxMin$q_MAX, 4), min = 0, max = 100, step = 1e-6, width = '100%')

})

output$calib_q_min <- renderUI({
    numericInput("test_LinkProp_L","Lower:", value = round(CalibParamMaxMin$q_MIN, 4), min = 0, max = 100, step = 1e-6, width = '100%')
})

# ART Initiation Rate = gamma
output$calib_gamma_max <- renderUI({
    numericInput("test_ARTRate_U","Upper:", value = round(CalibParamMaxMin$gamma_MAX, 4), min = 0, max = 100, step = 1e-6, width = '100%')

})

output$calib_gamma_min <- renderUI({
    numericInput("test_ARTRate_L","Lower:", value = round(CalibParamMaxMin$gamma_MIN, 4), min = 0, max = 100, step = 1e-6, width = '100%')
})

# ART Initiation Rate (Side Door) = theta
output$calib_theta_max <- renderUI({
    numericInput("test_ARTsideRate_U","Upper:", value = round(CalibParamMaxMin$theta_MAX, 4), min = 0, max = 100, step = 1e-6, width = '100%')

})

output$calib_theta_min <- renderUI({
    numericInput("test_ARTsideRate_L","Lower:", value = round(CalibParamMaxMin$theta_MIN, 4), min = 0, max = 100, step = 1e-6, width = '100%')
})

# Pre-ART Dropout Rate = kappa
output$calib_kappa_max <- renderUI({
    numericInput("test_PreARTDropRate_U","Upper:", value = round(CalibParamMaxMin$kappa_MAX, 4), min = 0, max = 100, step = 1e-6, width = '100%')

})

output$calib_kappa_min <- renderUI({
    numericInput("test_PreARTDropRate_L","Lower:", value = round(CalibParamMaxMin$kappa_MIN, 4), min = 0, max = 100, step = 1e-6, width = '100%')
})

# ART Dropout Rate = omega
output$calib_omega_max <- renderUI({
    numericInput("test_ARTDropRate_U","Upper:", value = round(CalibParamMaxMin$omega_MAX, 4), min = 0, max = 100, step = 1e-6, width = '100%')

})

output$calib_omega_min <- renderUI({
    numericInput("test_ARTDropRate_L","Lower:", value = round(CalibParamMaxMin$omega_MIN, 4), min = 0, max = 100, step = 1e-6, width = '100%')
})

# Natural Mortality Rate = mu
output$calib_mu_max <- renderUI({
    numericInput("test_NatMortRate_U","Upper:", value = round(CalibParamMaxMin$mu_MAX, 4), min = 0, max = 100, step = 1e-6, width = '100%')

})

output$calib_mu_min <- renderUI({
    numericInput("test_NatMortRate_L","Lower:", value = round(CalibParamMaxMin$mu_MIN, 4), min = 0, max = 100, step = 1e-6, width = '100%')
})

# Adherence Proportion = p
output$calib_p_max <- renderUI({
    numericInput("test_AdhProp_U","Upper:", value = round(CalibParamMaxMin$p_MAX, 4), min = 0, max = 100, step = 1e-6, width = '100%')

})

output$calib_p_min <- renderUI({
    numericInput("test_AdhProp_L","Lower:", value = round(CalibParamMaxMin$p_MIN, 4), min = 0, max = 100, step = 1e-6, width = '100%')
})
