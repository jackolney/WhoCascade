observeEvent(input$uPLHIV_source, {
    if(input$uPLHIV_source != "Please select source...") {
        if(input$uPLHIV_source %in% c("Mathematical Model", "Nationally Representative Study")) {
            output$uPLHIV_quality <- renderUI({
                bsButton(inputId = "_uPLHIV_quality_", label = "GOOD", style = "success", size = "large", block = TRUE, disabled = TRUE, icon = icon("thumbs-up", class = "fa-lg fa-fw", lib = "font-awesome"))
            })
        } else if(input$uPLHIV_source == "Peer-reviewed Study") {
            output$uPLHIV_quality <- renderUI({
                bsButton(inputId = "_uPLHIV_quality_", label = "AVERAGE", style = "warning", size = "large", block = TRUE, disabled = TRUE)
            })
        } else if(input$uPLHIV_source == "Estimate") {
            output$uPLHIV_quality <- renderUI({
                bsButton(inputId = "_uPLHIV_quality_", label = "POOR", style = "danger", size = "large", block = TRUE, disabled = TRUE, icon = icon("thumbs-down", class = "fa-lg fa-fw", lib = "font-awesome"))
            })
        }
    } else {
        output$uPLHIV_quality <- renderUI({})
    }
})

observeEvent(input$uDIAG_source, {
    if(input$uDIAG_source != "Please select source...") {
        if(input$uDIAG_source %in% c("Mathematical Model", "Nationally Representative Study")) {
            output$uDIAG_quality <- renderUI({
                bsButton(inputId = "_uDIAG_quality_", label = "GOOD", style = "success", size = "large", block = TRUE, disabled = TRUE, icon = icon("thumbs-up", class = "fa-lg fa-fw", lib = "font-awesome"))
            })
        } else if(input$uDIAG_source == "Peer-reviewed Study") {
            output$uDIAG_quality <- renderUI({
                bsButton(inputId = "_uDIAG_quality_", label = "AVERAGE", style = "warning", size = "large", block = TRUE, disabled = TRUE)
            })
        } else if(input$uDIAG_source == "Estimate") {
            output$uDIAG_quality <- renderUI({
                bsButton(inputId = "_uDIAG_quality_", label = "POOR", style = "danger", size = "large", block = TRUE, disabled = TRUE, icon = icon("thumbs-down", class = "fa-lg fa-fw", lib = "font-awesome"))
            })
        }
    } else {
        output$uDIAG_quality <- renderUI({})
    }
})

observeEvent(input$uCARE_source, {
    if(input$uCARE_source != "Please select source...") {
        if(input$uCARE_source %in% c("Mathematical Model", "Nationally Representative Study")) {
            output$uCARE_quality <- renderUI({
                bsButton(inputId = "_uCARE_quality_", label = "GOOD", style = "success", size = "large", block = TRUE, disabled = TRUE, icon = icon("thumbs-up", class = "fa-lg fa-fw", lib = "font-awesome"))
            })
        } else if(input$uCARE_source == "Peer-reviewed Study") {
            output$uCARE_quality <- renderUI({
                bsButton(inputId = "_uCARE_quality_", label = "AVERAGE", style = "warning", size = "large", block = TRUE, disabled = TRUE)
            })
        } else if(input$uCARE_source == "Estimate") {
            output$uCARE_quality <- renderUI({
                bsButton(inputId = "_uCARE_quality_", label = "POOR", style = "danger", size = "large", block = TRUE, disabled = TRUE, icon = icon("thumbs-down", class = "fa-lg fa-fw", lib = "font-awesome"))
            })
        }
    } else {
        output$uCARE_quality <- renderUI({})
    }
})

observeEvent(input$uART_source, {
    if(input$uART_source != "Please select source...") {
        if(input$uART_source %in% c("Mathematical Model", "Nationally Representative Study")) {
            output$uART_quality <- renderUI({
                bsButton(inputId = "_uART_quality_", label = "GOOD", style = "success", size = "large", block = TRUE, disabled = TRUE, icon = icon("thumbs-up", class = "fa-lg fa-fw", lib = "font-awesome"))
            })
        } else if(input$uART_source == "Peer-reviewed Study") {
            output$uART_quality <- renderUI({
                bsButton(inputId = "_uART_quality_", label = "AVERAGE", style = "warning", size = "large", block = TRUE, disabled = TRUE)
            })
        } else if(input$uART_source == "Estimate") {
            output$uART_quality <- renderUI({
                bsButton(inputId = "_uART_quality_", label = "POOR", style = "danger", size = "large", block = TRUE, disabled = TRUE, icon = icon("thumbs-down", class = "fa-lg fa-fw", lib = "font-awesome"))
            })
        }
    } else {
        output$uART_quality <- renderUI({})
    }
})

observeEvent(input$uVIRAL_source, {
    if(input$uVIRAL_source != "Please select source...") {
        if(input$uVIRAL_source %in% c("Mathematical Model", "Nationally Representative Study")) {
            output$uVIRAL_quality <- renderUI({
                bsButton(inputId = "_uVIRAL_quality_", label = "GOOD", style = "success", size = "large", block = TRUE, disabled = TRUE, icon = icon("thumbs-up", class = "fa-lg fa-fw", lib = "font-awesome"))
            })
        } else if(input$uVIRAL_source == "Peer-reviewed Study") {
            output$uVIRAL_quality <- renderUI({
                bsButton(inputId = "_uVIRAL_quality_", label = "AVERAGE", style = "warning", size = "large", block = TRUE, disabled = TRUE)
            })
        } else if(input$uVIRAL_source == "Estimate") {
            output$uVIRAL_quality <- renderUI({
                bsButton(inputId = "_uVIRAL_quality_", label = "POOR", style = "danger", size = "large", block = TRUE, disabled = TRUE, icon = icon("thumbs-down", class = "fa-lg fa-fw", lib = "font-awesome"))
            })
        }
    } else {
        output$uVIRAL_quality <- renderUI({})
    }
})

# Diagnosis Rate = rho
output$calib_rho_max <- renderUI({
    numericInput("test_DiagRate_U","Upper:", value = CalibParamMaxMin$rho_MAX, min = 0, max = 100, step = 1e-6, width = '100%')
})

output$calib_rho_min <- renderUI({
    numericInput("test_DiagRate_L","Lower:", value = CalibParamMaxMin$rho_MIN, min = 0, max = 100, step = 1e-6, width = '100%')
})

# Linkage Proportion = q
output$calib_q_max <- renderUI({
    numericInput("test_LinkProp_U","Upper:", value = CalibParamMaxMin$q_MAX, min = 0, max = 100, step = 1e-6, width = '100%')

})

output$calib_q_min <- renderUI({
    numericInput("test_LinkProp_L","Lower:", value = CalibParamMaxMin$q_MIN, min = 0, max = 100, step = 1e-6, width = '100%')
})

# ART Initiation Rate = gamma
output$calib_gamma_max <- renderUI({
    numericInput("test_ARTRate_U","Upper:", value = CalibParamMaxMin$gamma_MAX, min = 0, max = 100, step = 1e-6, width = '100%')

})

output$calib_gamma_min <- renderUI({
    numericInput("test_ARTRate_L","Lower:", value = CalibParamMaxMin$gamma_MIN, min = 0, max = 100, step = 1e-6, width = '100%')
})

# ART Initiation Rate (Side Door) = theta
output$calib_theta_max <- renderUI({
    numericInput("test_ARTsideRate_U","Upper:", value = CalibParamMaxMin$theta_MAX, min = 0, max = 100, step = 1e-6, width = '100%')

})

output$calib_theta_min <- renderUI({
    numericInput("test_ARTsideRate_L","Lower:", value = CalibParamMaxMin$theta_MIN, min = 0, max = 100, step = 1e-6, width = '100%')
})

# Pre-ART Dropout Rate = kappa
output$calib_kappa_max <- renderUI({
    numericInput("test_PreARTDropRate_U","Upper:", value = CalibParamMaxMin$kappa_MAX, min = 0, max = 100, step = 1e-6, width = '100%')

})

output$calib_kappa_min <- renderUI({
    numericInput("test_PreARTDropRate_L","Lower:", value = CalibParamMaxMin$kappa_MIN, min = 0, max = 100, step = 1e-6, width = '100%')
})

# ART Dropout Rate = omega
output$calib_omega_max <- renderUI({
    numericInput("test_ARTDropRate_U","Upper:", value = CalibParamMaxMin$omega_MAX, min = 0, max = 100, step = 1e-6, width = '100%')

})

output$calib_omega_min <- renderUI({
    numericInput("test_ARTDropRate_L","Lower:", value = CalibParamMaxMin$omega_MIN, min = 0, max = 100, step = 1e-6, width = '100%')
})

# Natural Mortality Rate = mu
output$calib_mu_max <- renderUI({
    numericInput("test_NatMortRate_U","Upper:", value = CalibParamMaxMin$mu_MAX, min = 0, max = 100, step = 1e-6, width = '100%')

})

output$calib_mu_min <- renderUI({
    numericInput("test_NatMortRate_L","Lower:", value = CalibParamMaxMin$mu_MIN, min = 0, max = 100, step = 1e-6, width = '100%')
})

# Adherence Proportion = p
output$calib_p_max <- renderUI({
    numericInput("test_AdhProp_U","Upper:", value = CalibParamMaxMin$p_MAX, min = 0, max = 100, step = 1e-6, width = '100%')

})

output$calib_p_min <- renderUI({
    numericInput("test_AdhProp_L","Lower:", value = CalibParamMaxMin$p_MIN, min = 0, max = 100, step = 1e-6, width = '100%')
})
