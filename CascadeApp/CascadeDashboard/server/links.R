# 'NEXT' button control
observeEvent(input$NEXT_intro,      {updateTabItems(session, inputId = "sideBar", selected = "country")})

observeEvent(input$NEXT_country,    {updateTabItems(session, inputId = "sideBar", selected = "plhiv")})

observeEvent(input$NEXT_plhiv,      {updateTabItems(session, inputId = "sideBar", selected = "diagnosis")})

observeEvent(input$NEXT_diag,       {updateTabItems(session, inputId = "sideBar", selected = "linkage")})

observeEvent(input$NEXT_care,       {updateTabItems(session, inputId = "sideBar", selected = "treatment")})

observeEvent(input$NEXT_art,        {updateTabItems(session, inputId = "sideBar", selected = "suppression")})

observeEvent(input$NEXT_viral,      {updateTabItems(session, inputId = "sideBar", selected = "calibration")})

observeEvent(input$wizardResults_1, {updateTabItems(session, inputId = "sideBar", selected = "your_cascade")})

observeEvent(input$wizardResults_2, {updateTabItems(session, inputId = "sideBar", selected = "care_cascade")})

observeEvent(input$wizardResults_3, {updateTabItems(session, inputId = "sideBar", selected = "powers_cascade")})

observeEvent(input$wizardResults_4, {updateTabItems(session, inputId = "sideBar", selected = "_909090")})

observeEvent(input$wizardResults_5, {updateTabItems(session, inputId = "sideBar", selected = "incidence_mortality")})

observeEvent(input$wizardOpt_1,     {updateTabItems(session, inputId = "sideBar", selected = "opt_cost")})

observeEvent(input$wizardOpt_2,     {updateTabItems(session, inputId = "sideBar", selected = "opt_parameter")})

observeEvent(input$wizardOpt_3,     {updateTabItems(session, inputId = "sideBar", selected = "opt_results")})

observeEvent(input$wizardOpt_4,     {updateTabItems(session, inputId = "sideBar", selected = "opt_budget")})


# observeEvent(input$wizardCalibration, {updateTabItems(session, inputId = "sideBar", selected = "calibration")})

# observeEvent(input$calib_accept, {updateTabItems(session, inputId = "sideBar", selected = "parameters")})
