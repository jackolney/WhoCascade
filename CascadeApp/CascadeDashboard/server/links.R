# 'NEXT' button control
# Rehash all these links, they are rather junky.

# Introduction
observeEvent(input$NEXT_intro, {
    updateTabItems(session,
        inputId = "sideBar",
        selected = "country"
    )
})

# Country
observeEvent(input$PREV_country, {
    updateTabItems(session,
        inputId = "sideBar",
        selected = "introduction"
    )
})

observeEvent(input$NEXT_country, {
    updateTabItems(session,
        inputId = "sideBar",
        selected = "plhiv"
    )
})

# PLHIV
observeEvent(input$PREV_plhiv, {
    updateTabItems(session,
        inputId = "sideBar",
        selected = "country"
    )
})

observeEvent(input$NEXT_plhiv, {
    updateTabItems(session,
        inputId = "sideBar",
        selected = "diagnosis"
    )
})

# Diagnosis
observeEvent(input$PREV_diag, {
    updateTabItems(session,
        inputId = "sideBar",
        selected = "plhiv"
    )
})

observeEvent(input$NEXT_diag, {
    updateTabItems(session,
        inputId = "sideBar",
        selected = "linkage"
    )
})

# Care
observeEvent(input$PREV_care, {
    updateTabItems(session,
        inputId = "sideBar",
        selected = "diagnosis"
    )
})

observeEvent(input$NEXT_care, {
    updateTabItems(session,
        inputId = "sideBar",
        selected = "treatment"
    )
})

# ART
observeEvent(input$PREV_art, {
    updateTabItems(session,
        inputId = "sideBar",
        selected = "linkage"
    )
})

observeEvent(input$NEXT_art, {
    updateTabItems(session,
        inputId = "sideBar",
        selected = "suppression"
    )
})

# Viral Suppression
observeEvent(input$PREV_viral, {
    updateTabItems(session,
        inputId = "sideBar",
        selected = "treatment"
    )
})

observeEvent(input$NEXT_viral, {
    updateTabItems(session,
        inputId = "sideBar",
        selected = "calibration"
    )
})

# Calibration
observeEvent(input$NEXT_calib, {
    updateTabItems(session,
        inputId = "sideBar",
        selected = "your_cascade"
    )
})

# Your Cascade
observeEvent(input$PREV_yourCascade, {
    updateTabItems(session,
        inputId = "sideBar",
        selected = "calibration"
    )
})

observeEvent(input$NEXT_yourCascade, {
    updateTabItems(session,
        inputId = "sideBar",
        selected = "care_cascade"
    )
})

# Power's Cascade (deprecated)
# observeEvent(input$NEXT_careCascade, {
#     updateTabItems(session,
#         inputId = "sideBar",
#         selected = "powers_cascade"
#     )
# })

# CareCascade
observeEvent(input$PREV_careCascade, {
    updateTabItems(session,
        inputId = "sideBar",
        selected = "your_cascade"
    )
})

observeEvent(input$NEXT_careCascade, {
    updateTabItems(session,
        inputId = "sideBar",
        selected = "_909090"
    )
})

# PowersCascade (this page is inactive, but should still retain some functionality)
observeEvent(input$NEXT_powersCascade, {
    updateTabItems(session,
        inputId = "sideBar",
        selected = "_909090"
    )
})

# 90-90-90
observeEvent(input$PREV_909090, {
    updateTabItems(session,
        inputId = "sideBar",
        selected = "care_cascade"
    )
})

observeEvent(input$NEXT_909090, {
    updateTabItems(session,
        inputId = "sideBar",
        selected = "incidence_mortality"
    )
})

# Incidence / Mortality
observeEvent(input$PREV_incMort, {
    updateTabItems(session,
        inputId = "sideBar",
        selected = "_909090"
    )
})

observeEvent(input$NEXT_incMort, {
    updateTabItems(session,
        inputId = "sideBar",
        selected = "opt-intro"
    )
})

# Optimisation Introduction
observeEvent(input$PREV_optIntro, {
    updateTabItems(session,
        inputId = "sideBar",
        selected = "incidence_mortality"
    )
})

observeEvent(input$NEXT_optIntro, {
    updateTabItems(session,
        inputId = "sideBar",
        selected = "opt-wizard"
    )
})

# Optimisation Wizard
observeEvent(input$NEXT_optWizard, {
    updateTabItems(session,
        inputId = "sideBar",
        selected = "opt-results"
    )
})

# Not sure on this?
observeEvent(input$wizardOpt_2, {
    updateTabItems(session,
        inputId = "sideBar",
        selected = "opt-parameter"
    )
})

# Not sure on this?
observeEvent(input$wizardOpt_3, {
    updateTabItems(session,
        inputId = "sideBar",
        selected = "opt-2results"
    )
})

# Not sure either?
observeEvent(input$wizardOpt_4, {
    updateTabItems(session,
        inputId = "sideBar",
        selected = "opt-budget"
    )
})

# Optimisation Parameters
observeEvent(input$goToInts, {
    updateTabItems(session,
        inputId = "sideBar",
        selected = "opt-parameter"
    )
})

# Optimisation Costs
observeEvent(input$goToCost, {
    updateTabItems(session,
        inputId = "sideBar",
        selected = "opt-cost"
    )
})

# Not sure?
observeEvent(input$goToWizard1, {
    updateTabItems(session,
        inputId = "sideBar",
        selected = "opt-wizard"
    )
})

# Not sure?
observeEvent(input$goToWizard2, {
    updateTabItems(session,
        inputId = "sideBar",
        selected = "opt-wizard"
    )
})
