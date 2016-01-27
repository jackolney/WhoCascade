# StartUp Caution Alert
createAlert(session,
    anchorId = "startAlert",
    alertId = NULL,
    title = "WARNING",
    content = "This interactive web-based model is still under development.
    Any data entered into the model is done so at the users own risk.
    Clicking 'save' in any tab saves the current inputs to a centrally accessible spreadsheet hosted by Google.
    Results produced by this model are not finalised.
    Use with caution!",
    style = "danger",
    dismiss = TRUE,
    append = TRUE)

# PopOver
addPopover(session, id = "plotOpt909090",
    title = "Info",
    content = "Vertical line represents 73% viral suppression (end goal of 90-90-90 targets, 0.9^3 = 0.729).",
    placement = "bottom",
    trigger = "hover",
    options = NULL)
