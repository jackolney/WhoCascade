# Saving input values from setup tab.
saveCascadeData <- function(data) {
    # Grab the Google Sheet
    sheet <- gs_title("WHO-Cascade-Data")
    # Add the data as a new row
    gs_add_row(sheet, input = data)
}

# Google Sheet API Interface

locateSheet <- function() {
    return(gs_title("SpectrumIncidenceEstimates"))
}

getIncidenceData <- function(theTable) {
    return(gs_read(theTable,ws="NewInfections"))
}

getCD4Data <- function(theTable) {
    return(gs_read(theTable,ws="CD4-Distribution"))
}
