# Script to contain error calculation functions
# Function to calculate error in a data.frame

# AssembleComparisonDataFrame for passing to Error function
AssembleComparisonDataFrame <- function(country, model, data) {

    # Create Model data.frame
    modelOutput <- data.frame()
    for(i in 1:4) {
        year <- model$time + 2010
        source <- "model"
        if(i == 1) {
            value <- model$N
            indicator <- "PLHIV"
        } else if(i == 2) {
            value <- model$Dx + model$Care + model$PreLtfu + model$Tx + model$Ltfu
            indicator <- "PLHIV Diagnosed"
        } else if(i == 3) {
            value <- model$Care + model$Tx
            indicator <- "PLHIV in Care"
        } else if(i == 4) {
            value <- model$Tx
            indicator <- "PLHIV on ART"
        }

        iOutput <- data.frame(country, indicator, source, year, value)
        modelOutput <- rbind(modelOutput, iOutput)
    }
    modelOutput

    # Create Data data.frame
    dataOutput <- data.frame()

    for(i in 1:4) {
        if(i == 1) {
            dOutput <- data[["calib"]][data[["calib"]]$indicator == "PLHIV",]
        } else if(i == 2) {
            dOutput <- data[["calib"]][data[["calib"]]$indicator == "PLHIV Diagnosed",]
        } else if(i == 3) {
            dOutput <- data[["calib"]][data[["calib"]]$indicator == "PLHIV in Care",]
        } else if(i == 4) {
            dOutput <- data[["calib"]][data[["calib"]]$indicator == "PLHIV on ART",]
        }
        dOutput$source <- "data"
        dataOutput <- rbind(dataOutput, dOutput)
    }
    dataOutput

    output <- rbind(dataOutput, modelOutput)
    output
}

# This will need to be able to handle ALL FOUR ERRORS and return a neat data.frame of errors in return.
SSE <- function(df) {
    # if(!is.data.frame(df)) stop("Not passing a data frame.")
    uniqueIndicators <- unique(df$indicator)
    for(i in 1:length(uniqueIndicators)) {
        data <- df[df$indicator == uniqueIndicators[i],]

        uniqueYears <- unique(data$year)

        for(j in 1:length(uniqueYears)) {
            iYr <- data[data$year == uniqueYears[j],]

            iData  <- iYr[iYr$source == "data","value"]
            if(any(is.na(iData))) next
            if(length(iData) > 1) iData <- mean(iData)

            iModel <- iYr[iYr$source == "model","value"]

            value <- sum((iData - iModel)^2)

            year <- uniqueYears[j]

            iError <- data.frame(country = data$country[1], indicator = data$indicator[1], year, value, source = "error")

            df <- rbind(df, iError)
        }
    }
    df
}
