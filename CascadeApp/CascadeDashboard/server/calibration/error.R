# Script to contain error calculation functions
# Function to calculate error in a data.frame

# This will need to be able to handle ALL FOUR ERRORS and return a neat data.frame of errors in return.
SSE <- function(df) {
    if(!is.data.frame(df)) stop("Not passing a data frame.")

    uniqueYears <- unique(df$year)

    for(i in 1:length(uniqueYears)) {
        iYr <- dplyr::filter(df, year == uniqueYears[i])

        iData  <- (dplyr::filter(iYr, source == "data") %>% select(value))[[1]]
        if(length(iData) > 1) iData <- mean(iData)

        iModel <- (dplyr::filter(iYr, source == "model") %>% select(value))[[1]]

        value <- sum((iData - iModel)^2)

        year <- uniqueYears[i]

        iError <- data.frame(country = df$country[1], indicator = df$indicator[1], year, value, source = "error")

        df <- rbind(df, iError)
    }
    df
}
