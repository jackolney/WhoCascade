# This file will source all the others and pull together a 'master data set' including:
#   - Previous data on the cascade (calibration-data.R)
#   - Assumptions (assumptions.R)
#   - Marrakech data (as the end target) (marrakech-data.R)

source("calibration-data.R")
source("assumptions.R")
source("marrakech-data.R")

# Then pull all this together into one 'master function'.

# THIS, is what the model will use as data and calculate error against.

# Everything will take an argument of 'uCountry' which will be 'userCountry'
# This will be called / generated once someone clicks on the map / picks from the dropdown menu

# This function will need to run some tests on the data.set to make sure that it is sensical.

# Set country
userCountry <- "Kenya"

GetMasterDataSet <- function(userCountry) {
    # Get all the data (all your base)
    countryData <- GetCountryData(userCountry)
    countryAssumptions <- MakeAssumptions(userCountry, countryData)
    marrakechData <- GetMarrakechData(userCountry)

    # Filter out year of Marrakech data, before binding.
    intermediate <- dplyr::filter(rbind(countryData$calib, countryAssumptions), year != marrakechData$year[1])

    # MASTER DATA SET (for calibration)
    countryMasterDataSet <- rbind(intermediate, marrakechData)

    # ggplot(countryMasterDataSet, aes(x = year, y = value, group = indicator)) +
    #     geom_line(aes(color = indicator)) +
    #     geom_point(aes(color = indicator)) +
    #     theme_classic()

    # Overwrite calib on countryData
    countryData$calib <- countryMasterDataSet

    # Return final list
    countryData
}

GetMasterDataSet("Kenya")[["calib"]]
