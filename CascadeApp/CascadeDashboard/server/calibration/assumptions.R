# This file will contain all the relevant assumptions for individual countries.
# At first this will be based upon the marrakech.csv data presented.
# But, we will eventually pivot to relying entirely on data entered into the model
    # Will need some testing architecture to sort that out.
    # i.e. a test to make sure values don't conflict - will need to be relatively clever too.

# source("calibration-data.R")
# uCountry <- "Kenya"
# countryData <- GetCountryData("Kenya")

# need to make judgement calls on each country. Regarding the data.

# So after GetCountryData() we need a MakeAssumptions() [to fill in the gaps]
MakeAssumptions <- function(uCountry, countryData) {

    if(uCountry == "Kenya") {
        # Do assumptiony stuff then.
        countryData <- countryData$calib

        # For now propogate assumptions over time.

        ## DIAGNOSES ##
        # In Kenya, we have no indication of # diagnosed in 2015 or any year previous
        # Luckily, we have data from neighbouring countries that we can use to generate a 'regional estimate'
        # dplyr::filter(test, indicator == "PLHIV")$value

        # What is we made the assumption of #diagnosed as the average of neighbouring countries? (for a particular time point)
        e.diag = 0.6436 # Ethiopia (2011)
        t.diag = 0.5859 # Tanzania (2011)
        u.diag = 0.6274 # Uganda (2011)

        # We assume that this carries over time
        k.plhiv <- dplyr::filter(countryData, indicator == "PLHIV")$value # Kenya

        # Expanding over timeframe
        country   <- "Kenya"
        indicator <- "PLHIV Diagnosed"
        year      <- seq(2010, 2015, 1)
        value     <- k.plhiv * mean(e.diag, t.diag, u.diag)
        weight    <- "red"
        new.diag  <- data.frame(country, indicator, year, value, weight)

        # test3 <- rbind(countryData, new.diag)
        # ggplot(test3, aes(x = year, y = value)) + geom_point(aes(color = indicator))

        # TEST TEST TEST
        # dplyr::filter(test3, indicator == "PLHIV")$value
        # dplyr::filter(test3, indicator == "PLHIV Diagnosed")$value
        # dplyr::filter(test3, indicator == "PLHIV on ART")$value

        # return the 'new' data.frame (pass to next lines)

        ## CARE ##
        # From Marrakech data we know that in 2015, 57% of PLHIV were in care.
        # As we don't have any care data for prior to 2015, we take this value and attach is for each year over time.

        # Building data.frame
        country   <- "Kenya"
        indicator <- "PLHIV in Care"
        year      <- seq(2010, 2015, 1)
        value     <- dplyr::filter(countryData, indicator == "PLHIV")$value * 0.57
        weight    <- "red"
        new.care  <- data.frame(country, indicator, year, value, weight)

        # test4 <- rbind(countryData, new.diag, new.care)
        # ggplot(test4, aes(x = year, y = value)) + geom_point(aes(color = indicator))

        # Caveats
        # In 2015, PLHIV in CARE < PLHIV on ART. This gets overwritten by Marrakech data in any case.
        # Might just truncate the data from 2015 and leave the rest.

        assumptions.return <- rbind(new.diag, new.care)

    } else {
        stop("No code written for generating assumptions on other countries aside from Kenya.")
    }
    # Return the assumptions data.frame taking the standard form as before.

    assumptions.return
}

# Careful not to return the countryData too.
# MakeAssumptions("Kenya", countryData)

