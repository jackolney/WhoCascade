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
        # In Kenya, we know that 46.9% of PLHIV were diagnosed in 2012.
        # Spectrum tells us that there are 1327788 PLWHIV in 2012.
        # 1327788 * 0.469 = 622733
        # However, we do not know the values for other years so we make the assumption it holds over time.

        # So 46.9% aware of status in 2012
        k.propDiag <- 0.469 # KAIS2012

        # We assume that this carries over time
        k.plhiv <- countryData[countryData$indicator == "PLHIV" & countryData$year != 2012,"value"]

        # Expanding over timeframe
        country   <- "Kenya"
        indicator <- "PLHIV Diagnosed"
        year      <- c(2010, 2011, 2013, 2014, 2015)
        value     <- k.plhiv * k.propDiag
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
        # But that is from using the Kenyan estimate of '# PLHIV'
        # If we use our Spectrum estimate for PLHIV, we find that 67% of PLHIV were in care in 2015.
        # But, both of these values, cause issues, because in 2012, only 47% of PLHIV were diagnosed...
        # Therefore prior to 2015, fewer individuals must have been in care.
        # But, perhaps we should leave the model as is, and allow THE MODEL to figure it all out.

        # Building data.frame
        country   <- "Kenya"
        indicator <- "PLHIV in Care"
        year      <- seq(2010, 2015, 1)
        # value     <- countryData[countryData$indicator == "PLHIV","value"] * 0.57
        value     <- countryData[countryData$indicator == "PLHIV","value"] * 0.67
        weight    <- "red"
        new.care  <- data.frame(country, indicator, year, value, weight)

        # test4 <- rbind(countryData, new.diag, new.care)
        # ggplot(test4, aes(x = year, y = value)) + geom_point(aes(color = indicator))

        # Caveats
        # In 2015, PLHIV in CARE < PLHIV on ART. This gets overwritten by Marrakech data in any case.
        # Might just truncate the data from 2015 and leave the rest.

        assumptions.return <- rbind(new.diag, new.care)

    } else {
        warning("No code written for generating assumptions on other countries aside from Kenya.")
        assumptions.return <- c()
    }
    # Return the assumptions data.frame taking the standard form as before.

    assumptions.return
}

# Careful not to return the countryData too.
# MakeAssumptions("Kenya", countryData)
