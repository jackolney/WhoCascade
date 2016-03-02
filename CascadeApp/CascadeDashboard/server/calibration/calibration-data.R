# require(readr)

dir()

# Setup Page -> Multiple Tabs
# Location Tab -> Please Select Country (displays a map of available countries (interactive [plotGoogleMap package])).
# 1) Pick Country.
# on next page the model will pull in all data from Marrakech, colour code boxes according to the weights assigned to them ('traffic lights').
# User can adjust 'marrakech' values and relative weights.
# clicking 'next' will run the calibration (visualise this?, or at least the final plot -> fun shit load of curves on the screen).
# Then 'parameter' page will be entered, showing the "ghost values" that the model has decided upon (in grey or something).
# The user can then leave them, as is, OR alter them (re-run calibration if so?).
# Then proceed to view results -> presented as an average perhaps, or a range (tricky).


# This 'calibration-data.R' script will include functions that are called during calibration.
# When 'country' is picked, pull out relevant data from all the data/calibration and data/marrakech.csv files.
# Assemble dynamically into a calibration-data DATA.FRAME using a standard format; i.e.
# data.frame(<year>,<country>,<category>,<value>,<source>,<details>)
# Need to specify the initial values in the model?????
#       - Values for state variables in 2010?
#       - CD4 distribution (SPECTRUM -- Just update script for it) [could even update it every year]
# Then run the model, check SSE between model and data, optim to minimise (weighting how?)
# Algorithm for doing this multiple times?
# (Store results for each simulation?)
# Plot results?
# Evaluate transition rates that the model decied upon.

# Model runs between 2010 and 2015.
# uses marrakech data to specify state of care (targets) in either 2015/2014/2013.
# fixed parameter values between those times.
# Spectrum tells us incidence
# Need to account for changing treatment guidelines in this period (have csv of dates, just need to configure it).

# Country Selection
userCountry = "Kenya"

# What folders do I need to search through?


calib.files.path <- c()
for(i in 1:length(calib.files)) {
    print(i)
    calib.files.path[i] <- paste0("server/data/calibration/", calib.files[i])
}

# For all files in calib.files.path, do readr and identify all those with Kenya
readr::read_csv

a <- readr::read_csv(calib.files.path[1])

dplyr::filter(a, Country == "Kenya")


# Lets try that again.
# Read all csv data in at applicataion start, then we can just jump between countries as and when we select them.
c.file.path <- "server/data/calibration"
calib.incidence            <- readr::read_csv(paste0(c.file.path, "/incident-infections.csv"),   col_names = TRUE, skip = 1)
calib.cd4                  <- readr::read_csv(paste0(c.file.path, "/cd4-distribution-2010.csv"), col_names = TRUE, skip = 0)
calib.art                  <- readr::read_csv(paste0(c.file.path, "/art.csv"),                   col_names = TRUE, skip = 1)
calib.hiv_awareness_unaids <- readr::read_csv(paste0(c.file.path, "/hiv-awareness-unaids.csv"),  col_names = TRUE, skip = 0)
calib.not_on_art           <- readr::read_csv(paste0(c.file.path, "/not-on-art.csv"),            col_names = TRUE, skip = 1)
calib.plhiv                <- readr::read_csv(paste0(c.file.path, "/plhiv.csv"),                 col_names = TRUE, skip = 1)
calib.previous_data        <- readr::read_csv(paste0(c.file.path, "/previous-data.csv"),         col_names = TRUE, skip = 0)
calib.rates                <- readr::read_csv(paste0(c.file.path, "/rates.csv"),                 col_names = TRUE, skip = 0)
calib.treatment_guidelines <- readr::read_csv(paste0(c.file.path, "/treatment-guidelines.csv"),  col_names = TRUE, skip = 0)

# For each find userCountry
userCountry

calib.incidence[[userCountry]]

# something along the lines of:

GetCountryData <- function(uCountry) {
    # This will then return a data.frame containing ALL available data.

    # But how do we handle counties with values missing?
    # Return a list of three things?
    #   [1] - Incidence and CD4 (the ESSENTIALS)
    #   [2] - Additional data points (varied width)
    #   [3] - Treatment Guideline changes for the specific country
    #   [4] - Rates (if available) [only will be possible for like 3 countries who supplied rates in Marrakech]
    # Think we need a standard format here??
}




calib.incidence$Kenya

# Essentials (list elements 1 and 2)
calib.incidence[[userCountry]]
dplyr::filter(calib.cd4, Country == userCountry)

# Additional Stuff (to be pulled together somehow) (list element 3)
dplyr::filter(calib.art, Country == userCountry)
dplyr::filter(calib.hiv_awareness_unaids, country == userCountry)
dplyr::filter(calib.not_on_art, Country == userCountry)
dplyr::filter(calib.plhiv, Country == userCountry)
dplyr::filter(calib.previous_data, country == userCountry)

# (list element 4)
dplyr::filter(calib.treatment_guidelines, country == userCountry)
# (list element 5)
dplyr::filter(calib.rates, country == userCountry)


# Create List()
calib.df <- list()
calib.df[[1]] <- calib.incidence[[userCountry]]
calib.df[[2]] <- dplyr::filter(calib.cd4, country == userCountry)
calib.df[[3]] <- dplyr::filter(calib.art, country == userCountry)
calib.df[[4]] <- dplyr::filter(calib.hiv_awareness_unaids, country == userCountry)
calib.df[[5]] <- dplyr::filter(calib.not_on_art, country == userCountry)
calib.df[[6]] <- dplyr::filter(calib.plhiv, country == userCountry)
calib.df[[7]] <- dplyr::filter(calib.previous_data, country == userCountry)
calib.df[[8]] <- dplyr::filter(calib.rates, country == userCountry)
calib.df[[9]] <- dplyr::filter(calib.treatment_guidelines, country == userCountry)

# Create a vector of data names
names(calib.df) <- c(
    "incidence",
    "cd4",
    "art",
    "hiv_awareness_unaids",
    "not_on_art",
    "plhiv",
    "previous_data",
    "rates",
    "treatment_guidelines"
    )

# Define is.not.empty function
is.not.empty <- function(ListElement) {
    if(!is.list(ListElement)) stop("not a list")
    if(dim(ListElement)[1] == 0) {
        return(FALSE)
    } else {
        return(TRUE)
    }
}

is.not.empty(calib.df[[4]])

# Start reshaping from here.

# calib.art
if(is.not.empty(calib.df$art)) {
    temp.art <- reshape2::melt(calib.df$art[c("country","indicator","2010","2011","2012","2013","2014","2015")],
        id.vars = c("country", "indicator"),
        variable.name = "year",
        value.name = "value")
}

# calib.hiv_awareness_unaids
if(is.not.empty(calib.df$hiv_awareness_unaids)) {
    temp.hiv_awareness_unaids <- reshape2::melt(calib.df$hiv_awareness_unaids[c("country","year","value")],
        id.vars = c("country", ""))
}

calib.hiv_awareness_unaids

# calib.not_on_art

# calib.plhiv

# calib.previous_data


# List of temp.names
temp.names <- c("blah","blah","blah")
# if each exists, then cbind that shit.
exists("temp.art")


calib.df

(calib.df[[1]])
# annoyingly, 'incidence' doesn't fit with everything else... urgh.
calib.df

# Need a list-wide check function that will tell us if any elements are missing.
# Alert the user, then proceed to next step.

# We essentially need a whole bunch of points in a single data.frame
calib.df$previous_data

calib.df


test <- reshape2::melt(calib.art)

a <- dplyr::filter(test, Country == "Kenya")$value
