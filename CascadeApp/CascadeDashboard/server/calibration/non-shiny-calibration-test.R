# This is a calibration test script that will run a non-shiny version of the cascade model.
# For calibration testing, in the absence of all the bells and whistles that come with the shiny app.
# This script will be a template for future shiny versions of the model.
# Ideally, will just be a case of drag and drop of functions.
# Create all functions external to this script.

setwd("~/git/WhoCascade/CascadeApp/CascadeDashboard")
dir()

# -------- #
# WORKFLOW #
# -------- #

# --------- #
# STAGE ONE #
# --------- #

# source all the relevant files
source("server/calibration/master.R", local = FALSE)

# Test functions are all running and load "Kenya" data
KenyaData <- GetMasterDataSet("Kenya")
KenyaData

# load 'cascade' package and ensure it is the latest build.
devtools::load_all(pkg = "~/git/WhoCascade/cascade")

# Run 'test'
devtools::test(pkg = "~/git/WhoCascade/cascade")

# Pull out functions used by the model
source("server/calibration/initial.R")
time <- seq(0, 5, 1)
p <- parameters()
iPLHIV <- (dplyr::filter(KenyaData[["calib"]], indicator == "PLHIV") %>% dplyr::filter(year == 2010) %>% select(value))[[1]]
y <- GetCalibInitial(p,
    UnDx_500 =    iPLHIV * KenyaData[["cd4"]][1,2][[1]],
    UnDx_350500 = iPLHIV * KenyaData[["cd4"]][1,3][[1]],
    UnDx_250350 = iPLHIV * KenyaData[["cd4"]][1,4][[1]],
    UnDx_200250 = iPLHIV * KenyaData[["cd4"]][1,5][[1]],
    UnDx_100200 = iPLHIV * KenyaData[["cd4"]][1,6][[1]],
    UnDx_50100 =  iPLHIV * KenyaData[["cd4"]][1,7][[1]],
    UnDx_50 =     iPLHIV * KenyaData[["cd4"]][1,8][[1]]
)
i <- incidence(as.double(KenyaData[["incidence"]])) # Can only replace this array if you enter a replacement of the same length (7). Also, if not as.double() ode() throws an error.




# Parameter update
# beta <- GetBeta(y, p)
# p[62] <- beta # p[["beta"]]
p[50] <- KenyaData[["cd4"]][1,2][[1]] # p[["Iota_1"]]
p[51] <- KenyaData[["cd4"]][1,3][[1]] # p[["Iota_2"]]
p[52] <- KenyaData[["cd4"]][1,4][[1]] # p[["Iota_3"]]
p[53] <- KenyaData[["cd4"]][1,5][[1]] # p[["Iota_4"]]
p[54] <- KenyaData[["cd4"]][1,6][[1]] # p[["Iota_5"]]
p[55] <- KenyaData[["cd4"]][1,7][[1]] # p[["Iota_6"]]
p[56] <- KenyaData[["cd4"]][1,8][[1]] # p[["Iota_7"]]

# Treatment Guidelines (t_1 to t_5)
# Need some assumptions here around WHO Stage / CD4 levels

# ConvertYear to alter treatment_guidelines into something that the model can understand
ConvertYear <- function(year) {
    if(is.na(year)) return(10)
    if(!is.numeric(year)) stop("Non-numeric value passed to ConvertYear()")
    if((year - 2010) <= 0) {
        return(0)
    } else {
        return(year - 2010)
    }
}

p[["t_1"]] <- ConvertYear(KenyaData[["treatment_guidelines"]][["more500"]])
p[["t_2"]] <- ConvertYear(KenyaData[["treatment_guidelines"]][["less500"]])
p[["t_3"]] <- ConvertYear(KenyaData[["treatment_guidelines"]][["less350"]])
p[["t_4"]] <- ConvertYear(KenyaData[["treatment_guidelines"]][["less250"]])
p[["t_5"]] <- ConvertYear(KenyaData[["treatment_guidelines"]][["less200"]])

# source calibration model function (with setup and data.frame return)
source("server/calibration/model.R")

result <- CallCalibModel(time, y, p, i)

# Feed values for "kenya" into calibration version of the model
# Compare 'KenyaData' to 'result'
test <- KenyaData[["calib"]]
test

names(result)

# Test with one variable at a time.
# The way I see this, is that we have a quartz window with 4 segments.
# Each a comparison between model and data (eventually will be merged onto one plot).
# Functions to show the error between model and data.

# PLHIV
year <- result$time + 2010
value <- result$N
indicator <- "PLHIV"
country <- "Kenya"
source <- "model"

modelOutput <- data.frame(country, indicator, source, year, value)

dataOutput <- dplyr::filter(KenyaData[["calib"]], indicator == "PLHIV")

dataOutput <- dplyr::mutate(dataOutput, source = "data")

output <- rbind(dataOutput, modelOutput)
output

ggplot(output, aes(x = year, y = value, group = source)) + geom_line() + geom_point(aes(color = indicator, shape = source), size = 3)

output

length(unique(df$year))

# Function to calculate error in a data.frame
# Throw this into the error.R file.
SSE <- function(df) {
    if(!is.data.frame(df)) stop("Not passing a data frame.")

    uniqueYears <- unique(df$year)

    for(i in 1:length(uniqueYears)) {
        iYr <- dplyr::filter(df, year == uniqueYears[i])

        iData  <- (dplyr::filter(iYr, source == "data") %>% select(value))[[1]]
        iModel <- (dplyr::filter(iYr, source == "model") %>% select(value))[[1]]

        value <- sum((iData - iModel)^2)

        year <- uniqueYears[i]

        iError <- data.frame(country = df$country[1], indicator = df$indicator[1], year, value, source = "error")

        df <- rbind(df, iError)
    }
    df
}

PLHIV_Error <- SSE(output)


test <- dplyr::filter(PLHIV_Error, source %in% c("model", "data"))

ggplot(test, aes(x = year, y = value, group = source)) + geom_point(aes(color = source))

# Need a calculate error function.
# how many types of error? (4)

KenyaData[["calib"]]

# PLHIV = DONE.

###########
# TUESDAY #
###########
# Finish creating error calculations and functions.

# PLHIV Diagnosed

# PLHIV in Care

# PLHIV on ART



# ----------- #
# CALIBRATION #
# ----------- #

# LHS or optim()
# Plot test outputs
# dynamic labelling of input data
# What is the best strategy?

# --------- #
# WEIGHTING #
# --------- #

# How do we weight studies?????
