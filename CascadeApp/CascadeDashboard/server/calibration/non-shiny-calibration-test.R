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
source("server/calibration/initial.R", local = FALSE)
time <- seq(0, 5, 1)
p <- parameters(
    prop_preART_500    = KenyaData[["cd4"]][1,"prop.Off.ART.500"][[1]],
    prop_preART_350500 = KenyaData[["cd4"]][1,"prop.Off.ART.350500"][[1]],
    prop_preART_250350 = KenyaData[["cd4"]][1,"prop.Off.ART.250350"][[1]],
    prop_preART_200250 = KenyaData[["cd4"]][1,"prop.Off.ART.200250"][[1]],
    prop_preART_100200 = KenyaData[["cd4"]][1,"prop.Off.ART.100200"][[1]],
    prop_preART_50100  = KenyaData[["cd4"]][1,"prop.Off.ART.50100"][[1]],
    prop_preART_50     = KenyaData[["cd4"]][1,"prop.Off.ART.50"][[1]],
    t_1 = ConvertYear(KenyaData[["treatment_guidelines"]][["more500"]]),
    t_2 = ConvertYear(KenyaData[["treatment_guidelines"]][["less500"]]),
    t_3 = ConvertYear(KenyaData[["treatment_guidelines"]][["less350"]]),
    t_4 = ConvertYear(KenyaData[["treatment_guidelines"]][["less250"]]),
    t_5 = ConvertYear(KenyaData[["treatment_guidelines"]][["less200"]])
    )
y <- GetCalibInitial(p, KenyaData)
i <- incidence(as.double(KenyaData[["incidence"]])) # Can only replace this array if you enter a replacement of the same length (7). Also, if not as.double() ode() throws an error.

# Parameter update
# beta <- GetBeta(y, p)

# Treatment Guidelines (t_1 to t_5)
# Need some assumptions here around WHO Stage / CD4 levels

# ConvertYear to alter treatment_guidelines into something that the model can understand


# source calibration model function (with setup and data.frame return)
source("server/calibration/model.R", local = FALSE)

# Running the model
result <- CallCalibModel(time, y, p, i)

# Feed values for "kenya" into calibration version of the model
# Compare 'KenyaData' to 'result'

# Test with one variable at a time.
# The way I see this, is that we have a quartz window with 4 segments.
# Each a comparison between model and data (eventually will be merged onto one plot).
# Functions to show the error between model and data.

# Function to calculate error in a data.frame
# Throw this into the error.R file.
source("server/calibration/error.R", local = FALSE)

# this function is held in error.R
df <- AssembleComparisonDataFrame(country = "Kenya", model = result, data = KenyaData)

## Pass to SSE() and return an updated data.frame
# Genius
error <- SSE(df)

p1 <- ggplot(dplyr::filter(error, indicator == "PLHIV"), aes(x = year, y = value, group = source)) +
    geom_line() + geom_point(aes(color = indicator, shape = source), size = 3)

p2 <- ggplot(dplyr::filter(error, indicator == "PLHIV Diagnosed"), aes(x = year, y = value, group = source)) +
    geom_line() + geom_point(aes(color = indicator, shape = source), size = 3)

p3 <- ggplot(dplyr::filter(error, indicator == "PLHIV in Care"), aes(x = year, y = value, group = source)) +
    geom_line() + geom_point(aes(color = indicator, shape = source), size = 3)

p4 <- ggplot(dplyr::filter(error, indicator == "PLHIV on ART"), aes(x = year, y = value, group = source)) +
    geom_line() + geom_point(aes(color = indicator, shape = source), size = 3)

graphics.off()
quartz.options(w = 10, h = 5)
gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)


# Without Error in data.frame #
error2 <- dplyr::filter(error, source != "error")
p1 <- ggplot(dplyr::filter(error2, indicator == "PLHIV"), aes(x = year, y = value, group = source)) +
    geom_line() + geom_point(aes(color = indicator, shape = source), size = 3)

p2 <- ggplot(dplyr::filter(error2, indicator == "PLHIV Diagnosed"), aes(x = year, y = value, group = source)) +
    geom_line() + geom_point(aes(color = indicator, shape = source), size = 3)

p3 <- ggplot(dplyr::filter(error2, indicator == "PLHIV in Care"), aes(x = year, y = value, group = source)) +
    geom_line() + geom_point(aes(color = indicator, shape = source), size = 3)

p4 <- ggplot(dplyr::filter(error2, indicator == "PLHIV on ART"), aes(x = year, y = value, group = source)) +
    geom_line() + geom_point(aes(color = indicator, shape = source), size = 3)

graphics.off()
quartz.options(w = 10, h = 5)
gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)

# Pull out error.
a <- dplyr::filter(error, source == "error")

ggplot(a, aes(x = year, y = value)) + geom_point(aes(color = indicator), size = 3)

# TOTAL ERROR! (to minimise)
sum(a$value)

#####################
# START OF FUNCTION #
#####################

# GetCountryMasterData
KenyaData <- GetMasterDataSet("Kenya")

# Set some crap.
time <- seq(0, 5, 1)
p <- parameters(
    prop_preART_500    = data[["cd4"]][1,"prop.Off.ART.500"][[1]],
    prop_preART_350500 = data[["cd4"]][1,"prop.Off.ART.350500"][[1]],
    prop_preART_250350 = data[["cd4"]][1,"prop.Off.ART.250350"][[1]],
    prop_preART_200250 = data[["cd4"]][1,"prop.Off.ART.200250"][[1]],
    prop_preART_100200 = data[["cd4"]][1,"prop.Off.ART.100200"][[1]],
    prop_preART_50100  = data[["cd4"]][1,"prop.Off.ART.50100"][[1]],
    prop_preART_50     = data[["cd4"]][1,"prop.Off.ART.50"][[1]],
    t_1 = ConvertYear(data[["treatment_guidelines"]][["more500"]]),
    t_2 = ConvertYear(data[["treatment_guidelines"]][["less500"]]),
    t_3 = ConvertYear(data[["treatment_guidelines"]][["less350"]]),
    t_4 = ConvertYear(data[["treatment_guidelines"]][["less250"]]),
    t_5 = ConvertYear(data[["treatment_guidelines"]][["less200"]])
    )
y <- GetCalibInitial(p, data)
i <- incidence(as.double(data[["incidence"]]))

# The following parameters can then be updated on the fly.
p[["Rho"]]
p[["Epsilon"]]
p[["q"]]
p[["Kappa"]]
p[["Gamma"]]
p[["Theta"]]
p[["Omega"]]

# Need a SINGLE FUNCTION, that:
    # - Assembles model
    # - Takes changing parameter values (p)
    # - Runs model
    # - Analyses data & calculates ERROR

ERROR <- function(data, p) {

# data = MasterDataSet for a particular country
# Idea is that this function will be called multiple times and therefore needs to be relatively fast.

    out <- SSE(AssembleComparisonDataFrame(country = "Kenya", model = CallCalibModel(time, y, p, i), data = data))

}

# Hadleys new visual profiler (awesome, cool).
profvis::profvis({
    SSE(AssembleComparisonDataFrame(country = "Kenya", model = CallCalibModel(time, y, p, i), data = KenyaData))
})

#############
# WEDNESDAY #
#############
# Create a parameter set
# Use: FME::Latinhyper
help(Latinhyper)

# Example
parRange <- data.frame(min = c(0, 1, 2, 3), max = c(10, 9, 8, 7))
rownames(parRange) <- c("par1", "par2", "par3", "par4")
parRange
FME::Latinhyper(parRange, num = 10)

# We can then apply this to a function, i.e. SSE(AssembleComparisonDataFrame(CallCalibModel()))

# Finish creating error calculations and functions. = DONE.
# All numbers should be absolute at this stage (CallCalibModel now only returns absolute numbers) = DONE.
# Do each seperately first, then the pull them all together. = DONE.
# Alter assumptions around what values we put in at the start of calibration? (assuming ZERO PLHIV etc. maybe crazy!?) = DONE.


# ----------- #
# CALIBRATION #
# ----------- #

# LHS or optim()
# Plot test outputs
# dynamic labelling of input data
# What is the best strategy?

# IDEA:
    # - Pull all 'error' values together.
    # - Multiply by some weight, depending on what they are
    # - Ensure that we are still able to minimise values to ZERO.

# --------- #
# WEIGHTING #
# --------- #

# How do we weight studies?????
