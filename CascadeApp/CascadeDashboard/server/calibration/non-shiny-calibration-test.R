# This is a calibration test script that will run a non-shiny version of the cascade model.
# For calibration testing, in the absence of all the bells and whistles that come with the shiny app.
# This script will be a template for future shiny versions of the model.
# Ideally, will just be a case of drag and drop of functions.
# Create all functions external to this script.
rm(list=ls())
setwd("~/git/WhoCascade/CascadeApp/CascadeDashboard")
dir()

# -------- #
# WORKFLOW #
# -------- #

# --------- #
# STAGE ONE #
# --------- #

# source all the relevant files
source("server/calibration/master.R",  local = FALSE)
source("server/calibration/initial.R", local = FALSE)
source("server/calibration/model.R",   local = FALSE)
source("server/calibration/error.R",   local = FALSE)

# This contains simple function calls for the models in various permutations
source("server/calibration/misc-functions.R", local = FALSE)

# load 'cascade' package and ensure it is the latest build.
devtools::load_all(pkg = "~/git/WhoCascade/cascade")
devtools::test(pkg = "~/git/WhoCascade/cascade")

# Run baseline model (nothing fancy)
RunBaselineModel()


#####################
# START OF FUNCTION #
#####################

# Need a SINGLE FUNCTION, that:
    # - Assembles model
    # - Takes changing parameter values (p)
    # - Runs model
    # - Analyses data & calculates ERROR

# Hadleys new visual profiler (awesome, cool).
# profvis::profvis({
#     SSE(AssembleComparisonDataFrame(country = "Kenya", model = CallCalibModel(time, y, p, i), data = KenyaData))
# })

#############
# WEDNESDAY #
#############
# Create a parameter set
# Use: FME::Latinhyper

# WHAT assumptions do we make about model parameters here?
# These are ALL based on the previous Kenya model - perhaps we should start with a more blank slate here?
# We want a simple calibration, that is also fast.

# BASELINE SIMULATION.
# BREAK IT DOWN. ONLY LOOK AT ONE PARAMETER AT A TIME.
# Start with RHO

# Run Calibration Model
RunCalibration(100)

# Varying RHO pushing it down super far so that it is rediculously quick.

1 / lhs[,"rho"][k]
1 / lhs[,"epsilon"][k]
1 / lhs[,"kappa"][k]
1 / lhs[,"gamma"][k]
1 / lhs[,"theta"][k]
1 / lhs[,"omega"][k]
lhs[,"q"][k]


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
