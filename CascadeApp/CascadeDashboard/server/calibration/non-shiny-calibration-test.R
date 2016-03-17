# This is a calibration test script that will run a non-shiny version of the cascade model.
# For calibration testing, in the absence of all the bells and whistles that come with the shiny app.
# This script will be a template for future shiny versions of the model.
# Ideally, will just be a case of drag and drop of functions.
# Create all functions external to this script.
rm(list=ls())
setwd("~/git/WhoCascade/CascadeApp/CascadeDashboard")
# dir()
graphics.off()
quartz.options(w = 10, h = 8)

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

# Create a parameter set
# Use: FME::Latinhyper = DONE.

# WHAT assumptions do we make about model parameters here?
# These are ALL based on the previous Kenya model - perhaps we should start with a more blank slate here?
# We want a simple calibration, that is also fast.

# BASELINE SIMULATION.
# BREAK IT DOWN. ONLY LOOK AT ONE PARAMETER AT A TIME.
# Start with RHO and then expand.
# HOW WILL I WEIGHT THIS?

# -------------------------------------------------------------------------------- #
# Allowing natural mortality to be adjusted, allows us to better fit to PLHIV data #
# -------------------------------------------------------------------------------- #

# Run Calibration Model
RunCalibration(100)

# 1000 simulations and picking the top 100 takes 48 seconds.

# The outcome of the calibration is two-fold:
    # 1) Return a range of values (min, max) of "ghost values". = DONE.
    # 2) Return a barplot of the "CASCADE"


############
# THURSDAY #
############

# 1) Get calibration to return barplot of the cascade. = DONE.
#     - Mean of the runs as the bar with shaded area around it? (just some error_bars)

RunCalibration(100)

# 2) Are we getting reasonable parameter values?

RunCalibration(1000)

# 3) Are we ready to share?
# 4) How easily can we do the 'whole thing' here?
# 5) Expand to include "PLHIV Virally Suppressed".


# LONG TERM GOAL (BEFORE FLORIDA HOLIDAY),
# Have the 'whole thing' done for KENYA.

# THERE AFTER, expand.
# end of this week, produce calibrated version of Kenya.




# ----------- #
# CALIBRATION #
# ----------- #

# LHS or optim(). Going with LHS for now. Can always pivot back.
# Plot test outputs.
# dynamic labelling of input data (what the hell do I mean here?)
# What is the best strategy? LHS and brute force.

# IDEA:
    # - Pull all 'error' values together.
    # - Multiply by some weight, depending on what they are
    # - Ensure that we are still able to minimise values to ZERO (well, I can dream)

# --------- #
# WEIGHTING #
# --------- #

# How do we weight studies?????
