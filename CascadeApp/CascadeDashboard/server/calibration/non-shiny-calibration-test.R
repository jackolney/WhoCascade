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

p <- parameters()
y <- initial(p)
i <- incidence()

# Parameter update
# beta <- GetBeta(y, p)
# p[62] <- beta # p[["beta"]]

# Put it all in a list
plist <- list(p, i)
names(plist) <- c("r_par", "r_inc")

# The Model #
result <- deSolve::ode(times = time, y = y, func = "calib_derivs", parms = plist, initfunc = "calib_initmod", dllname = "cascade")



KenyaData


# Feed values for "kenya" into calibration version of the model





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
