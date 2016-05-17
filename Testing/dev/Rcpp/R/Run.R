# Control Rcpp script -- 25/01/16
setwd("~/git/WhoCascade/dev/Rcpp")
rm(list=ls())
dir()

# Rcpp
require(Rcpp)

sourceCpp("./src/main.cpp")

system.time(Cascade())

Cascade()

Run <- function(x = 1) {
    for (i in 1:x) {
        Cascade()
    }
}

system.time(Run())

system.time(Run(100))

# Output - Done.
# Input:
#   Conversion from NumericVector to array? with .names()?
