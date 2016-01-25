# Control Rcpp script -- 25/01/16
setwd("~/git/WhoCascade/dev/Rcpp")

dir()

# Rcpp
require(Rcpp)

ls()
sourceCpp("./src/main.cpp")

Run <- function(x = 1) {
    for(i in 1:x) {
        Cascade()
    }
}

system.time(Run())

system.time(Run(100))
