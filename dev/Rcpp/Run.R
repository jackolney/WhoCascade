# Control Rcpp script -- 25/01/16

setwd("~/git/WhoCascade/dev/Rcpp")

dir()

# Rcpp
require(Rcpp)

ls()
sourceCpp("Euler.cpp")
ls()

f1(x)

mean(x)
meanC(x)

mod <- lm(mpg ~ wt, data = mtcars)
mpe(mod)

Euler()