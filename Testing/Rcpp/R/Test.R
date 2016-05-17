# Test R Script
rm(list=ls())

dyn.load("src/Cascade.so")

Run <- function(x = 1) {
    for (i in 1:x) {
        .Call("Cascade", 10)
    }
}

system.time(Run(100))
