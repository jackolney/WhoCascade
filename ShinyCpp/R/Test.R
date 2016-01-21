# Test R Script
rm(list=ls())

dyn.load("Cascade.so")

Run <- function() {
    .Call("Cascade")  
}

Run()