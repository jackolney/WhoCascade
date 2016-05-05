# devtools::install_github("jackolney/WhoCascade/cascade", ref = "master")
# devtools::load_all(pkg = "~/git/WhoCascade/cascade")

if (version$os == "darwin13.4.0") {
    loc <- "/Library/Frameworks/R.framework/Versions/3.3/Resources/library"
} else if (version$os == "linux-gnu") {
    loc <- "/home/DIDE/jjo11/R/x86_64-pc-linux-gnu-library/3.2/"
}

library(cascade,      lib.loc = loc)
library(deSolve,      lib.loc = loc)
library(dplyr,        lib.loc = loc)
library(DT,           lib.loc = loc)
library(ggplot2,      lib.loc = loc)
library(googlesheets, lib.loc = loc)
library(grid,         lib.loc = loc)
library(gridExtra,    lib.loc = loc)
library(RColorBrewer, lib.loc = loc)
library(readr,        lib.loc = loc)
library(rmarkdown,    lib.loc = loc)
library(scales,       lib.loc = loc)
library(shiny,        lib.loc = loc)
library(shinyjs,      lib.loc = loc)
library(testthat,     lib.loc = loc)
library(V8,           lib.loc = loc)
