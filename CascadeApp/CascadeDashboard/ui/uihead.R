# devtools::install_github("jackolney/shinydashboard", ref = "master")
# devtools::load_all(pkg = "~/git/packages/shinydashboard")

if (version$os == "darwin13.4.0") {
    loc <- "/Library/Frameworks/R.framework/Versions/3.3/Resources/library"
} else if (version$os == "linux-gnu") {
    loc <- "/home/DIDE/jjo11/R/x86_64-pc-linux-gnu-library/3.2/"
}

library(DT,             lib.loc = loc)
library(ggplot2,        lib.loc = loc)
library(leaflet,        lib.loc = loc)
library(rgdal,          lib.loc = loc)
library(shiny,          lib.loc = loc)
library(shinyBS,        lib.loc = loc)
library(shinydashboard, lib.loc = loc)
library(shinyjs,        lib.loc = loc)
library(shinyTable,     lib.loc = loc)
library(shinythemes,    lib.loc = loc)
library(V8,             lib.loc = loc)

# source global-lists
source("ui/global-lists.R", local = TRUE)
