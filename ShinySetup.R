# WhoCascade ShinySetup.R #
# Jack Olney - 30/09/2015 #

# Get devtools from Hadley
# install.packages("devtools")
require(devtools)

# Get shinyapps from rstudio repo
# install_github('rstudio/shinyapps')
require(shinyapps)

# Authorize account (not so fast!)
# Details on shinyapps.io

# To deploy an app
library(shinyapps)
deployApp('path/to/your/app')

# Get Themes
install.packages("shinythemes")
require(shinythemes)

# ------------------------------- #

# Get shiny
# install.packages('shiny')
require(shiny)

setwd("./TestApp")

runApp()

# ------------------------------ #
require(shinyapps)

# deployApp("./ShinyCascade")

# deployApp("./ShinyCascadeDashboard")

deployApp("CascadeDashboard")

