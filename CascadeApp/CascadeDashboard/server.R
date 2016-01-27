# Check this in a 'serverhead.R' and source eventually
source("server/serverhead.R", local = TRUE)
shinyServer(function(input, output, session) source("server/app.R", local = TRUE))
