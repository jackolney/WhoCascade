source("server/alert.R", local = TRUE)
source("server/datatable.R", local = TRUE)
source("server/google-sheets.R", local = TRUE)
source("server/observeevent.R", local = TRUE)
source("server/parameters.R", local = TRUE)
source("server/plot.R", local = TRUE)
source("server/reactive.R", local = TRUE)
source("server/render.R", local = TRUE)
source("server/table.R", local = TRUE)
source("server/valuebox.R", local = TRUE)

## It appears as though no server code exists here.
## The reality is that it is all farmed out to other places.
## This code is tidy.

# Need to be removed EVENTUALLY
source("TheModel.R")

# Uncomment to hide sideBar at start (still flashes up though).
# shinyjs::addClass(selector = "body", class = "sidebar-collapse")
