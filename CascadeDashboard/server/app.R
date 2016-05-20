source("server/alert.R",          local = TRUE)
source("server/google-sheets.R",  local = TRUE)
source("server/leaflet.R",        local = TRUE)
source("server/links.R",          local = TRUE)
source("server/render.R",         local = TRUE)
source("server/report-fig.R",     local = TRUE)
source("server/report.R",         local = TRUE)
source("server/valuebox.R",       local = TRUE)
source("ui/global-lists.R",       local = TRUE)

## It appears as though no server code exists here.
## The reality is that it is all farmed out to other places.
## This code is tidy.

# Uncomment to hide sideBar at start (still flashes up though).
# shinyjs::addClass(selector = "body", class = "sidebar-collapse")

# ----- #
# MODEL #
# ----- #

source("server/model/beta.R",       local = TRUE)
source("server/model/initial.R",    local = TRUE)
source("server/model/mean-model.R", local = TRUE)
source("server/model/model.R",      local = TRUE)
source("server/model/parameters.R", local = TRUE)

# ---------- #
# CALBRATION #
# ---------- #

source("server/calibration/assumptions.R",      local = TRUE)
source("server/calibration/calibration-data.R", local = TRUE)
source("server/calibration/calibration.R",      local = TRUE)
source("server/calibration/check-csv.R",        local = TRUE)
source("server/calibration/data-table.R",       local = TRUE)
source("server/calibration/error.R",            local = TRUE)
source("server/calibration/initial.R",          local = TRUE)
source("server/calibration/marrakech-data.R",   local = TRUE)
source("server/calibration/master.R",           local = TRUE)
source("server/calibration/misc-functions.R",   local = TRUE)
source("server/calibration/mission-control.R",  local = TRUE)
source("server/calibration/model.R",            local = TRUE)
source("server/calibration/observe-input.R",    local = TRUE)
source("server/calibration/observe-output.R",   local = TRUE)
source("server/calibration/plot-functions.R",   local = TRUE)
source("server/calibration/plot.R",             local = TRUE)
source("server/calibration/progress.R",         local = TRUE)
source("server/calibration/result.R",           local = TRUE)
source("server/calibration/update-input.R",     local = TRUE)

# ---------- #
# PROJECTION #
# ---------- #

source("server/projection/CD4-distribution.R", local = TRUE)
source("server/projection/data-table.R",       local = TRUE)
source("server/projection/extract-data.R",     local = TRUE)
source("server/projection/observe-event.R",    local = TRUE)
source("server/projection/plot-functions.R",   local = TRUE)
source("server/projection/plot.R",             local = TRUE)
source("server/projection/reactive.R",         local = TRUE)

# ------------ #
# OPTIMISATION #
# ------------ #

source("server/optimisation/functions.R",      local = TRUE)
source("server/optimisation/observe-input.R",  local = TRUE)
source("server/optimisation/observe.R",        local = TRUE)
source("server/optimisation/optimise.R",       local = TRUE)
source("server/optimisation/parameters.R",     local = TRUE)
source("server/optimisation/plot-functions.R", local = TRUE)
source("server/optimisation/plot.R",           local = TRUE)
source("server/optimisation/sim.R",            local = TRUE)
source("server/optimisation/table.R",          local = TRUE)