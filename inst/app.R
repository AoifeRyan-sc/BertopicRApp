# this is for local use of an application - not for deployment. 
# when deploying with the multiple file set up (ui.R, server.R)
# you need to ensure your scripts etc. are all sourced - good place for this
# is global.R which is automatically sourced.

library(shiny)
library(BertopicR)
library(here)
library(magrittr)

# Temporary to allow for live reloads in an RStudio background job or app running in a terminal:
options(shiny.autoreload=TRUE)
options(shiny.port = 7775)
options(shiny.host = "127.0.0.1")

# Source Business logic/helper functions
source(here("inst/R/helper_functions.R"))
source(here("inst/R/reducingAsync.R"))

source(here("inst/modules/upload.R"))
source(here("inst/modules/embed_reduce.R"))
source(here("inst/modules/reduce.R"))
source(here("inst/modules/modelExplore.R"))
source(here("inst/modules/clustering.R"))
source(here("inst/modules/outlier.R"))

# Source App Files
source(here("inst/ui.R"))
source(here("inst/server.R"))

# Run an app the IDE way if in an interactive session, else run it the terminal/deployment way:
if(interactive()){
  shinyApp(ui, server)
} else {
  shiny::runApp(here::here("app"))
  }
