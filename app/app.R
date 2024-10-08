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
source(here("app/R/helper_functions.R"))
source(here("app/R/reducingAsync.R"))

# source(here("app/www/styles.css"))
source(here("app/modules/upload.R"))
source(here("app/modules/embed_reduce.R"))
source(here("app/modules/reduce.R"))
source(here("app/modules/modelExplore.R"))
source(here("app/modules/clustering.R"))
source(here("app/modules/outlier.R"))

# Source App Files
source(here("app/ui.R"))
source(here("app/server.R"))

# Run an app the IDE way if in an interactive session, else run it the terminal/deployment way:
if(interactive()){
  shinyApp(ui, server)
} else {
  shiny::runApp(here::here("app"))
}
