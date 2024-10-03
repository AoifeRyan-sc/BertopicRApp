# this is for local use of an application - not for deployment. 
# when deploying with the multiple file set up (ui.R, server.R)
# you need to ensure your scripts etc. are all sourced - good place for this
# is global.R which is automatically sourced.

library(shiny)
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
source(here("app/modules/reduce.R"))
source(here("app/modules/modelling.R"))
source(here("app/modules/modelExplore.R"))
source(here("app/modules/clustering.R"))
source(here("app/modules/outlier.R"))

# source(here("app/modules/clustering_elements.R"))

source(here("app/modules/clustering_panel_modules.R"))

# Source App Files
source(here("app/ui.R"))
source(here("app/server.R"))

# Run an app the IDE way if in an interactive session, else run it the terminal/deployment way:
if(interactive()){
  shinyApp(ui, server)
} else {
  shiny::runApp(here::here("app"))
}

# 
# ui <- fluidPage(
#   theme = shinythemes::shinytheme("readable"),
#   
#   shiny::tagList(
#     shinyjs::useShinyjs(),
#     shiny::navbarPage(
#       "BertopicR",
#       id = "main_navpage",
#       # shiny::tabPanel("Reduce Embeddings",
#       #          shiny::titlePanel("Reduce Embeddings"),
#       #          reducingUi("reducing_panel")
#       # ),
#       shiny::tabPanel("Clustering",
#                shiny::titlePanel("Clustering"),
#                clusteringUi("clustering_panel")
#       ),
#       
#       shiny::tabPanel("Explore the Model",
#                modelExploreUi("explore_model_panel")
#       ),
#       
#       shiny::tabPanel("Outlier Manipulation",
#                id = "outlier_manipulation",
#                shiny::titlePanel("Outlier Manipulation"),
#                
#                outlierUi("outlier_panel")
#       )
#     ) 
#   )
# ) 
# 
# 
# server <- function(input, output, session) {
#   options(shiny.maxRequestSize=100*1024^2)
#   
#   reducingServer("reducing_panel", df = df)
#   
#   clustering_output <- clusteringServer("clustering_panel")
#   clusters <- clustering_output$clusters
#   model <- clustering_output$model
#   cluster_model <- clustering_output$cluster_model
#   df <- clustering_output$df
#   
#   shiny::observe({
#     if(!is.null(model()) && cluster_model() == "K-Means"){
#       shiny::hideTab(inputId = "main_navpage", target = "Outlier Manipulation")
#     }
#   })
#   
#   shiny::observe({
#     if(is.null(model())){
#       shiny::showTab(inputId = "main_navpage", target = "Outlier Manipulation")
#     }
#   })
#   
#   modelExploreServer("explore_model_panel", model = model, df= df)
#   outlierServer("outlier_panel", df = df, model = model, clusters = clusters, embedder = embedder)
# }
# 
# shiny::shinyApp(ui, server)