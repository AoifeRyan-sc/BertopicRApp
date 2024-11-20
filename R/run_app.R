library(shiny)
library(BertopicR)

#' Run BertopicR App
#'
#' @return output app
#' @export
#'
run_app <- function(){
  # this is a slow download if left to the middle of the app 
  # so best to just do it at the start
  embedder <- BertopicR::bt_make_embedder_st("all-miniLM-L6-v2")
  
  appDir <- system.file("app", package = "BertopicRApp")
  message("app directory: ", appDir)
  shiny::runApp(appDir, display.mode = "normal")
  # shiny::shinyApp(ui, server)
}