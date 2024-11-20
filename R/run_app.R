library(shiny)
library(BertopicR)

#' Run BertopicR App
#'
#' @return output app
#' @export
#'
run_app <- function(){
  appDir <- system.file("app", package = "BertopicRApp")
  shiny::runApp(appDir, display.mode = "normal")
  # shiny::shinyApp(ui, server)
}