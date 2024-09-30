library(shiny)
library(BertopicR)

#' Run BertopicR App
#'
#' @return output app
#' @export
#'
run_app <- function(){
  appDir <- system.file("shinyapp", package = "BertopicRApp")
  shiny::runApp(appDir, display.mode = "normal")
  # shiny::shinyApp(ui, server)
}