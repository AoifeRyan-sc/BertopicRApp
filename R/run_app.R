library(shiny)
library(BertopicR)

#' Run BertopicR App
#'
#' @return output app
#' @export
#'
run_app <- function(){
  shiny::shinyApp(ui, server)
}