clustering_upload_ui <- function(id){
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tabPanel(
      "Upload Data",
      shiny::br(),
      shiny::fileInput(
        ns("data_upload"), "Upload your data", 
        accept = c(".xlsx", ".csv", ".tsv", ".rds"), multiple = FALSE),
      shiny::uiOutput(ns("data_upload_error_message")),
      shiny::verbatimTextOutput(ns("reduced_embeddings_sample")),
      value = "upload"
    )
  )
}