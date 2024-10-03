#' ui specs for upload tab
#'
#' @param id parameter for shiny identification
#'
#' @noRd
#'
uploadUi <- function(id) {
  
  ns <- NS(id)
  
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::fileInput(
        ns("data_upload"), "Upload your data",
        accept = c(".xlsx", ".csv", ".tsv", ".rds"), multiple = FALSE),
      shiny::uiOutput(ns("data_upload_error_message")),
      shiny::actionButton(ns("embed_button"), label = shiny::HTML("<strong>Embed</strong>"), class = "btn-succes", 
                          width = "100%", style = "margin-bottom: 30px; border-width: 2px;"),
      shiny::uiOutput(ns("embed_status"))
    ),
    shiny::mainPanel(
      DT::dataTableOutput((ns("uploaded_data"))),
      shiny::downloadButton(ns("data_download_clustering"), label = "Download Data Table")
    )
  )

}

uploadServer <- function(id, r){
  shiny::moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # r$df <- shiny::reactive({
    shiny::observe({
      shiny::req(input$data_upload)
      
      ext <- tools::file_ext(input$data_upload$name)
      r$df <- switch(ext,
                     csv = readr::read_csv(input$data_upload$datapath),
                     tsv = vroom::vroom(input$data_upload$datapath, delim = "\t"),
                     xlsx = readxl::read_xlsx(input$data_upload$datapath),
                     rds = readRDS(input$data_upload$datapath),
      ) %>%
        dplyr::mutate(rowid = dplyr::row_number(), .before = 1)
      
    })
    
    output$data_upload_error_message <- shiny::renderUI({
      req(input$data_upload)
      
      required_cols <- c("docs")
      # , "embeddings", "v1", "v2")  # Add your required column names here
      missing_cols <- setdiff(required_cols, colnames(r$df))
      
      if (length(missing_cols) > 0) {
        shiny::tagList(
          shiny::p(shiny::HTML(paste("<b>Error:</b> Required columns missing from data:", paste(missing_cols, collapse = ", "))))
        )
      } 
      # else if (!c("v1", "v2") %in% colnames(r$df)){
      #   shiny::tagList(
      #     shiny::p(shiny::HTML(paste("<b>Error:</b> Required columns missing from data:", paste(missing_cols, collapse = ", "))))
      #   )
      # } # I think maybe we don't need this
    })
    
    # shiny::observeEvent(input$data_upload, {
    #   shinyjs::reset("reduced_embeddings_upload")
    #   r$reduced_embeddings <- NULL
    # }) # not sure what this is
    
    output$uploaded_data <- DT::renderDataTable({
      req(is.data.frame(r$df))
      printdf <- r$df %>% dplyr::select(-embeddings)
      printdf
    })
    
    shiny::observeEvent(input$embed_button, {
      r$embedding_happening <- "happening"
      r$embedding_job <- callr::r_bg(function(docs, embedding_model){
        embedder <- BertopicR::bt_make_embedder_st("all-MiniLM-L6-v2")
        embeddings <- BertopicR::bt_do_embedding(embedder, docs, accelerator = "mps") # what happens if user doesn't have mps?
      },
      args = list(docs = df$docs, embedding_model = "all-MiniLM-L6-v2"),
      supervise = TRUE)
    }) 
    
    shiny::observe({
      shiny::req(r$embedding_happening == "happening")
      shiny::invalidateLater(250)
      print(r$embedding_job$is_alive() == FALSE)

      if (r$embedding_job$is_alive() == FALSE){
        r$embedding_happening = "finished"
        r$embeddings <- r$embedding_job$get_result()
      }
    })
    
    
    output$embed_status <- shiny::renderUI({
      if (r$embedding_happening == "happening"){
        htmltools::tagList(
          htmltools::tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
          ),
          htmltools::div(
            class = "reducing-embeddings",
            span(class = "timer-emoji", "⏳"),
            span(class = "reducing-text", "Calculating Embeddings")
          ))
      } else if (r$embedding_happening == "finished") {
        htmltools::tagList(
          htmltools::tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
          ),
          htmltools::div(
            class = "reduced-embeddings",
            span(class = "check-emoji", "✅"),
            span(class = "reducing-text", "Text Embedded!")
          ))
      } else{
        # htmltools::div()
      }
    })
    
  })
}