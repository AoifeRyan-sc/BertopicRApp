#' ui specs for upload tab
#'
#' @param id parameter for shiny identification
#'
#' @noRd
#'
# uploadUi <- function(id) {
# 
#   ns <- NS(id)
# 
#   shiny::sidebarLayout(
#     shiny::sidebarPanel(
#       shiny::fileInput(
#         ns("data_upload"), "Upload your data",
#         accept = c(".xlsx", ".csv", ".tsv", ".rds"), multiple = FALSE),
#       shiny::uiOutput(ns("data_upload_error_message")),
#       div(
#         style = "position: relative;",
#         shiny::textInput(ns("text_col"), "Text Column:", placeholder = "message"),
#         div(
#           id = ns("text_col_icon"),
#           style = "position: absolute; top: 0; right: 5px; transform: translateX(-20%);",
#           # bsicons::bs_icon("question-circle-fill")
#           tags$i(
#             id = ns("text_col_icon"),
#             class = "glyphicon glyphicon-question-sign",
#             style = "position: absolute; top: 10px; right: 5px; transform: translateX(-20%);"
#           )
#         )
#       ),
#       shinyBS::bsTooltip(id = ns("text_col_icon"), title = "This is to pass the text column on which you wish to topic model. It will be displays as 'docs' in all table outputs throughout the app.", placement = "top"),
#       shiny::actionButton(ns("submit_col_name"), "Submit")
#     ),
#     shiny::mainPanel(
#       DT::dataTableOutput((ns("uploaded_data"))),
#     )
#   )
# 
# }
# #
uploadUi <- function(id) {
  ns <- NS(id)

  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::fileInput(
        ns("data_upload"), "Upload your data",
        accept = c(".xlsx", ".csv", ".tsv", ".rds"), multiple = FALSE),
      shiny::uiOutput(ns("data_upload_error_message")),
      div(
        style = "position: relative;",
        shiny::textInput(ns("text_col"), "Text Column:", placeholder = "message"),
        div(
          id = ns("text_col_icon"),
          style = "position: absolute; top: 0; right: 5px; transform: translateX(-20%);",
          bslib::tooltip(
            bsicons::bs_icon("question-circle-fill"),
            "This is to pass the text column on which you wish to topic model. It will be displaed as 'docs' in all table outputs throughout the app."
          )
        )
      ),
      shiny::actionButton(ns("submit_col_name"), "Submit")
    ),
    shiny::mainPanel(
      DT::dataTableOutput((ns("uploaded_data")))
    )
  )
}
uploadServer <- function(id, r){
  shiny::moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # r$df <- shiny::reactive({
    shiny::observe({
      shiny::req(input$data_upload, input$submit_col_name)
      
      ext <- tools::file_ext(input$data_upload$name)
      r$df <- switch(ext,
                     csv = readr::read_csv(input$data_upload$datapath),
                     tsv = vroom::vroom(input$data_upload$datapath, delim = "\t"),
                     xlsx = readxl::read_xlsx(input$data_upload$datapath),
                     rds = readRDS(input$data_upload$datapath),
      ) %>%
        dplyr::mutate(rowid = dplyr::row_number(), .before = 1)  %>%
        dplyr::rename(docs = input$text_col)
    })
    
    # output$data_upload_error_message <- shiny::renderUI({
    #   req(input$data_upload)
    # 
    #   required_cols <- c("docs")
    #   # , "embeddings", "v1", "v2")  # Add your required column names here
    #   missing_cols <- setdiff(required_cols, colnames(r$df))
    # 
    #   if (length(missing_cols) > 0) {
    #     shiny::tagList(
    #       shiny::p(shiny::HTML(paste("<b>Error:</b> Required columns missing from data:", paste(missing_cols, collapse = ", "))))
    #     )
    #   }
    #   # else if (!c("v1", "v2") %in% colnames(r$df)){
    #   #   shiny::tagList(
    #   #     shiny::p(shiny::HTML(paste("<b>Error:</b> Required columns missing from data:", paste(missing_cols, collapse = ", "))))
    #   #   )
    #   # } # I think maybe we don't need this
    # })
    
    # shiny::observeEvent(input$data_upload, {
    #   shinyjs::reset("reduced_embeddings_upload")
    #   r$reduced_embeddings <- NULL
    # }) # not sure what this is
    
    output$uploaded_data <- DT::renderDataTable({
      req(is.data.frame(r$df))
      r$df 
    })
    
  
    
  })
}