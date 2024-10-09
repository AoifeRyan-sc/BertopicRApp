embedReduceUi <- function(id){
  
  ns <- NS(id)
  
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      htmltools::h4("Step 1: Embed Documents"),
      shiny::actionButton(ns("embed_button"), label = shiny::HTML("<strong>Embed</strong>"), class = "btn-succes", 
                          width = "100%", style = "margin-bottom: 30px; border-width: 2px;"),
      shiny::uiOutput(ns("embed_status")),
      htmltools::h4("Step 2: Reduce Embedding Dimension"),
      shiny::selectInput(ns("reducing_method"), "Reducing Method", choices = c("UMAP", "PCA")),
      # "PCA")),
      shiny::conditionalPanel(
        condition = "input.reducing_method == 'UMAP'", ns = ns,
        shiny::div(
          class = "row",
          shiny::div(
            class = "col-md-6",
            shiny::numericInput(ns("n_neighbours"), "No. of Nearest Neighbours", value = 15)
          ),
          shiny::div(
            class= "col-md-6",
            shiny::numericInput(ns("n_components"), "No. of Dimensions", value = 5)
          ),
          style = "margin-top: 30px"
        ),
        shiny::div(
          class = "row",
          shiny::div(
            class = "col-md-7",
            shiny::numericInput(ns("min_dist"), "Min Distance Between Points", value = 0)
          ),
          shiny::div(
            class= "col-md-5",
            shiny::selectInput(ns("reducing_metric"), "Distance Metric", choices = c("cosine", "euclidean")) # expand this
          )
        ),
        shiny::uiOutput(ns("print_status")),
        # shiny::verbatimTextOutput(ns("print_status")),
        shiny::actionButton(ns("do_reducing"), label = shiny::HTML("<strong>Reduce</strong>"), class = "btn-succes", 
                            width = "100%", style = "margin-bottom: 30px; border-width: 2px;")
      ),
      shiny::conditionalPanel(
        condition = "input.reducing_method == 'PCA'", ns = ns,
        "This functionality is not yet implemented in the app."
      )
      # embedUi(ns("embed_components")),
      # reduceUi(ns("reduce_components"))
    ),
    shiny::mainPanel(
      shiny::fluidRow(
        bslib::card(
          shiny::uiOutput(ns("embed_status")),
          shiny::uiOutput(ns("embed_progress_update"))
        ),
        bslib::card(
          shiny::uiOutput(ns("reduce_status"))
        ),
        bslib::card(
          shiny::uiOutput(ns("reduce2d_status"))
        )
      )
    )
  )
  
}

embedReduceServer <- function(id, r){
  shiny::moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    shiny::observeEvent(input$embed_button, {
      r$embedding_happening <- "happening"
      r$embedder <- BertopicR::bt_make_embedder_st("all-MiniLM-L6-v2") # want to be able to reference this again but can't pass between sessions
      r$embedding_job <- callr::r_bg(function(docs, embedding_model){
        embedder <- BertopicR::bt_make_embedder_st("all-MiniLM-L6-v2")
        embeddings <- BertopicR::bt_do_embedding(embedder, docs, accelerator = "mps") # what happens if user doesn't have mps?
      },
      args = list(docs = r$df$docs, embedding_model = "all-MiniLM-L6-v2"),
      supervise = TRUE)
      
      # r$embedding_job <- callr::r_bg(function(docs, embedder){
      #   # embedder <- BertopicR::bt_make_embedder_st("all-MiniLM-L6-v2")
      #   embeddings <- BertopicR::bt_do_embedding(embedder, docs, accelerator = "mps") # what happens if user doesn't have mps?
      # },
      # args = list(docs = r$df$docs, embedder = r$embedder),
      # supervise = TRUE)

    }) 
    
    shiny::observe({
      shiny::req(r$embedding_happening == "happening")
      shiny::invalidateLater(250)

      if (r$embedding_job$is_alive() == FALSE){
        r$embedding_happening = "finished"
        r$embeddings <- r$embedding_job$get_result()
      }
    })
    
    output$embed_status <- shiny::renderUI({
      progress_annimation(r$embedding_happening, r$embedding_happening, "Reduced", "Reducing in progress")
      # if (r$embedding_happening == "happening"){
      #   htmltools::tagList(
      #     htmltools::tags$head(
      #       tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
      #     ),
      #     htmltools::div(
      #       class = "reducing-embeddings",
      #       span(class = "timer-emoji", "⏳"),
      #       span(class = "reducing-text", "Calculating Embeddings")
      #     ))
      # } else if (r$embedding_happening == "finished") {
      #   htmltools::tagList(
      #     htmltools::tags$head(
      #       tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
      #     ),
      #     htmltools::div(
      #       class = "reduced-embeddings",
      #       span(class = "check-emoji", "✅"),
      #       span(class = "reducing-text", "Text Embedded!")
      #     ))
      # }
    })
    
    # output$embed_progress_update <- shiny::renderUI({
    #   req(r$embedding_happening == "happening")
    #   invalidateLater(250)
    #   
    #   embed_job_output <- r$embedding_job$read_error()
    #   embed_job_output <- gsub("\r", "\n", embed_job_output)
    #   shiny::htmlOutput(embed_job_output)
    # })
    
    shiny::observe({
      buttons <- c("reducing_method", "n_neighbours", "n_components", "min_dist", "reducing_metric", "do_reducing")
      lapply(buttons, shinyjs::disable)

      if (is.array(r$embeddings) | is.data.frame(r$embeddings)){
        lapply(buttons, shinyjs::enable)
      }
      
    })
    
    # reduced_embeddings <- backgroundReduce(
    #   id = "reduced_embeddings",
    #   n_neighbours = input$n_neighbours,
    #   n_components = input$n_components,
    #   min_dist = input$min_dist,
    #   metric = input$reducing_metric,
    #   embeddings = r$df$embeddings,
    #   wait_for_event = TRUE
    # ) # I think I want to change this to be a normal function
    
    shiny::observeEvent(input$do_reducing, {
      r$reducing_happening <- "happening"
      r$reducing_job <- callr::r_bg(function(n_neighbours, n_components, min_dist, metric, embeddings){
        reducer <- BertopicR::bt_make_reducer_umap(n_neighbours = n_neighbours, 
                                                   n_components = n_components, 
                                                   min_dist = min_dist, 
                                                   metric = metric)
        reduced_embeddings <- BertopicR::bt_do_reducing(reducer, embeddings)
      },
      args = list(n_neighbours = input$n_neighbours, n_components = input$n_components, min_dist = input$min_dist, metric = input$reducing_metric, embeddings = r$df$embeddings),
      supervise = TRUE)
    }) 
    
    reduced_embeddings2d <- backgroundReduce(
      id = "reduced_embeddings2d",
      n_neighbours = input$n_neighbours,
      n_components = 2,
      min_dist = input$min_dist,
      metric = input$reducing_metric,
      embeddings = r$df$embeddings,
      wait_for_event = TRUE
    ) # I think I want to change this to be a normal function
    
    shiny::observeEvent(input$do_reducing, {
      # reduced_embeddings$start_job()
      reduced_embeddings2d$start_job()
    })
    
    # shiny::observe({
    #   req(!is.null(reduced_embeddings))
    #   r$reduced_embeddings <- reduced_embeddings$get_result()
    #   # r$reduced_embeddings <- reduced_embeds
    # })
    
    shiny::observe({
      shiny::req(r$reducing_happening == "happening")
      shiny::invalidateLater(250)
      
      if (r$reducing_job$is_alive() == FALSE){
        r$reducing_happening = "finished"
        r$reduced_embeddings <- r$reducing_job$get_result()
      }
    })
    
    shiny::observe({
      # r$reduced_embeddings2d <- df[c("v1", "v2")]
      req(!is.null(reduced_embeddings2d))
      r$reduced_embeddings2d<- reduced_embeddings2d$get_result()
    })
    
    output$reduce_status <- shiny::renderUI({
      progress_annimation(r$reducing_happening, r$reduced_embeddings, "Reduced", "Reducing in progress")
      
      # if (r$reducing_happening == "happening"){
      #   htmltools::tagList(
      #     htmltools::tags$head(
      #       tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
      #     ),
      #     htmltools::div(
      #       class = "reducing-embeddings",
      #       span(class = "timer-emoji", "⏳"),
      #       span(class = "reducing-text", "Reducing Embeddings")
      #     ))
      # } else if (r$reducing_happening == "finished") {
      #   htmltools::tagList(
      #     htmltools::tags$head(
      #       tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
      #     ),
      #     htmltools::div(
      #       class = "reduced-embeddings",
      #       span(class = "check-emoji", "✅"),
      #       span(class = "reducing-text", "Embeddings Reduced!")
      #     ))
      # }
      
      # if (is.array(r$reduced_embeddings) | is.data.frame(r$reduced_embeddings)){
      #   htmltools::tagList(
      #     htmltools::tags$head(
      #       tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
      #     ),
      #     htmltools::div(
      #       class = "reduced-embeddings",
      #       span(class = "check-emoji", "✅"),
      #       span(class = "reducing-text", "Reduced!")
      #     ))
      # } else if (stringr::str_detect(reduced_embeddings$progress_message(),"Reducing in progress")){
      #   htmltools::tagList(
      #     htmltools::tags$head(
      #       tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
      #     ),
      #     htmltools::div(
      #       class = "reducing-embeddings",
      #       span(class = "timer-emoji", "⏳"),
      #       span(class = "reducing-text", "Reducing Embeddings")
      #     ))
      # } 
      # 
    })
    
    output$reduce2d_status <- shiny::renderUI({

      if (is.array(r$reduced_embeddings2d) | is.data.frame(r$reduced_embeddings2d)){
        htmltools::tagList(
          htmltools::tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
          ),
          htmltools::div(
            class = "reduced-embeddings",
            span(class = "check-emoji", "✅"),
            span(class = "reducing-text", "Reduced to 2D!")
          ))
      } else if (stringr::str_detect(reduced_embeddings2d$progress_message(),"Reducing in progress")){
        htmltools::tagList(
          htmltools::tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
          ),
          htmltools::div(
            class = "reducing-embeddings",
            span(class = "timer-emoji", "⏳"),
            span(class = "reducing-text", "Reducing Embeddings to 2D")
          ))
      } else {}
      
    })
  })
}


progress_annimation <- function(processing_value_check, complete_value_check, processing_text, complete_text){
  if (complete_value_check == "happening"){
    htmltools::tagList(
      htmltools::tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
      ),
      htmltools::div(
        class = "reducing-embeddings",
        span(class = "timer-emoji", "⏳"),
        span(class = "reducing-text", processing_text)
      ))
  } else if (processing_value_check == "finished") {
    htmltools::tagList(
      htmltools::tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
      ),
      htmltools::div(
        class = "reduced-embeddings",
        span(class = "check-emoji", "✅"),
        span(class = "reducing-text", complete_text)
      ))
  }
}