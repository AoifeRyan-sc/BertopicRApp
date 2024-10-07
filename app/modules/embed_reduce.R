# embedUi <- function(id){
#   ns <- NS(id)
#   shiny::tagList(
#     htmltools::h4("Step 1: Embed Documents"),
#     shiny::actionButton(ns("embed_button"), label = shiny::HTML("<strong>Embed</strong>"), class = "btn-succes", 
#                         width = "100%", style = "margin-bottom: 30px; border-width: 2px;"),
#     shiny::uiOutput(ns("embed_status"))
#   )
# }


reduceUi <- function(id){ 
  ns <- NS(id)
  
  shiny::tagList(
    htmltools::h4("Step 2: Reduce Embedding Dimension"),
    shiny::selectInput(ns("reducing_method"), "Reducing Method", choices = c("UMAP", "PCA")),
    # "PCA")),
    shiny::conditionalPanel(
      condition = "input.reducing_method == 'UMAP'", ns = ns,
      shiny::div(
        class = "row",
        shiny::div(
          class = "col-md-6",
          shiny::numericInput(ns("n_neighbours1"), "No. of Nearest Neighbours", value = 15)
        ),
        shiny::div(
          class= "col-md-6",
          shiny::numericInput(ns("n_components1"), "No. of Dimensions", value = 5)
        ),
        style = "margin-top: 30px"
      ),
      shiny::div(
        class = "row",
        shiny::div(
          class = "col-md-7",
          shiny::numericInput(ns("min_dist1"), "Min Distance Between Points", value = 0)
        ),
        shiny::div(
          class= "col-md-5",
          shiny::selectInput(ns("reducing_metric1"), "Distance Metric", choices = c("cosine", "euclidean")) # expand this
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
  )
}

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
            shiny::numericInput(ns("n_neighbours1"), "No. of Nearest Neighbours", value = 15)
          ),
          shiny::div(
            class= "col-md-6",
            shiny::numericInput(ns("n_components1"), "No. of Dimensions", value = 5)
          ),
          style = "margin-top: 30px"
        ),
        shiny::div(
          class = "row",
          shiny::div(
            class = "col-md-7",
            shiny::numericInput(ns("min_dist1"), "Min Distance Between Points", value = 0)
          ),
          shiny::div(
            class= "col-md-5",
            shiny::selectInput(ns("reducing_metric1"), "Distance Metric", choices = c("cosine", "euclidean")) # expand this
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
          shiny::uiOutput(ns("embed_status"))
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
      r$embedding_job <- callr::r_bg(function(docs, embedding_model){
        embedder <- BertopicR::bt_make_embedder_st("all-MiniLM-L6-v2")
        embeddings <- BertopicR::bt_do_embedding(embedder, docs, accelerator = "mps") # what happens if user doesn't have mps?
      },
      args = list(docs = r$df$docs, embedding_model = "all-MiniLM-L6-v2"),
      supervise = TRUE)

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
    
    shiny::observe({
      print(class(r$embeddings))
      shinyjs::disable("reducing_method")
      shinyjs::disable("n_neighbours1")
      shinyjs::disable("n_components1")
      shinyjs::disable("min_dist1")
      shinyjs::disable("reducing_metric1")
      shinyjs::disable("do_reducing")
      if (is.array(r$embeddings) | is.data.frame(r$embeddings)){
        shinyjs::enable("reducing_method")
        shinyjs::enable("n_neighbours1")
        shinyjs::enable("n_components1")
        shinyjs::enable("min_dist1")
        shinyjs::enable("reducing_metric1")
        shinyjs::enable("do_reducing")
      }
      
    })
    
    reduced_embeddings <- backgroundReduce(
      id = "reduced_embeddings",
      n_neighbours = input$n_neighbours1,
      n_components = input$n_components1,
      min_dist = input$min_dist1,
      metric = input$reducing_metric1,
      embeddings = r$df$embeddings,
      wait_for_event = TRUE
    ) # I think I want to change this to be a normal function
    
    reduced_embeddings2d <- backgroundReduce(
      id = "reduced_embeddings2d",
      n_neighbours = input$n_neighbours1,
      n_components = 2,
      min_dist = input$min_dist1,
      metric = input$reducing_metric1,
      embeddings = r$df$embeddings,
      wait_for_event = TRUE
    ) # I think I want to change this to be a normal function
    
    shiny::observeEvent(input$do_reducing, {
      reduced_embeddings$start_job()
      reduced_embeddings2d$start_job()
    }) 
    
    shiny::observe({
      req(!is.null(reduced_embeddings))
      r$reduced_embeddings <- reduced_embeddings$get_result()
    })
    
    shiny::observe({
      req(!is.null(reduced_embeddings2d))
      r$reduced_embeddings2d<- reduced_embeddings2d$get_result()
    })
    
    output$reduce_status <- shiny::renderUI({
      
      if (is.array(r$reduced_embeddings) | is.data.frame(r$reduced_embeddings)){
        htmltools::tagList(
          htmltools::tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
          ),
          htmltools::div(
            class = "reduced-embeddings",
            span(class = "check-emoji", "✅"),
            span(class = "reducing-text", "Reduced!")
          ))
      } else if (stringr::str_detect(reduced_embeddings$progress_message(),"Reducing in progress")){
        htmltools::tagList(
          htmltools::tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
          ),
          htmltools::div(
            class = "reducing-embeddings",
            span(class = "timer-emoji", "⏳"),
            span(class = "reducing-text", "Reducing Embeddings")
          ))
      } else {}
      
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