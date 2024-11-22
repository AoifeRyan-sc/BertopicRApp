embedReduceUi <- function(id){
  
  ns <- NS(id)
  
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      div(
        style = "position: relative;",
        htmltools::h4("Step 1: Embed Documents"),
        div(
          id = ns("embed_info_icon"),
          style = "position: absolute; top: 0; right: 5px; transform: translateX(-20%);",
          bslib::tooltip(
            bsicons::bs_icon("question-circle-fill"),
            htmltools::span("Uses the all-miniLM-L6-v2 embedding model to ", 
                            tags$a(href = "https://huggingface.co/blog/getting-started-with-embeddings", "embed text"),
                            "."
                            )
          )
        )
      ),
      # bslib::tooltip(
      #   htmltools::span(
      #     htmltools::h4("Step 1: Embed Documents"),
      #     bsicons::bs_icon("info-circle")
      #     ),
      #   htmltools::span("Uses the all-miniLM-L6-v2 embedding model to ", 
      #   tags$a(href = "https://huggingface.co/blog/getting-started-with-embeddings", "embed text"), 
      #   "."
      #   )
      # ),
      shiny::actionButton(ns("embed_button"), label = shiny::HTML("<strong>Embed</strong>"), class = "btn-succes", 
                          width = "100%", style = "margin-bottom: 30px; border-width: 2px;"),
      shiny::uiOutput(ns("embed_status")),
      htmltools::h4("Step 2: Reduce Embedding Dimension"),
      shiny::selectInput(ns("reducing_method"), "Reducing Method", choices = c("UMAP", "PCA")),
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
          shiny::uiOutput(ns("reduce_progress"))
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
    
    # embedding ----
    
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
      r$embedding_messages <- r$embedding_job$read_error()
      
      if (r$embedding_job$is_alive() == FALSE){
        r$embedding_happening = "finished"
        r$df$embeddings <- r$embedding_job$get_result()
      }
    })
    
    output$embed_status <- shiny::renderUI({
      embedding_progress_annimation <- progress_annimation(r$embedding_happening, r$embedding_happening, "Embedding in progress", "Embedded!")
      embedding_messages_div <- progress_extract(r$embedding_messages)
      
      shiny::div(embedding_progress_annimation, embedding_messages_div)
      
    })
    
    # Reducing for clustering ----
    
    shiny::observe({
      buttons <- c("reducing_method", "n_neighbours", "n_components", "min_dist", "reducing_metric", "do_reducing")
      lapply(buttons, shinyjs::disable)
      
      if (is.array(r$df$embeddings) | is.data.frame(r$df$embeddings)){
        lapply(buttons, shinyjs::enable)
      }
      
    })
    
    shiny::observeEvent(input$do_reducing, {
      req(is.array(r$df$embeddings) | is.data.frame(r$df$embeddings))
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
    
    shiny::observe({
      shiny::req(r$reducing_happening == "happening")
      shiny::invalidateLater(250)
      
      r$reducing_messages <- r$reducing_job$read_error()
      
      if (r$reducing_job$is_alive() == FALSE){
        r$reducing_happening = "finished"
        r$reduced_embeddings <- r$reducing_job$get_result()
        # r$reduced_embeddings <- reduced_embeds
      }
    })
    
    output$reduce_status <- shiny::renderUI({
      
      reducing_animation_div <- progress_annimation(r$reducing_happening, r$reducing_happening, "Reducing in progress", "Reduced!") 
      messages_div <- progress_extract(r$reducing_messages)
      
      shiny::div(reducing_animation_div, messages_div)
      
    })
    
    # Reducing to 2D ----
    
    shiny::observeEvent(input$do_reducing, {
      r$reducing2d_happening <- "happening"
      r$reducing2d_job <- callr::r_bg(function(n_neighbours, n_components, min_dist, metric, embeddings){
        reducer <- BertopicR::bt_make_reducer_umap(n_neighbours = n_neighbours, 
                                                   n_components = n_components, 
                                                   min_dist = min_dist, 
                                                   metric = metric)
        reduced_embeddings <- BertopicR::bt_do_reducing(reducer, embeddings)
      },
      args = list(n_neighbours = input$n_neighbours, n_components = 2, min_dist = input$min_dist, metric = input$reducing_metric, embeddings = r$df$embeddings),
      supervise = TRUE)
    }) 
    
    shiny::observe({
      shiny::req(r$reducing2d_happening == "happening")
      shiny::invalidateLater(250)
      
      r$reducing2d_messages <- r$reducing2d_job$read_error()
      
      if (r$reducing2d_job$is_alive() == FALSE){
        r$reducing2d_happening = "finished"
        r$reduced_embeddings2d <- r$reducing2d_job$get_result()
        # r$reduced_embeddings <- reduced_embeds
      }
    })
    
    output$reduce2d_status <- shiny::renderUI({
      
      reducing2d_animation_div <- progress_annimation(r$reducing2d_happening, r$reducing2d_happening, "Reducing to 2D in progress", "Reduced to 2D!") 
      messages2d_div <- progress_extract(r$reducing2d_messages)
      
      shiny::div(reducing2d_animation_div, messages2d_div)
      
    })
    
    # reduced_embeddings2d <- backgroundReduce(
    #   id = "reduced_embeddings2d",
    #   n_neighbours = input$n_neighbours,
    #   n_components = 2,
    #   min_dist = input$min_dist,
    #   metric = input$reducing_metric,
    #   embeddings = r$df$embeddings,
    #   wait_for_event = TRUE
    # ) # I think I want to change this to be a normal function
    # 
    # shiny::observeEvent(input$do_reducing, {
    #   # reduced_embeddings$start_job()
    #   reduced_embeddings2d$start_job()
    # })
    # 
    # shiny::observe({
    #   # r$reduced_embeddings2d <- df[c("v1", "v2")]
    # #   req(!is.null(reduced_embeddings2d))
    #   r$reduced_embeddings2d<- reduced_embeddings2d$get_result()
    # })
    # 
    # output$reduce2d_status <- shiny::renderUI({
    # 
    #   if (is.array(r$reduced_embeddings2d) | is.data.frame(r$reduced_embeddings2d)){
    #     htmltools::tagList(
    #       htmltools::tags$head(
    #         tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    #       ),
    #       htmltools::div(
    #         class = "reduced-embeddings",
    #         span(class = "check-emoji", "✅"),
    #         span(class = "reducing-text", "Reduced to 2D!")
    #       ))
    #   } else if (stringr::str_detect(reduced_embeddings2d$progress_message(),"Reducing in progress")){
    #     htmltools::tagList(
    #       htmltools::tags$head(
    #         tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    #       ),
    #       htmltools::div(
    #         class = "reducing-embeddings",
    #         span(class = "timer-emoji", "⏳"),
    #         span(class = "reducing-text", "Reducing Embeddings to 2D")
    #       ))
    #   } 
    # })
    
    # ----
  })
}

