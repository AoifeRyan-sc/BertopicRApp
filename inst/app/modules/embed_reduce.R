embedReduceUi <- function(id){
  
  ns <- NS(id)
  
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::div(
        style = "position: relative;",
        htmltools::h4("Step 1: Embed Documents"),
        shiny::div(
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
      shiny::actionButton(ns("embed_button"), label = shiny::HTML("<strong>Embed</strong>"), class = "btn-succes", 
                          width = "100%", style = "margin-bottom: 30px; border-width: 2px;"),
      shiny::uiOutput(ns("embed_status")),
      shiny::div(
        style = "position: relative;",
        htmltools::h4("Step 2: Reduce Embedding Dimension"),
        shiny::div(
          style = "position: absolute; top: 0; right: 5px; transform: translateX(-20%);",
          bslib::tooltip(
            bsicons::bs_icon("question-circle-fill"),
            htmltools::span("Embeddings have too many dimensions for a clustering algorithm to converge on, we use ", 
                            htmltools::tags$a(href = "https://umap-learn.readthedocs.io/en/latest/clustering.html", "UMAP"),
                            " to reduce embeddings to a size that allows clustering. We will also reduce behind the scenes to 2D for visualisaitons purposes."
            )
          )
        )
      ),
      shiny::selectInput(ns("reducing_method"), "Reducing Method", choices = c("UMAP")),
      # choices = c("UMAP", "PCA")),
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
            shiny::numericInput(ns("n_components"), "No. of Dimensions", value = 15),
            style = "margin-top: 25px"
          ),
          style = "margin-top: 30px"
        ),
        shiny::uiOutput(ns("print_status")),
        shiny::actionButton(ns("do_reducing"), label = shiny::HTML("<strong>Reduce</strong>"), class = "btn-succes", 
                            width = "100%", style = "margin-bottom: 30px; border-width: 2px;")
      ),
      shiny::conditionalPanel(
        condition = "input.reducing_method == 'PCA'", ns = ns,
        "This functionality is not yet implemented in the app."
      )
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
      args = list(n_neighbours = input$n_neighbours, n_components = input$n_components, min_dist = 0, metric = "cosine", embeddings = r$df$embeddings),
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
      args = list(n_neighbours = input$n_neighbours, n_components = 2, min_dist = 0, metric = "cosine", embeddings = r$df$embeddings),
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
    
  })
}

