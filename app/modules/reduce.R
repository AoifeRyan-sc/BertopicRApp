
#' Ui specs for Reducing tab
#'
#' @param id parameter for shiny identification
#'
#' @noRd
#' 
reducingCalcUi <- function(id){
  
  ns <- shiny::NS(id)
  
  shiny::tagList(
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


#' Reducing UI server function
#'
#' @param id parameter for shiny identification
#' @param df reactive dataframe containing docs and embedding info 
#'
#' @noRd
#'
  reducingCalcServer <- function(id, r){
  shiny::moduleServer(id, function(input, output, session){
    
    # ns <- session$ns
    
    shiny::observeEvent(input$do_reducing, {print("button pressed")})
    
    reduced_embeddings1 <- backgroundReduce(
      id = "reduced_embeddings1",
      n_neighbours = input$n_neighbours1,
      n_components = input$n_components1,
      min_dist = input$min_dist1,
      metric = input$reducing_metric1,
      embeddings = r$df$embeddings,
      wait_for_event = TRUE
    )
      
    shiny::observeEvent(input$do_reducing, {
      reduced_embeddings1$start_job()
      print("starting job")
    }) 
    
    flag <- shiny::reactiveValues(epoch_completed = FALSE)
    output$print_status <- shiny::renderText({
      reduced_embeddings1$progress_message()
    })
    
    output$print_status <- shiny::renderUI({
      if (stringr::str_detect(reduced_embeddings1$progress(),"Reducing in progress")){
        htmltools::tagList(
          htmltools::tags$head(
          tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
        ),
        htmltools::div(
          class = "reducing-embeddings",
          span(class = "timer-emoji", "⏳"),
          span(class = "reducing-text", "Reducing Embeddings")
        ))
      } else if (stringr::str_detect(reduced_embeddings1$progress(), "Epochs completed")){
        htmltools::tagList(
          htmltools::tags$head(
          tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
        ),
        htmltools::div(
          class = "reduced-embeddings",
          span(class = "check-emoji", "✅"),
          span(class = "reducing-text", "Reduced!")
        ))
      }
      
    })

    
    # output$pca_unavailable_message <- shiny::HTML(p(
    #   "This functionality is not yet implemented in the app."
    #   ))
    return(
      # list(
      shiny::reactive(reduced_embeddings1$get_result())
      # embeddings2 = reduced_embeddings2$get_result()
    # )
    )
    
    
  })
  
}

