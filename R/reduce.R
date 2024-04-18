#' Ui specs for Reducing tab
#'
#' @param id parameter for shiny identification
#'
#' @noRd
#' 
reducingUi <- function(id){
  
  ns <- shiny::NS(id)
  
  shiny::tagList(
    # shiny::radioButtons(ns("load_or_reduce_embeddings"), 
    #                     "Do you want to load pre-calculate reduced embeddings or do it here?",
    #                     choices = c("Load in reduced embeddings", 
    #                                 "Calculate in app")),
    # shiny::conditionalPanel(
    #   condition = "input.load_or_reduce_embeddings == 'Load in reduced embeddings'", ns = ns,
    #   shiny::fileInput(ns("reduced_embeddings_upload"), "Upload Reduced Embeddings",
    #                    accept = c(".xlsx", ".csv", ".tsv", ".rds", ".rda"), multiple = FALSE)
    # ),
    # shiny::conditionalPanel(
      # condition = "input.load_or_reduce_embeddings == 'Calculate in app'", ns = ns,
      shiny::selectInput(ns("reducing_method"), "Reducing Method", choices = c("UMAP", "PCA")),
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
        shiny::actionButton(ns("do_reducing_option1"), label = shiny::HTML("<strong>Reduce</strong>"), class = "btn-succes", 
                            width = "100%", style = "margin-bottom: 30px; border-width: 2px;")
      ),
      shiny::conditionalPanel(
        condition = "input.cluster_method == 'PCA'", ns = ns,
        shiny::numericInput(ns("this_is_a_test"), "This is a test", value = 0)
      # )
      ),
      shiny::verbatimTextOutput(ns("print_status"))
    )
}


#' Reducing UI server function
#'
#' @param id parameter for shiny identification
#' @param df reactive dataframe containing docs and embedding info 
#'
#' @noRd
#'
  reducingServer <- function(id, df){
  shiny::moduleServer(id, function(input, output, session){
    
    # ns <- session$ns
    
    shiny::observeEvent(input$do_reducing_option1, {print("button pressed")})
    
    reduced_embeddings1 <- reducingAsyncServer(
      id = "reduced_embeddings1",
      n_neighbours = input$n_neighbours1,
      n_components = input$n_components1,
      min_dist = input$min_dist1,
      metric = input$reducing_metric1,
      embeddings = df()$embeddings,
      wait_for_event = TRUE
    )
      
    shiny::observeEvent(input$do_reducing_option1, {
      reduced_embeddings1$start_job()
    }) 
    
    output$print_status <- shiny::renderPrint({
      reduced_embeddings1$get_result()
      # session$ns
    })
    return(
      # list(
      shiny::reactive(reduced_embeddings1$get_result())
      # embeddings2 = reduced_embeddings2$get_result()
    # )
    )
    
    
  })
  
}

