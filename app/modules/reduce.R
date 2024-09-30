#' Ui specs for Reducing tab
#'
#' @param id parameter for shiny identification
#'
#' @noRd
#' 
reducingUi <- function(id){
  
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
        shiny::verbatimTextOutput(ns("print_status")),
        shiny::actionButton(ns("do_reducing_option1"), label = shiny::HTML("<strong>Reduce</strong>"), class = "btn-succes", 
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
  reducingServer <- function(id, df){
  shiny::moduleServer(id, function(input, output, session){
    
    # ns <- session$ns
    
    shiny::observeEvent(input$do_reducing_option1, {print("button pressed")})
    
    reduced_embeddings1 <- backgroundReduce(
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
      print("starting job")
    }) 
    
    flag <- rectiveValues(epoch_completed = FALSE)
    
    output$print_status <- shiny::renderText({
      # message <- ""
      # print(reduced_embeddings1$progress())
      if (stringr::str_detect(progress, "Epochs completed")) {
        message <- paste(message, reduced_embeddings1$progress(), collapse = "\n")
        flag$epoch_completed <- TRUE
      }
      
      
      if (stringr::str_detect(reduced_embeddings1$progress(),"Reducing in progress")){
        message <- reduced_embeddings1$progress() 
        print("message is space and reducing in progress")
        message
      } else if (stringr::str_detect(reduced_embeddings1$progress(), "Epochs completed")){
        
        print("epochs")
        message
        # print_message <- message
      } else {
        print("other")
        message
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

