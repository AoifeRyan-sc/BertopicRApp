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
  reducingServer <- function(id, df){
  shiny::moduleServer(id, function(input, output, session){
    
    # ns <- session$ns
    
    observeEvent(input$do_reducing_option1, {print("button pressed")})
     
    reduced_embeddings1 <- reducingAsyncServer(
      id = "reduced_embeddings1", 
      n_neighbours = input$n_neighbours1, 
      n_components = input$n_components1,
      min_dist = input$min_dist1, 
      metric = input$reducing_metric1,
      embeddings = df()$embeddings,
      wait_for_event = TRUE
    )
    
    observeEvent(input$do_reducing_option1, {
      reduced_embeddings1$start_job()
    }) 
    
    output$print_stats <- shiny::renderPrint({
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


#' Title
#'
#' @param id 
#'
#' @return
#' @export
#'
#' @examples
reducingUiSave <- function(id){
  
  ns <- shiny::NS(id)

  shiny::sidebarLayout(
    shiny::sidebarPanel(
#       tags$style(type = "text/css", ".form-control.shiny-bound-input, 
# .selectize-input {height: 40px;}"),
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
                            width = "100%", style = "margin-bottom: 30px; border-width: 2px;"),
        shiny::div(
          class = "row",
          shiny::div(
            class = "col-md-6",
            shiny::numericInput(ns("n_neighbours2"), "No. of Nearest Neighbours", value = 15)
          ),
          shiny::div(
            class= "col-md-6",
            shiny::numericInput(ns("n_components2"), "No. of Dimensions", value = 5)
          )
        ),
        shiny::div(
          class = "row",
          shiny::div(
            class = "col-md-7",
            shiny::numericInput(ns("min_dist2"), "Min Distance Between Points", value = 0)
          ),
          shiny::div(
            class= "col-md-5",
            shiny::selectInput(ns("reducing_metric2"), "Distance Metric", choices = c("cosine", "euclidean")) # expand this
          )
        ),
        shiny::actionButton(ns("do_reducing_option2"), label = shiny::HTML("<strong>Reduce</strong>"), class = "btn-succes", 
                            width = "100%", style = "border-width: 2px;")
      ),
      shiny::conditionalPanel(
        condition = "input.cluster_method == 'PCA'", ns = ns,
        shiny::numericInput(ns("this_is_a_test"), "This is a test", value = 0)
      )
    ),
    shiny::mainPanel(
      # plotly::plotlyOutput(ns("reducing_plot"))
      shiny::verbatimTextOutput(ns("reducer1")),
      shiny::verbatimTextOutput(ns("reducer2")),
      shiny::verbatimTextOutput(ns("reduced_embeddings_1")),
      shiny::verbatimTextOutput(ns("reduced_embeddings_2"))
      )
  )
  
}

#' Title
#'
#' @param id 
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
reducingServerSave <- function(id, df){
  shiny::moduleServer(id, function(input, output, session){
    
    reducer1 <- shiny::eventReactive(input$do_reducing_option1, {
      print("reduced button 1")
      bt_make_reducer_umap(n_neighbours = input$n_neighbours1, n_components = input$n_components1,
                           min_dist = input$min_dist1, metric = input$reducing_metric1)
    })
    
    reduced_embeddings1 <- shiny::eventReactive(reducer1(), {
      print("reducing embeddings 1")
      callr::r_bg(function(){
        bt_do_reducing(reducer1, df()$embeddings)
      },
      supervise = TRUE)
    })
    
    reducer2 <- shiny::eventReactive(input$do_reducing_option2, {
      print("reduced button 1")
      bt_make_reducer_umap(n_neighbours = input$n_neighbours2, n_components = input$n_components2,
                           min_dist = input$min_dist2, metric = input$reducing_metric2)
    })
    
    reduced_embeddings2 <- shiny::eventReactive(reducer2(), {
      print("reducing embeddings 2")
      callr::r_bg(function(){
        bt_do_reducing(reducer2, df()$embeddings)
      })
    })
    
    # reduced_embeddings2 <- reducingAsyncServer(
    #   id = "idk_really", 
    #   fun_async = "do_reducing_async",
    #   fun_args = list(reducer = reducer2, df = df),
    #   wait_for_event = TRUE
    # )
    
    # observe({
    #   reduced_embeddings2$start_job()
    # }) |>  bindEvent(input$do_reducing_option2)
    
    res_rct <- shiny::reactiveVal(NULL)
    observe({
      alive <- reduced_embeddings2()$is_alive()
      if (isFALSE(alive)) {
        res_rct(reduced_embeddings2()$get_result())
        message(sprintf("done: %s", id))
        # poll_rct(FALSE)
      }
    })
    
    output$reducer1 <- renderPrint({
      paste0("reducer 1: ", reducer1())
    })
    
    output$reduced_embeddings_1 <- renderPrint({
      paste0("reduced embeddings 1: ", reduced_embeddings1()$get_result())
    })
    
    output$reduced_embeddings_2 <- renderPrint({
      # paste0("reduced embeddings 2: ", reduced_embeddings2()$get_result())
      # 
      paste0("reduced embeddings 2: ", reactive(res_rct()))
    })
    
    output$reducer2 <- renderPrint({
      paste0("reducer 2: ", reducer2())
    })
  })
  
}


reducingUi_2param_sets <- function(id){
  
  ns <- shiny::NS(id)
  
  shiny::tagList(
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
                          width = "100%", style = "margin-bottom: 30px; border-width: 2px;"),
      shiny::div(
        class = "row",
        shiny::div(
          class = "col-md-6",
          shiny::numericInput(ns("n_neighbours2"), "No. of Nearest Neighbours", value = 15)
        ),
        shiny::div(
          class= "col-md-6",
          shiny::numericInput(ns("n_components2"), "No. of Dimensions", value = 5)
        )
      ),
      shiny::div(
        class = "row",
        shiny::div(
          class = "col-md-7",
          shiny::numericInput(ns("min_dist2"), "Min Distance Between Points", value = 0)
        ),
        shiny::div(
          class= "col-md-5",
          shiny::selectInput(ns("reducing_metric2"), "Distance Metric", choices = c("cosine", "euclidean")) # expand this
        )
      ),
      shiny::actionButton(ns("do_reducing_option2"), label = shiny::HTML("<strong>Reduce</strong>"), class = "btn-succes", 
                          width = "100%", style = "border-width: 2px;")
    ),
    shiny::conditionalPanel(
      condition = "input.cluster_method == 'PCA'", ns = ns,
      shiny::numericInput(ns("this_is_a_test"), "This is a test", value = 0)
    ),
    shiny::verbatimTextOutput(ns("printtest"))
  )
}

reducingUi_trying_to_abstract_ui_elements <- function(id){
  
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::selectInput(ns("reducing_method"), "Reducing Method", choices = c("UMAP", "PCA")),
    shiny::conditionalPanel(
      condition = "input.reducing_method == 'UMAP'", ns = ns,
      reducingParamsUi(ns("reducing_params"), id_num = "1")
    ),
    shiny::conditionalPanel(
      condition = "input.cluster_method == 'PCA'", ns = ns,
      shiny::numericInput(ns("this_is_a_test"), "This is a test", value = 0)
    ),
    shiny::verbatimTextOutput(ns("printtest"))
  )
}