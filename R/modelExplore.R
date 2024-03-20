#' Title
#'
#' @param id 
#'
#' @return
#' @export
#'
#' @examples
modelExploreUi <- function(id){
  ns <- NS(id)
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      uiOutput(ns("summary_table"))
    ),
    shiny::mainPanel(
      shiny::tabsetPanel(
        type = "tabs",
        shiny::tabPanel(
          "Documents",
          DT::dataTableOutput(ns("doc_breakdown"))
        ),
        shiny::tabPanel(
          "UMAP",
          plotly::plotlyOutput(ns("selected_cluster_umap"))
        ),
        shiny::tabPanel(
          "Top Terms / WLO / bigram",
          shiny::plotOutput(ns("top_terms"))
        ),
        shiny::tabPanel(
          "Representation",
          shiny::plotOutput(ns("representation"))
        )
      )
    )
  )
}

#' Title
#'
#' @param id 
#' @param model 
#'
#' @return
#' @export
#'
#' @examples
modelExploreServer <- function(id, model = model, df = df){
  shiny::moduleServer(id, function(input, output, session){
    
    output$summary_table <- shiny::renderUI({
      if(!is.null(model())){
        ns <- session$ns

        print("Rendering datatable output...")
        DT::dataTableOutput(ns("summary_table_output"))

      } else {
        tagList(
          h4("Warning: The condition is not met!"),
          p("Please ensure that the model has the specific value.")
        )
      }
    })
    
    
    output$summary_table_output <- DT::renderDataTable({
      print("Rendering datatable output even more...")
      model()$get_topic_info() %>%
        select(-c(Representative_Docs, Representation)) %>%
        DT::datatable(rownames = FALSE,
                      selection = "single")
    })

    selected_cluster <- reactive({
      req(input$summary_table_output_rows_selected)
      input$summary_table_output_rows_selected - 2
    })

    output$doc_breakdown <- DT::renderDataTable({
      model()$get_document_info(docs = df$docs) %>%
        filter(Topic == selected_cluster()) %>%
        select(Document, Probability) %>%
        DT::datatable(selection = "single")
    })

  })
}