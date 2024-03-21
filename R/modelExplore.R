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
      shiny::conditionalPanel(
        condition = "input.model_explore_panels == 3", ns = ns,
        shiny::selectInput(
          inputId = ns("wlo_topic_selection"),
          label = "Topics to Compare",
          choices = NULL,
          multiple = TRUE)
      ),
      shiny::uiOutput(ns("summary_table")),
      width = 6
    ),
    shiny::mainPanel(
      shiny::tabsetPanel(
        type = "tabs",
        shiny::tabPanel(
          "Documents",
          DT::dataTableOutput(ns("doc_breakdown")),
          value = 1
        ),
        shiny::tabPanel(
          "UMAP",
          plotly::plotlyOutput(ns("selected_cluster_umap")),
          value = 2
        ),
        shiny::tabPanel(
          "WLO",
          shiny::plotOutput(ns("wlo")),
          value = 3
        ),
        shiny::tabPanel(
          "Representation",
          shiny::plotOutput(ns("representation")),
          value = 4
        ),
        id = ns("model_explore_panels")
      ),
      width = 6,
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
        DT::dataTableOutput(ns("summary_table_output"))

      } else {
        tagList(
          h4("Warning: No model has been generated."),
          p("To generate a model, set the parameters in the clustering panel to your desired values and click `Model`.")
        )
      }
    }) # conditional display 
    
    output$summary_table_output <- DT::renderDataTable({
      model()$get_topic_info() %>%
        select(-c(Representative_Docs, Representation)) %>%
        DT::datatable(rownames = FALSE,
                      selection = "single",
                      width = 4)
    })

    selected_cluster <- reactive({
      req(input$summary_table_output_rows_selected)
      input$summary_table_output_rows_selected - 2
    }) # topic selected in topic summary table

    output$doc_breakdown <- DT::renderDataTable({
      model()$get_document_info(docs = df$docs) %>%
        filter(Topic == selected_cluster()) %>%
        select(Document, Probability) %>%
        DT::datatable(selection = "single")
    })
    
    shiny::observeEvent(model(), {
      named_options <- model()$get_topic_info() %>% distinct(Name)
      topic_options <- model()$get_topic_info() %>% distinct(Topic)
      shiny::updateSelectInput(inputId = "wlo_topic_selection", choices = c(topic_options = named_options))
    })
    
    output$wlo <- shiny::renderPlot({
      data <- model()$get_document_info(docs = df$docs)
      data %>% 
        dplyr::filter(Name %in% input$wlo_topic_selection) %>%
        ParseR::calculate_wlos(topic_var = Topic, 
                               text_var = Document,
                               filter_by = "association")
    })

  })
}