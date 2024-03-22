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
      shiny::uiOutput(ns("topic_summary_display")),
      width = 6
    ),
    shiny::mainPanel(
      shiny::tabsetPanel(
        type = "tabs",
        shiny::tabPanel(
          "Documents",
          # DT::dataTableOutput(ns("doc_breakdown")),
          shiny::uiOutput(ns("doc_breakdown_display")),
          value = 1
        ),
        shiny::tabPanel(
          "UMAP",
          plotly::plotlyOutput(ns("selected_cluster_umap")),
          value = 2
        ),
        shiny::tabPanel(
          "WLO",
          shiny::uiOutput(ns("wlo_display")),
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
    ns <- session$ns
    
    output$topic_summary_display <- shiny::renderUI({
      if(!is.null(model())){
        DT::dataTableOutput(ns("topic_summary"))

      } else {
        tagList(
          h4("Warning: No model has been generated."),
          p("To generate a model, set the parameters in the clustering panel to your desired values and click `Model`.")
        )
      }
    }) # conditional display 
    
    output$topic_summary <- DT::renderDataTable({
      model()$get_topic_info() %>%
        select(-c(Representative_Docs, Representation)) %>%
        DT::datatable(rownames = FALSE,
                      selection = "single",
                      width = 4)
    })
    
    output$doc_breakdown_display <- shiny::renderUI({
      if(!is.null(model())){
        DT::dataTableOutput(ns("doc_breakdown"))
        
      } else {
        tagList(
          h4("Warning: No model has been generated."),
          p("To generate a model, set the parameters in the clustering panel to your desired values and click `Model`.")
        )
      }
    }) # conditional display 
    
    output$doc_breakdown <- DT::renderDataTable({
      model()$get_document_info(docs = df()$docs) %>%
        filter(Topic == selected_cluster()) %>%
        select(Document) %>%
        DT::datatable(selection = "single")
    })

    selected_cluster <- reactive({
      req(input$topic_summary_rows_selected)
      input$topic_summary_rows_selected - 2
    }) # topic selected in topic summary table
    
    shiny::observeEvent(model(), {
      named_options <- model()$get_topic_info() %>% distinct(Name) %>% pull(Name)
      
      shiny::updateSelectInput(inputId = "wlo_topic_selection", 
                               choices = c(named_options),
                               selected = named_options[1:2])
    })
    
    output$wlo_display <- shiny::renderUI({
      if(!is.null(model())){
        shiny::plotOutput(ns("wlo"))
        
      } else {
        tagList(
          h4("Warning: No model has been generated."),
          p("To generate a model, set the parameters in the clustering panel to your desired values and click `Model`.")
        )
      }
    }) # conditional display 
    
    output$wlo <- shiny::renderPlot({
      data <- model()$get_document_info(docs = df()$docs)
      data %>% 
        dplyr::filter(Name %in% input$wlo_topic_selection) %>%
        ParseR::calculate_wlos(topic_var = Topic, 
                               text_var = Document,
                               filter_by = "association")
    })

  })
}