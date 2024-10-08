#' UI specs for Explore Model tab
#'
#' @param id parameter for shiny identification
#'
#' @noRd
#' 
modelExploreUi <- function(id){
  ns <- shiny::NS(id)
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::uiOutput(ns("topic_summary_display")),
      width = 4
    ),
    shiny::mainPanel(
      shiny::tabsetPanel(
        type = "tabs",
        shiny::tabPanel(
          "Documents",
          shiny::br(),
          shiny::uiOutput(ns("doc_breakdown_display")),
          value = 1
        ),
        shiny::tabPanel(
          "UMAP",
          shiny::br(),
          # plotly::plotlyOutput(ns("model_explore_umap")),
          shiny::uiOutput(ns("model_explore_umap_display")),
          value = 2
        ),
        shiny::tabPanel(
          "WLO",
          shiny::br(),
          shiny::uiOutput(ns("wlo_display")),
          value = 3
        ),
        shiny::tabPanel(
          "Representation",
          shiny::br(),
          # shiny::plotOutput(ns("representation")),
          shiny::uiOutput(ns("representation_display")),
          value = 4
        ),
        id = ns("model_explore_panels")
      ),
      width = 8,
    )
  )
}

#' Explore Model UI Server Function
#'
#' @param id parameter for shiny identification
#' @param model Bertopic model created
#' @param df reactive dataframe containing docs and embedding info 
#'
#' @noRd
modelExploreServer <- function(id, r){
  shiny::moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    output$topic_summary_display <- shiny::renderUI({
      if(!is.null(r$model)){
        shiny::tagList(
          shiny::br(),
          DT::dataTableOutput(ns("topic_summary"))
        )
        
      } else {
        shiny::tagList(
          shiny::h4("Warning: No model has been generated."),
          shiny::p("To generate a model, set the parameters in the clustering panel to your desired values and click `Model`.")
        )
      }
    }) # conditional display
    
    output$topic_summary <- DT::renderDataTable({
      r$model$get_topic_info() %>%
        dplyr::select(-c(Representative_Docs, Representation)) %>%
        DT::datatable(rownames = FALSE,
                      selection = "multiple",
                      width = 4)
    })
    
    shiny::observe({
      shiny::req(input$topic_summary_rows_selected)
      selected_topic <- input$topic_summary_rows_selected - 2
      print(selected_topic)
      r$df_selected_topic <- r$df[r$df$topics %in% as.list(selected_topic), ]
    })
    
    # selected_cluster <- shiny::reactive({
    #   shiny::req(input$topic_summary_rows_selected)
    #    input$topic_summary_rows_selected - 2
    # }) # topic selected in topic summary table
    
    # df_explore_model <- shiny::reactive({
    #   r$df %>%
    #   dplyr::mutate(topic = r$model$topics_) %>%
    #   dplyr::filter(topic %in% as.list(selected_cluster()))
    # })
    
    output$doc_breakdown_display <- shiny::renderUI({
      if(!is.null(r$model)){
        shiny::tagList(
          shiny::br(),
          DT::dataTableOutput(ns("doc_breakdown"))
        )
        
      } else {
        shiny::tagList(
          shiny::h4("Warning: No model has been generated."),
          shiny::p("To generate a model, set the parameters in the clustering panel to your desired values and click `Model`.")
        )
      }
    }) # conditional display
    
    output$doc_breakdown <- DT::renderDataTable({
      req(r$df_selected_topic)
      r$df_selected_topic %>%
        dplyr::select(docs, topics) %>%
        DT::datatable(selection = "single")
    })
    
    output$model_explore_umap_display <- shiny::renderUI({
      if(!is.null(r$model)){
        shiny::tagList(
          shiny::br(),
          plotly::plotlyOutput(ns("model_explore_umap"))
        )
        
      } else {
        shiny::tagList(
          shiny::h4("Warning: No model has been generated."),
          shiny::p("To generate a model, set the parameters in the clustering panel to your desired values and click `Model`.")
        )
      }
    }) # conditional display
    
    output$model_explore_umap <- plotly::renderPlotly({
      req(r$df_selected_topic)
      createUmap("model_explore_umap", df = r$df_selected_topic, 
                 colour_var = r$df_selected_topic$topics,
                 title = "UMAP of document embeddings: Topic zoom")
      # plotly::event_register(o, "plotly_selected")
      # o
    })
    
    shiny::observeEvent(r$model, {
      req(!is.null(r$model))
      named_options <- r$model$get_topic_info() %>% dplyr::distinct(Name) %>% dplyr::pull(Name)
      
      shiny::updateSelectInput(inputId = "wlo_topic_selection",
                               choices = c(named_options),
                               selected = named_options[1:2])
    })
    
    output$wlo_display <- shiny::renderUI({
      if(!is.null(r$model)){
        shiny::plotOutput(ns("wlo"))
        
      } else {
        shiny::tagList(
          shiny::h4("Warning: No model has been generated."),
          shiny::p("To generate a model, set the parameters in the clustering panel to your desired values and click `Model`.")
        )
      }
    }) # conditional display
    
    output$wlo <- shiny::renderPlot({
      data <- r$model$get_document_info(docs = r$df$docs)
      r$df_selected_topic %>%
        # dplyr::filter(Name %in% input$wlo_topic_selection) %>%
        # dplyr::filter(Topic %in% selected_cluster()) %>%
        calculate_wlos_app(topic_var = topics,
                           text_var = docs,
                           filter_by = "association")
    })
    
    output$representation_display <- shiny::renderUI({
      if(!is.null(r$model)){
        shiny::tagList(
          # shiny::br(),
          # plotly::plotOutput(ns("representaion"))
          shiny::br()
        )
        
      } else {
        shiny::tagList(
          shiny::h4("Warning: No model has been generated."),
          shiny::p("To generate a model, set the parameters in the clustering panel to your desired values and click `Model`.")
        )
      }
    }) # conditional display
    
    
  })
}