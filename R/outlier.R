#' ui specs for outlier tab
#'
#' @param id parameter for shiny identification
#'
#' @noRd
#'
outlierUi <- function(id) {
  
  ns <- shiny::NS(id)

  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::selectInput(ns("outlier_method"), "Outlier Reduction Method", 
                  choices = c("c-tf-idf",
                              "embeddings",
                              "tokenset similarity")),
      shiny::sliderInput(ns("outlier_threshold"),
                  "Threshold:",
                  min = 0,
                  max = 1,
                  value = 0.3)
    ),
    shiny::mainPanel(
      shiny::uiOutput(ns("outlier_display")),
      DT::dataTableOutput(ns("selected_outlier_data_df")),
      shiny::downloadButton(ns("data_download_outliers"), label = "Download Data Table")
    )
  )
}

#' Outlier Ui Server Function
#'
#' @param input parameter for shiny identification
#' @param output parameter for shiny identification
#' @param session parameter for shiny identification
#' @param df reactive dataframe containing docs and embedding info 
#' @param clusters reactive list of clusters corresponding to docs in df
#' @param embedder reactive (?) embedder used to create embeddings 
#' 
#' @noRd
#' 
outlierServer <- function(id, df, model, clusters, embedder){
  
  shiny::moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    method <- shiny::reactive({input$outlier_method})
    threshold <- shiny::reactive({input$outlier_threshold})
    
    outliers <- shiny::reactive({
        if (method() == "c-tf-idf") {
          BertopicR::bt_outliers_ctfidf(
            fitted_model = model(),
            documents = df()$docs,
            topics = clusters(), 
            threshold = threshold())
        } else if (method() == "embeddings") {
          BertopicR::bt_outliers_embeddings(
            fitted_model = model(),
            documents = df()$docs,
            topics = clusters(), 
            embeddings = df()$embeddings,
            embedding_model = embedder,
            threshold = threshold())
        } else if (method() == "tokenset similarity") {
          BertopicR::bt_outliers_tokenset_similarity(
            fitted_model = model(),
            documents = df()$docs,
            topics = clusters(),
            threshold = threshold())
        }
    })
    
    new_topics <- shiny::eventReactive(outliers(), {
      outliers()$new_topics
    })
    
    output$outlier_display <- shiny::renderUI({
      if(!is.null(model())){
        plotly::plotlyOutput(ns("outlier_plot"))
      } else{
        shiny::tagList(
          shiny::h4("Warning: No model has been generated."),
          shiny::p("To generate a model, set the parameters in the clustering panel to your desired values and click `Model`.")
        )
      }
    })

    # output$outlier_plot <- umapServer("umap_outliers", df = df, colour_var = new_topics)
    output$outlier_plot <- plotly::renderPlotly({
      o <- createUmap("umap_outliers", df = df, colour_var = new_topics)
      plotly::event_register(o, "plotly_selected")
      o
    })
    
    outlier_display_data <- shiny::reactive({
      selected <- plotly::event_data("plotly_selected")
      df_outlier_temp <- df() %>% 
        dplyr::select(-c(reduced_embeddings, embeddings, v1, v2)) %>%
        dplyr::mutate(new_topics = new_topics(),
                      old_topcis = clusters())
      df_outlier_temp[df_outlier_temp$rowid %in% selected$customdata, ] %>% 
        dplyr::select(-rowid)
    })
    
    output$selected_outlier_data_df <- DT::renderDataTable({
      outlier_display_data()
    })
    
    output$data_download_outliers <- shiny::downloadHandler(
      filename = function() {
        paste0("outlier_data_", format(Sys.time(), "%d-%m-%Y_%H:%M:%S"), ".csv")
      },
      content = function(file) {
        utils::write.csv(outlier_display_data(), file, row.names = FALSE)
      }
    )
    

  })
 
}
