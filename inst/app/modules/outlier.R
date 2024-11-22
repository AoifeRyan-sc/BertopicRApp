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
      shiny::div(
        style = "position: relative;",
        shiny::selectInput(ns("outlier_method"), "Outlier Reduction Method", 
                           choices = c("c-tf-idf",
                                       "embeddings",
                                       "tokenset similarity")),
        shiny::div(
          style = "position: absolute; top: 0; right: 5px; transform: translateX(-20%);",
          bslib::tooltip(
            bsicons::bs_icon("question-circle-fill"),
            htmltools::span(
              "Different methods use different metrics to move outliers into topics, the suitable threshold will change between topics and between outlier reduction methods. You can find some more info by testing methods out and ",
              htmltools::tags$a(href = "https://maartengr.github.io/BERTopic/getting_started/outlier_reduction/outlier_reduction.html", "here"),
              "."
            )
          )
        )
      ),
      shiny::sliderInput(ns("outlier_threshold"),
                  "Threshold:",
                  min = 0,
                  max = 1,
                  value = 0.3),
    ),
    shiny::mainPanel(
      shiny::uiOutput(ns("outlier_display")),
      DT::dataTableOutput(ns("selected_outlier_data_df")),
      shiny::downloadButton(ns("data_download_outliers"), label = "Download Data Table"),
      shiny::downloadButton(ns("download_topic_model"), label = "Download Topic Model"),
      shiny::br(),
      shiny::htmlOutput(ns("download_note"))
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
outlierServer <- function(id, r){
  
  shiny::moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    shiny::observe({
      r$method <- input$outlier_method
      r$threshold <- input$outlier_threshold
    })
    
    outliers <- shiny::reactive({
        if (r$method == "c-tf-idf") {
          BertopicR::bt_outliers_ctfidf(
            fitted_model = r$model,
            documents = r$df$docs,
            topics = r$clusters(), 
            threshold = r$threshold)
        } else if (r$method == "embeddings") {
          BertopicR::bt_outliers_embeddings(
            fitted_model = r$model,
            documents = r$df$docs,
            topics = r$clusters(), 
            embeddings = r$df$embeddings,
            embedding_model = r$embedder,
            threshold = r$threshold)
        } else if (r$method == "tokenset similarity") {
          BertopicR::bt_outliers_tokenset_similarity(
            fitted_model = r$model,
            documents = r$df$docs,
            topics = r$clusters(),
            threshold = r$threshold)
        }
    })
    
    new_topics <- shiny::eventReactive(outliers(), {
      outliers()$new_topics
    })
    
    output$outlier_display <- shiny::renderUI({
      if(!is.null(r$model)){
        plotly::plotlyOutput(ns("outlier_plot"))
      } else{
        shiny::tagList(
          shiny::h4("Warning: No model has been generated."),
          shiny::p("To generate a model, set the parameters in the clustering panel to your desired values and click `Model`.")
        )
      }
    })

    output$outlier_plot <- plotly::renderPlotly({
      o <- createUmap("umap_outliers", df = r$df, colour_var = new_topics(),
                      title = "UMAP of document embeddings: Reassigning outliers")

    })
    
    outlier_display_data <- shiny::reactive({
      shiny::req(r$model)
      selected <- plotly::event_data("plotly_selected", source = "umap_outliers")
      df_outlier_temp <- r$df %>% 
        dplyr::select(-c(embeddings, v1, v2)) %>%
        dplyr::mutate(new_topics = new_topics(),
                      old_topcis = r$clusters())
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
    
    
    output$download_topic_model <- shiny::downloadHandler(
      filename = function() {
        print(r$model)
        paste0("topic_model", format(Sys.time(), "%d-%m-%Y_%H:%M:%S"), ".bt")
      },
      content = function(file) {
        r$model$save(file)
      }
    )
    
    output$download_note <- shiny::renderUI({
      shiny::HTML("<i>Note that when retrieving a Bertopic model in your rstudio later, you must access the filepath relative to your wd, you cannot retrieve it relative to the home directory using a '~'.</i>")
    })
    

  })
 
}
