#' ui specs for outlier tab
#'
#' @param id 
#'
#' @return
#' @export
#'
#' @examples
outlierUi <- function(id) {
  
  ns <- NS(id)

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
      # plotly::plotlyOutput(ns("outlier_plot"))
      shiny::uiOutput(ns("outlier_display"))
    )
  )
}
#' # 
#' #' Title
#' #'
#' #' @param input 
#' #' @param output 
#' #' @param session 
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
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
    
    new_topics <- eventReactive(outliers(), {
      outliers()$new_topics
    })
    
    output$outlier_display <- shiny::renderUI({
      if(!is.null(model())){
        plotly::plotlyOutput(ns("outlier_plot"))
      } else{
        tagList(
          h4("Warning: No model has been generated."),
          p("To generate a model, set the parameters in the clustering panel to your desired values and click `Model`.")
        )
      }
    })

    output$outlier_plot <- umapServer("umap_outliers", df = df, colour_var = new_topics)

  })
 
}

outlierServer_save <- function(id, df, model, clusters, embedder){
  
  shiny::moduleServer(id, function(input, output, session){
    method <- shiny::reactive(input$outlier_method)
    threshold <- shiny::reactive(input$outlier_threshold)
    
    
    output$outlier_plot <- shiny::renderPlot({
      
      if (method() == "c-tf-idf"){
        outliers <- BertopicR::bt_outliers_ctfidf(
          # fitted_model = model,
          fitted_model = model(),
          documents = df$docs,
          topics = clusters(), # THIS NEEDS TO CHANGE WHEN I INTEGRATE KMEANS
          threshold = threshold())
      } else if (method() == "embeddings"){
        outliers <- BertopicR::bt_outliers_embeddings(
          # fitted_model = model,
          fitted_model = model(),
          documents = df$docs,
          topics = clusters(), # THIS NEEDS TO CHANGE WHEN I INTEGRATE KMEANS
          embeddings = df$embeddings,
          embedding_model = embedder,
          threshold = threshold())
      } else if (method() == "tokenset similarity"){
        outliers <- BertopicR::bt_outliers_tokenset_similarity(
          # fitted_model = model,
          fitted_model = model(),
          documents = df$docs,
          topics = clusters(), # THIS NEEDS TO CHANGE WHEN I INTEGRATE KMEANS
          threshold = threshold())
      }
      
      colour_pal <- metafolio::gg_color_hue(length(unique(outliers$current_topics)))
      
      df %>%
        dplyr::mutate(new_topics = as.factor(outliers$new_topics)) %>%
        ggplot2::ggplot(aes(x = v1, y = v2, colour = new_topics)) +
        ggplot2::geom_point() 
      # +
      #   ggplot2::scale_color_manual(values = colour_pal)
    })
    
  })
  
}
