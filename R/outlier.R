#' ui specs for outlier tab
#'
#' @param id 
#'
#' @return
#' @export
#'
#' @examples
outlier_ui <- function(id) {
  # Application title

  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("outlier_method", "Outlier Reduction Method", 
                  choices = c("c-tf-idf",
                              "embeddings",
                              "tokenset similarity")),
      sliderInput("outlier_threshold",
                  "Threshold:",
                  min = 0,
                  max = 1,
                  value = 0.3)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("outlier_plot")
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
#' outlier_server <- function(input, output, session){
#'   
#'   method <- reactive(input$outlier_method)
#'   threshold <- reactive(input$outlier_threshold)
#'   
#'   
#'   output$outlier_plot <- renderPlot({
#'     
#'     if (method() == "c-tf-idf"){
#'       outliers <- bt_outliers_ctfidf(fitted_model = model(),
#'                                      documents = df$docs,
#'                                      topics = hdbscan_clusters(), # THIS NEEDS TO CHANGE WHEN I INTEGRATE KMEANS
#'                                      threshold = threshold())
#'     } else if (method() == "embeddings"){
#'       outliers <- bt_outliers_embeddings(fitted_model = model(),
#'                                          documents = df$docs,
#'                                          topics = hdbscan_clusters(), # THIS NEEDS TO CHANGE WHEN I INTEGRATE KMEANS
#'                                          embeddings = embeddings,
#'                                          embedding_model = embedder,
#'                                          threshold = threshold())
#'     } else if (method() == "tokenset similarity"){
#'       outliers <- bt_outliers_tokenset_similarity(fitted_model = model(),
#'                                                   documents = df$docs,
#'                                                   topics = hdbscan_clusters(), # THIS NEEDS TO CHANGE WHEN I INTEGRATE KMEANS
#'                                                   threshold = threshold())
#'     }
#'     
#'     colour_pal <- metafolio::gg_color_hue(length(unique(outliers$current_topics)))
#'     
#'     df %>%
#'       mutate(new_topics = as.factor(outliers$new_topics)) %>%
#'       ggplot(aes(x = v1, y = v2, colour = new_topics)) +
#'       geom_point() +
#'       scale_color_manual(values = colour_pal)
#'   })
#'   
#' }
