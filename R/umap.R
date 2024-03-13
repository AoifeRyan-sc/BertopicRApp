umapUi <- function(id){
  ns <- NS(id)
  tagList(
    plotOutput(ns("cluster_plot"))
    )
}

umapServer <- function(id, df = df, clusters = clusters){
  # if (!is.reactive(clusters)){
  #   stop("Clusters should be a reactive variable.")
  # }
  # 
  # if (is.reactive(df)){
  #   stop("Input dataframe should not be reactive.")
  # }
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    output$cluster_plot <- shiny::renderPlot({
      
      # cluster_pal <- gg_color_hue(length(unique(clusters())))
      
      df %>%
        dplyr::mutate(topics = as.factor(clusters())) %>%
        ggplot2::ggplot(aes(x = v1, y = v2, colour = topics)) +
        ggplot2::geom_point() +
        # scale_color_manual(values = cluster_pal) +
        ggplot2::theme_bw()
    })
  })
}