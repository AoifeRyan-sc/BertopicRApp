umapUi <- function(id){
  ns <- NS(id)
  tagList(
    shiny::plotOutput(ns("cluster_plot"))
    )
}

umapServer <- function(id, df = df, colour_var){
  # if (!is.reactive(clusters)){
  #   stop("Clusters should be a reactive variable.")
  # }
  # 
  # if (is.reactive(df)){
  #   stop("Input dataframe should not be reactive.")
  # }
  moduleServer(id, function(input, output, session){
    
    plotly::renderPlotly({
      color_pal <- pals::stepped2(length(unique(colour_var())) - 1)
      
      df %>% dplyr::mutate(topics = as.factor(colour_var())) %>%
      plotly::plot_ly(x = ~v1,
                      y = ~v2,
                      color = ~topics,
                      type = "scatter", mode = "markers",
                      text = ~docs, hoverinfo = "text",
                      colors = c("grey80", color_pal) 
      ) %>%
        plotly::layout(dragmode = "lasso") %>%
        plotly::config(
          displaylogo = FALSE,
          edits = list(
            shapePosition = TRUE,
            annotation = TRUE
          )
        )

      
    })
 
  })
}

umapUi_save <- function(id){
  ns <- NS(id)
  tagList(
    shiny::plotOutput(ns("cluster_plot"))
  )
}

umapServer_save <- function(id, df = df, colour_var){
  # if (!is.reactive(clusters)){
  #   stop("Clusters should be a reactive variable.")
  # }
  # 
  # if (is.reactive(df)){
  #   stop("Input dataframe should not be reactive.")
  # }
  moduleServer(id, function(input, output, session){
    shiny::renderPlot({
      
      # cluster_pal <- gg_color_hue(length(unique(clusters())))
      
      df %>%
        dplyr::mutate(topics = as.factor(colour_var())) %>%
        ggplot2::ggplot(aes(x = v1, y = v2, colour = topics)) +
        ggplot2::geom_point() +
        # scale_color_manual(values = cluster_pal) +
        ggplot2::theme_bw()
    })
  })
}
