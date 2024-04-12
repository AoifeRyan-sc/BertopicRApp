#' UMAP Ui Server Function
#'
#' @param id parameter for shiny identification
#' @param df reactive dataframe containing docs and embedding info 
#' @param colour_var reactive list of groups vorresponding to docs in df that is the colour var in the umap
#'
#' @noRd
#' 
createUmap <- function(id, df = df, colour_var){
  # if (!is.reactive(clusters)){
  #   stop("Clusters should be a reactive variable.")
  # }
  # 
  # if (is.reactive(df)){
  #   stop("Input dataframe should not be reactive.")
  # }
  # shiny::moduleServer(id, function(input, output, session){
    
    # plotly::renderPlotly({

    if (-1 %in% colour_var()){
        colour_pal <- c("grey80", pals::stepped2(length(unique(colour_var())) - 1))
      } else{
        colour_pal <- pals::stepped2(length(unique(colour_var())))
      }
      

      df() %>% dplyr::mutate(topics = as.factor(colour_var())) %>%
      plotly::plot_ly(x = ~v1,
                      y = ~v2,
                      color = ~topics,
                      customdata = ~rowid,
                      type = "scatter", mode = "markers",
                      text = ~docs, hoverinfo = "text",
                      colors = colour_pal
      ) %>%
        plotly::layout(dragmode = "lasso") %>%
        plotly::config(
          displaylogo = FALSE,
          edits = list(
            shapePosition = TRUE,
            annotation = TRUE
          )
        )
      
      # plotly::event_register(p, "plotly_selected")
      
      # p
    # })

  # })
}

