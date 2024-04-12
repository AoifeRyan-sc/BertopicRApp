reducingParamsUi <- function(id, id_num){
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      class = "row",
      shiny::div(
        class = "col-md-6",
        shiny::numericInput(ns(paste0("n_neighbours", id_num)), "No. of Nearest Neighbours", value = 15)
      ),
      shiny::div(
        class= "col-md-6",
        shiny::numericInput(ns(paste0("n_components", id_num)), "No. of Dimensions", value = 5)
      ),
      style = "margin-top: 30px"
    ),
    shiny::div(
      class = "row",
      shiny::div(
        class = "col-md-7",
        shiny::numericInput(ns(paste0("min_dist", id_num)), "Min Distance Between Points", value = 0)
      ),
      shiny::div(
        class= "col-md-5",
        shiny::selectInput(ns(paste0("reducing_metric", id_num)), "Distance Metric", choices = c("cosine", "euclidean")) # expand this
      )
    ),
    shiny::actionButton(ns(paste0("do_reducing_option", id_num)), label = shiny::HTML("<strong>Reduce</strong>"), class = "btn-succes", 
                        width = "100%", style = "margin-bottom: 30px; border-width: 2px;")
  )
}

#' @title identify and visualises the textual differences between groups
#'
#' @description A function to visualize word frequency and log odds ratio for a given dataset. The function takes several arguments and returns a list containing the visualization and the underlying data.
#'
#' @details Plots terms that are more/less likely to appear in conversation across different topics.
#'
#' When using filter_by = "association" it's important to look at the top_terms_cutoff argument. The higher the value, the more terms you'll include; this can mean you include very low frequency terms. A good starting point is ~ 500-1000. If you wanted to see only the top 50 terms of the whole dataset and how they are distributed across groups, you could use top_terms_cutoff == 50.
#'
#'
#' @param df A dataframe.
#' @param topic_var The variable that contains the topic label for each post.
#' @param text_var The variable containing the text to be compared.
#' @param top_n The maximum number of words to include for each topic in the visualisation.
#' @param filter_by Whether the top_n terms per category are ordered by their association (high WLO value) or by frequency
#' @param top_terms_cutoff The maximum number of terms to select the WLOs from. Only relevant for filter_by = "association"
#' @param nrow The number of rows the plots should be displayed with.
#'
#' @return A list containing a table with weighted log-odds calculated for each word in each group, and a visualisation
#' 
#' @export
calculate_wlos_app <- function(df, topic_var, text_var = Message, top_n = 30, filter_by = c("association", "frequency"), top_terms_cutoff = 500, nrow = 4) {
  # Error handling ----
  if (!tibble::is_tibble(df) && !is.data.frame(df)) {
    stop("Input 'df' must be a tibble or a data frame.")
  }
  
  if (!rlang::has_name(df, deparse(substitute(text_var)))) {
    stop("Column specified by 'text_var = ' not found in 'df'.")
  }
  
  if (!rlang::has_name(df, deparse(substitute(topic_var)))) {
    stop("Column specified by 'topic_var = ' not found in 'df'.")
  }
  
  
  if (!(is.numeric(top_n) && top_n > 0 && top_n %% 1 == 0)) {
    stop("Parameter 'min_freq' must be a positive integer.")
  }
  
  if (!(is.numeric(top_n) && is.numeric(top_terms_cutoff) && top_terms_cutoff >= top_n)) {
    stop("Parameter top_terms_cutoff must be greater than or equal to top_n")
  }
  # End error handling ----
  
  filter_by <- match.arg(filter_by, choices = c("association", "frequency"))
  
  # Prepare tidy evaluate variables
  text_var <- rlang::enquo(text_var)
  topic_var <- rlang::enquo(topic_var)
  
  # Prepare the data for visualization
  # 1. Unnest tokens from the input text variable
  # 2. Count occurrences of each word within each topic
  # 3. Compute log odds for each word
  wlos <- df %>%
    tidytext::unnest_tokens(word, !!text_var) %>%
    dplyr::count(!!topic_var, word) %>%
    tidylo::bind_log_odds(set = !!topic_var, feature = word, n = n)
  
  if (filter_by == "association") {
    wlos <- wlos %>%
      dplyr::slice_max(order_by = n, n = top_terms_cutoff) %>%
      # Select the top_n words with the highest log odds ratio within each topic
      dplyr::slice_max(order_by = log_odds_weighted, n = top_n, by = !!topic_var, with_ties = FALSE)
  } else {
    wlos <- wlos %>%
      dplyr::slice_max(n = top_n, order_by = n, by = !!topic_var, with_ties = FALSE)
  }
  
  wlos <- wlos %>%
    dplyr::arrange(dplyr::desc(n))
  
  
  viz <- wlos %>%
    ggplot2::ggplot(ggplot2::aes(x = n, y = log_odds_weighted, label = word)) +
    # Add a horizontal line at y=0 with custom styling
    ggplot2::geom_hline(
      yintercept = 0, linetype = 2,
      color = "gray50", alpha = 0.5
    ) +
    ggrepel::geom_text_repel(
      size = 3,
      segment.size = 0.5,
      color = "black",
      bg.color = "white",
      max.overlaps = top_n,
      force = 10
    ) +
    ggplot2::geom_point(size = .4, show.legend = FALSE) +
    ggplot2::facet_wrap(c(topic_var), nrow = nrow, scales = "free") +
    ggplot2::scale_x_log10() +
    ggplot2::labs(
      x = "Word frequency",
      y = "Log odds ratio, weighted by uninformative Dirichlet prior"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      strip.background = ggplot2::element_rect(fill = "white", colour = "white"),
      strip.text = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank()
    )
  
  # Add a horizontal line at y=0 with custom styling
  return(list("viz" = viz, "view" = wlos))
}