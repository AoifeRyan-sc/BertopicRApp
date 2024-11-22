#' Title
#'
#' @param id  parameter for shiny identification
#' @param id_num  parameter for identification in case where function run multiple times
#'
#' @keywords internal
#'
# # reducingParamsUi <- function(id, id_num){
#   ns <- shiny::NS(id)
#   shiny::tagList(
#     shiny::div(
#       class = "row",
#       shiny::div(
#         class = "col-md-6",
#         shiny::numericInput(ns(paste0("n_neighbours", id_num)), "No. of Nearest Neighbours", value = 15)
#       ),
#       shiny::div(
#         class= "col-md-6",
#         shiny::numericInput(ns(paste0("n_components", id_num)), "No. of Dimensions", value = 5)
#       ),
#       style = "margin-top: 30px"
#     ),
#     shiny::div(
#       class = "row",
#       shiny::div(
#         class = "col-md-7",
#         shiny::numericInput(ns(paste0("min_dist", id_num)), "Min Distance Between Points", value = 0)
#       ),
#       shiny::div(
#         class= "col-md-5",
#         shiny::selectInput(ns(paste0("reducing_metric", id_num)), "Distance Metric", choices = c("cosine", "euclidean")) # expand this
#       )
#     ),
#     shiny::actionButton(ns(paste0("do_reducing_option", id_num)), label = shiny::HTML("<strong>Reduce</strong>"), class = "btn-succes", 
#                         width = "100%", style = "margin-bottom: 30px; border-width: 2px;")
#   )
# }

embed_docs <- function(doc, embedding_model = "all-MiniLM-L6-v2") {
  
  api_token <- Sys.getenv("HUGGINGFACE_API_KEY")
  base_hf_st_url <- "https://api-inference.huggingface.co/pipeline/feature-extraction/sentence-transformers/"
  endpoint_hf_st <- embedding_model
  
  # create the request with retry mechanism
  response <- httr2::request(base_hf_st_url) %>%
    httr2::req_url_path_append(endpoint_hf_st) %>% 
    httr2::req_headers(
      "Authorization" = paste("Bearer", api_token)
      # "Ret"
    ) %>%
    httr2::req_body_json(doc) %>%
    httr2::req_retry(max_tries = 10, # select a maximum of 10 retries
                     backoff = ~ 5, # constant 2s delay after failure
                     is_transient = \(resp) httr2::resp_status(resp) %in% c(429, 500, 503)) %>% # specify common transient errors
    httr2::req_perform()
  
  # if response status isn't 200(OK), stop function
  if (httr2::resp_status(response) != 200) {
    # if transient error met, print message
    if (httr2::resp_status(response) %in% c(429, 500, 503)) {
      message("Max retries reached and timed out, please try again or check server-side connection.")
    }
    stop("Request failed with status: ", httr2::resp_status(response))
  }
  
  # return cosine similarity matrix
  data <- httr2::resp_body_json(response) %>% 
    unlist() %>% 
    as.data.frame() %>%
    t() %>%
    as.data.frame()
  
  return(data)
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

#' UMAP Ui Server Function
#'
#' @param id parameter for shiny identification
#' @param df reactive dataframe containing docs and embedding info 
#' @param colour_var reactive list of groups vorresponding to docs in df that is the colour var in the umap
#'
#' @noRd
#' 
createUmap <- function(source_id, df = df, colour_var, title){
  full_palette <- pals::stepped2(20)
  n_clusters <- length(unique(colour_var))
  indices <- round(seq(1, 20, length.out = n_clusters))

  if (-1 %in% colour_var){
    colour_pal <- c("grey80", full_palette[indices - 1])
  } else{
    colour_pal <- full_palette[indices]
  }
  
  p <- df %>% dplyr::mutate(topics = as.factor(colour_var))  %>%
    plotly::plot_ly(x = ~v1,
                    y = ~v2,
                    color = ~topics,
                    customdata = ~rowid,
                    type = "scatter",
                    mode = "markers",
                    text = ~docs,
                    hoverinfo = "text",
                    colors = colour_pal,
                    marker = list(opacity = 0.7),  # Adjust marker size and opacity
                    source = source_id) %>%
    plotly::layout(dragmode = "lasso",
                   title = title,
                   xaxis = list(title = "V1",
                                showline = FALSE,
                                linecolor = "grey80",
                                mirror = TRUE,
                                linewidth = 1),
                   yaxis = list(title = "V2",
                                showline = FALSE,
                                zeroline = FALSE
                                # linecolor = "grey80",
                                # mirror = TRUE,
                                # linewidth = 1
                   ), # can remove all of this if not showing line
                   showlegend = TRUE,
                   legend = list(title = "Topics")) %>%
    plotly::config(
      displaylogo = FALSE,
      edits = list(
        shapePosition = TRUE,
        annotation = TRUE
      )
    )

  
  return(p)
  
}


title_file <- function(file_name, format) {
  stopifnot(is.character(file_name),
            is.character(format),
            grepl("^\\.", format))
  
  return(paste0(file_name, "_", format(Sys.time(), "%d-%m-%Y"), format))
}



progress_annimation <- function(processing_value_check, complete_value_check, processing_text, complete_text){
  if (complete_value_check == "happening"){
    return(
      htmltools::tagList(
      htmltools::tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
      ),
      htmltools::div(
        class = "reducing-embeddings",
        span(class = "timer-emoji", "⏳"),
        span(class = "reducing-text", processing_text)
      ))
    )
  } else if (processing_value_check == "finished") {
    return(
      htmltools::tagList(
        htmltools::tags$head(
          htmltools::tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
        ),
        htmltools::div(
          class = "reduced-embeddings",
          htmltools::span(class = "check-emoji", "✅"),
          htmltools::span(class = "reducing-text", complete_text)
        ))
    )
  }
}

progress_extract <- function(message_reactive){
  if (!is.null(message_reactive)) {
    shiny::div({
      if (stringr::str_detect(message_reactive, "Epochs completed")) {
        progress_regex <- "Epochs completed:(.*?)\\]"
        extracted_messages <- stringr::str_extract_all(message_reactive, progress_regex)[[1]]
        
        return(htmltools::div(paste(extracted_messages, collapse = "\n")))
      }
    })
  }
}