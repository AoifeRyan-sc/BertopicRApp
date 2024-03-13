#' Ui specs for clustering tab
#'
#' @param id 
#'
#' @return
#' @export
#'
#' @examples
clustering_ui <- function(id) {
  
  ns <- NS(id)
  # Sidebar layout
  # tagList(
    sidebarLayout(
    # Sidebar panel with inputs
    sidebarPanel(
        selectInput(ns("cluster_method"), "Clustering Method", choices = c("HDBSCAN", "K-Means")),
        conditionalPanel(
          condition = "input.cluster_method == 'K-Means'", ns = ns,
          numericInput(ns("n_clusters"), "Number of Clusters", value = 10)
        ),
        conditionalPanel(
          condition = "input.cluster_method == 'HDBSCAN'", ns = ns,
          sliderInput(ns("min_cluster_size"), "Minimum cluster size:",
                      # min = 2, max = nrow(df)/2, value = 20
                      min = 2, max = 20, value = 20), # arbitrarily setting the defaults
          sliderInput(ns("min_sample_size"), "Minimum number of samples:",
                      min = 1, max = 10, value = 1), # these values update as defined in the server
          # radioButtons("hdbscan_metric", "Metric", choices = c("cosine", "euclidean")),
          selectInput(ns("hdbscan_metric"), "Metric", choices = c(
            # "cosine",
            "braycurtis", "canberra", "chebyshev", "cityblock", "correlation",  "dice", "euclidean", "hamming", "jaccard", "jensenshannon", "kulczynski1", "mahalanobis", "matching", "minkowski", "rogerstanimoto", "russellrao", "seuclidean", "sokalmichener", "sokalsneath", "sqeuclidean", "yule")),
          radioButtons(ns("hdbscan_cluster_selection"), "Cluster Selection Method", choices = c("eom", "leaf"))
        ),
        actionButton(ns("do_modelling"), "Model", class = "btn-succes"),
        actionButton(ns("reset_model"), "Reset", classs = "btn-danger"),
        verbatimTextOutput(ns("complete_message")),
        verbatimTextOutput(ns("selection_method"))
      ),

  # Main panel with the plot
  mainPanel(
    plotOutput(ns("cluster_plot"))
  )
  ) # sidebarLayout
  # )
  
}

clustering_ui_legac <- function(id) {
  # Sidebar layout
  sidebarLayout(
    # Sidebar panel with inputs
    sidebarPanel(
      selectInput("cluster_method", "Clustering Method", choices = c("HDBSCAN", "K-Means")),
      conditionalPanel(
        condition = "input.cluster_method == 'K-Means'",
        numericInput("n_clusters", "Number of Clusters", value = 10)
      ),
      conditionalPanel(
        condition = "input.cluster_method == 'HDBSCAN'",
        sliderInput("min_cluster_size", "Minimum cluster size:",
                    # min = 2, max = nrow(df)/2, value = 20
                    min = 2, max = 20, value = 20), # arbitrarily setting the defaults
        sliderInput("min_sample_size", "Minimum number of samples:",
                    min = 1, max = 10, value = 1), # these values update as defined in the server
        # radioButtons("hdbscan_metric", "Metric", choices = c("cosine", "euclidean")),
        selectInput("hdbscan_metric", "Metric", choices = c(
          # "cosine",
          "braycurtis", "canberra", "chebyshev", "cityblock", "correlation",  "dice", "euclidean", "hamming", "jaccard", "jensenshannon", "kulczynski1", "mahalanobis", "matching", "minkowski", "rogerstanimoto", "russellrao", "seuclidean", "sokalmichener", "sokalsneath", "sqeuclidean", "yule")),
        radioButtons("hdbscan_cluster_selection", "Cluster Selection Method", choices = c("eom", "leaf"))
      ),
      actionButton("do_modelling", "Model", class = "btn-succes"),
      actionButton("reset_model", "Reset", classs = "btn-danger"),
      verbatimTextOutput("complete_message"),
      verbatimTextOutput("selection_method")
    ),
    
    # Main panel with the plot
    mainPanel(
      plotOutput("cluster_plot")
    )
  ) # sidebarLayout
}


#' Title
#'
#' @param id
#'
#' @return
#' @export
#'
#' @examples
clustering_server_legacy <- function(id, df = df){
  
  
  moduleServer(id, function(input, output , session){
    ns <- session$ns
    # clustering ----
    
    min_cluster <- reactive(input$min_cluster_size)
    min_samples <- reactive(input$min_sample_size)
    num_clusters <- reactive(input$n_clusters)
    select_method <- reactive(input$hdbscan_cluster_selection)
    hdb_metric <- reactive(input$hdbscan_metric)
    
    clusterer <- reactive({
      if (input$cluster_method == "HDBSCAN"){
        BertopicR::bt_make_clusterer_hdbscan(min_cluster_size = min_cluster(), min_samples = min_samples(), cluster_selection_method = select_method(), metric = hdb_metric())
      } else if (input$cluster_method == "K-Means"){
        BertopicR::bt_make_clusterer_kmeans(n_clusters = num_clusters())
      }
    })

    clusters <- reactive(bt_do_clustering(clusterer(), df$reduced_embeddings))

    observeEvent(min_cluster(), {
      updateSliderInput(inputId = "min_sample_size", max = min_cluster(), value = min_cluster()*0.5)
    })

    output$cluster_plot <- renderPlot({

      # cluster_pal <- metafolio::gg_color_hue(length(unique(clusters())))

      df %>%
        mutate(topics = as.factor(clusters())) %>%
        ggplot(aes(x = v1, y = v2, colour = topics)) +
        geom_point() +
        # scale_color_manual(values = cluster_pal) +
        theme_bw()
    })
    
    output$selection_method <- verbatimTextOutput({
      select_method()
    })
    
  })
 
  
}


#' Title
#'
#' @param id 
#'
#' @return
#' @export
#'
#' @examples
clustering_server <- function(id, df = df){
  
  moduleServer(id, function(input, output, session){
    # ns <- session$ns
    min_cluster <- reactive(input$min_cluster_size)
    min_samples <- reactive(input$min_sample_size)
    num_clusters <- reactive(input$n_clusters)
    select_method <- reactive(input$hdbscan_cluster_selection)
    hdb_metric <- reactive(input$hdbscan_metric)
    
    clusterer <- reactive({
      if (input$cluster_method == "HDBSCAN"){
        BertopicR::bt_make_clusterer_hdbscan(min_cluster_size = min_cluster(), min_samples = min_samples(), cluster_selection_method = select_method(), metric = hdb_metric())
      } else if (input$cluster_method == "K-Means"){
        BertopicR::bt_make_clusterer_kmeans(n_clusters = num_clusters())
      }
    })
    
    clusters <- reactive(BertopicR::bt_do_clustering(clusterer(), df$reduced_embeddings))
    
    shiny::observeEvent(min_cluster(), {
      shiny::updateSliderInput(inputId = "min_sample_size", max = min_cluster(), value = min_cluster()*0.5)
    })
    
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




