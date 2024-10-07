#' Ui specs for clustering tab
#'
#' @param id parameter for shiny identification
#' 
#'
#' @noRd
#' 
clusteringUi <- function(id) {

ns <- shiny::NS(id)

shiny::tagList(
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      # modellingUi(ns("modelling_selection"))
      shiny::selectInput(ns("cluster_method"), "Clustering Method", choices = c("HDBSCAN", "K-Means")),
      shiny::conditionalPanel(
        condition = "input.cluster_method == 'K-Means'", ns = ns,
        shiny::numericInput(ns("n_clusters"), "Number of Clusters", value = 10)
      ),
      shiny::conditionalPanel(
        condition = "input.cluster_method == 'HDBSCAN'", ns = ns,
        shiny::sliderInput(ns("min_cluster_size"), "Minimum cluster size:",
                           min = 2, max = 20, value = 20, round = TRUE
                           # step = ceiling((20 - 2)/5)
        ), # arbitrarily setting the defaults
        shiny::sliderInput(ns("min_sample_size"), "Minimum number of samples:",
                           min = 1, max = 10, value = 1), # these values update as defined in the server
        shiny::selectInput(ns("hdbscan_metric"), "Clustering Metric", choices = c(
          # "cosine", - Not supported by sklearn
          "euclidean", "braycurtis", "canberra", "chebyshev", "cityblock", "correlation",  "dice", "hamming", "jaccard", "jensenshannon", "kulczynski1", "mahalanobis", "matching", "minkowski", "rogerstanimoto", "russellrao", "seuclidean", "sokalmichener", "sokalsneath", "sqeuclidean", "yule")),
        shiny::radioButtons(ns("hdbscan_cluster_selection"), "Cluster Selection Method", choices = c("eom", "leaf"))
      ),
      shiny::actionButton(ns("do_modelling"), "Model", class = "btn-succes"),
      shiny::actionButton(ns("reset_model"), "Reset", classs = "btn-danger"),
      shiny::verbatimTextOutput(ns("complete_message"))

    ),
  shiny::mainPanel(
   # clusteringMainPanelUi(ns("clustering_main_panel"))
    shiny::uiOutput(ns("data_display")),
    DT::dataTableOutput(ns("selected_data_df")),
    shiny::downloadButton(ns("data_download_clustering"), label = "Download Data Table")
    )
)
)
}

#' Clustering UI Server Function
#'
#' @param id parameter for shiny identification
#' @param df reactive dataframe containing docs and embedding info 
#'
#' @noRd

# clusteringServer <- function(id, df = df){
clusteringServer <- function(id, r){
  shiny::moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    min_cluster <- shiny::reactive(input$min_cluster_size)
    min_samples <- shiny::reactive(input$min_sample_size)
    num_clusters <- shiny::reactive(input$n_clusters)
    select_method <- shiny::reactive(input$hdbscan_cluster_selection)
    hdb_metric <- shiny::reactive(input$hdbscan_metric)
    
    clusterer <- shiny::reactive({
      if (input$cluster_method == "HDBSCAN"){
        BertopicR::bt_make_clusterer_hdbscan(min_cluster_size = min_cluster(), min_samples = min_samples(), cluster_selection_method = select_method(), metric = hdb_metric())
      } else if (input$cluster_method == "K-Means"){
        BertopicR::bt_make_clusterer_kmeans(n_clusters = num_clusters())
      }
    })
    
    r$clusters <- shiny::reactive({
      print(clusterer())
      req(is.array(r$reduced_embeddings) | is.data.frame(r$reduced_embeddings))
      BertopicR::bt_do_clustering(clusterer(), r$reduced_embeddings)
    })
    
    
    output$data_display <- renderUI({ # idk why I can't get the conditional panels working with the namespacing
      if (is.data.frame(r$df) & is.null(r$reduced_embeddings)){
        DT::dataTableOutput((ns("uploaded_data")))
      } else if (!is.null(r$reduced_embeddings)){
        print("plot stuff")
        shinycssloaders::withSpinner(plotly::plotlyOutput(ns("cluster_plot")))
      }
    })
    
    output$uploaded_data <- DT::renderDataTable({
      req(is.data.frame(r$df))
      printdf <- r$df %>% dplyr::select(-embeddings)
      printdf
    })
    
    output$cluster_plot <- plotly::renderPlotly({
      shiny::req(is.array(r$reduced_embeddings2d) | is.data.frame(r$reduced_embeddings2d))
      print("plot stuff 2")
      r$df$v1 <- r$reduced_embeddings2d[,1]
      r$df$v2 <- r$reduced_embeddings2d[,2]
      createUmap("umap_clustering", df = r$df, colour_var = r$clusters(),
                 title = "UMAP of document embeddings: Cluster investigation") # can remove the id here
      
    })
    
    display_data <- shiny::reactive({
      # shiny::req(reduced_embeddings()) # delaying this to avoid error message
      shiny::req(is.array(r$reduced_embeddings) | is.data.frame(r$reduced_embeddings))
      selected <- plotly::event_data(event = "plotly_selected", source = "umap_clustering")
      df_temp <- r$df %>% dplyr::select(-c(embeddings, v1, v2))
      df_temp[df_temp$rowid %in% selected$customdata, ] %>%
        dplyr::select(-rowid)
      
    })
    
    output$selected_data_df <- DT::renderDataTable({
      display_data()
    })
    
    output$data_download_clustering <- shiny::downloadHandler(
      filename = function() {
        paste0("clustering_data_", format(Sys.time(), "%d-%m-%Y_%H:%M:%S"), ".csv")
      },
      content = function(file) {
        utils::write.csv(display_data(), file, row.names = FALSE)
      }
    )
    # clusteringMainPanelServer("clustering_main_panel", r)
    # modellingServer("modelling_selection", r)

    # list(clusters = reactive({r$clusters()}),
    #      model = reactive({r$model()}),
    #      cluster_model = reactive({r$cluster_model()}),
    #      df = reactive({r$df()})
    # )

  })
}






