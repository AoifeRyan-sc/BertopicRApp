#' Ui specs for clustering tab
#'
#' @param id parameter for shiny identification
#'
#' @noRd
#' 
clusteringUi <- function(id) {

ns <- shiny::NS(id)
shiny::tagList(
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::tabsetPanel(
        shiny::tabPanel(
          "Upload Data",
          shiny::br(),
          shiny::fileInput(
            "data_upload", "Upload your data", 
            accept = c(".xlsx", ".csv", ".tsv"), multiple = FALSE)
          ),
        shiny::tabPanel(
          "Reduce Embeddings",
          shiny::br(),
          reducingUi(ns("reduction_ui"))
        ),
        shiny::tabPanel(
          "Cluster",
          shiny::br(),
          modellingUi(ns("modelling_selection")),
          )
      ) 
    ),
  shiny::mainPanel(
    shiny::uiOutput(ns("cluster_plot_display")),
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

clusteringServer <- function(id, df = df){
  
  shiny::moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    df_for_later <- shiny::reactive({
      shiny::req(input$data_upload)
      
      ext <- tools::file_ext(input$data_upload$name)
      df <- switch(ext,
                   csv = readr::read_csv(input$data_upload$datapath, skip = 2),
                   # tsv = vroom::vroom(input$data_upload$datapath, delim = "\t"),
                   xlsx = readxl::read_xlsx(input$data_upload$datapath, skip = 2),
                   shiny::validate("Invalid file; Please upload a .xlsx or .csv file")
      ) %>%
        dplyr::mutate(rowid = dplyr::row_number())
    })
    
    # I NEED TO REMOVE THIS
    reduced_embeddings <- shiny::reactive({
      df()$reduced_embeddings
    })
    
    # I NEED TO REACTIVATE THIS IN THE FINAL VERSION
    # reduced_embeddings <- reducingServer("reduction_ui", df = df)
    # output$reduced_embeddings_sample <- renderPrint({
    #   reduced_embeddings()
    # })
    
    modelling_outputs <- modellingServer("modelling_selection", df = df, reduced_embeddings = reduced_embeddings)
    
    clusters <- modelling_outputs$clusters
    model <- modelling_outputs$model
    cluster_model <- modelling_outputs$cluster_model

    output$cluster_plot_display <- shiny::renderUI({
      if(!is.null(reduced_embeddings())){
       plotly::plotlyOutput(ns("cluster_plot"))
        
      } else {
        shiny::tagList(
          shiny::h4("Warning: Embedding have not been reduced."),
          shiny::p("To reduce embedding, go to the `Reduce Embeddings` tab, choose your desired parameters, and click `Reduce`.")
        )
      }
    }) # conditional display 

    output$cluster_plot <- plotly::renderPlotly({
      p <- createUmap("umap_clustering", df = df, colour_var = clusters) # can remove the id here
      plotly::event_register(p, "plotly_selected")
      p
    })
    
    
    display_data <- shiny::reactive({
      selected <- plotly::event_data("plotly_selected")
      df_temp <- df() %>% dplyr::select(-c(reduced_embeddings, embeddings, v1, v2))
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

    list(clusters = clusters,
         model = model,
         cluster_model = cluster_model,
         df_for_later = df_for_later)

  })
}






