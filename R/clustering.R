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
      shiny::tabsetPanel(
        id = ns("data_prep_panel"),
        shiny::tabPanel(
          "Upload Data",
          shiny::br(),
          shiny::fileInput(
            ns("data_upload"), "Upload your data", 
            accept = c(".xlsx", ".csv", ".tsv", ".rds"), multiple = FALSE),
          shiny::uiOutput(ns("data_upload_error_message")),
          shiny::verbatimTextOutput(ns("reduced_embeddings_sample")),
          value = "upload"
          ),
        shiny::tabPanel(
          "Reduce Embeddings",
          shiny::br(),
          reducingUi(ns("reduction_ui")),
          value = "reduce"
        ),
        shiny::tabPanel(
          "Cluster",
          shiny::br(),
          modellingUi(ns("modelling_selection")),
          value = "cluster"
          )
      ) 
    ),
  shiny::mainPanel(
    shiny::conditionalPanel(
      condition = "input.data_prep_panel == 'upload'", ns = ns,
      DT::dataTableOutput(ns("uploaded_data")),
    ),
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

# clusteringServer <- function(id, df = df){
clusteringServer <- function(id){
  
  shiny::moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    df <- shiny::reactive({
      shiny::req(input$data_upload)

      ext <- tools::file_ext(input$data_upload$name)
      df <- switch(ext,
                   csv = readr::read_csv(input$data_upload$datapath),
                   tsv = vroom::vroom(input$data_upload$datapath, delim = "\t"),
                   xlsx = readxl::read_xlsx(input$data_upload$datapath),
                   rds = readRDS(input$data_upload$datapath),
                   shiny::validate("Invalid file; Please upload a .xlsx, .rds, .rda or .csv file")
      ) %>%
        dplyr::mutate(rowid = dplyr::row_number())

    })
    
    # df <- shiny::reactive({
    # # data %>%
    # # dplyr::select(-reduced_embeddings) %>%
    # # mutate(rowid = row_number()
    # # data <- readRDS("testing_data/data.rda")
    #   data_test %>%
    #     dplyr::mutate(rowid = dplyr::row_number())# I NEED TO REMOVE THIS AND USE THE ABOVE
    # })
    
    output$data_upload_error_message <- shiny::renderUI({
      
      required_cols <- c("docs", "embeddings", "v1", "v2")  # Add your required column names here
      missing_cols <- setdiff(required_cols, colnames(df()))

      if (length(missing_cols) > 0) {
          shiny::tagList(
            shiny::p(shiny::HTML(paste("<b>Error:</b> Required columns missing from data:", paste(missing_cols, collapse = ", "))))
        )
      }
    })
    
    output$uploaded_data <- DT::renderDataTable({
      printdf <- df() %>% dplyr::select(-embeddings)
      printdf
    })
  
    
    # I NEED TO REMOVE THIS
    # reduced_embeddings <- shiny::reactive({
    #   df()$reduced_embeddings
    # })
    
    # I NEED TO REACTIVATE THIS IN THE FINAL VERSION
    reduced_embeddings <- shiny::reactiveVal(NULL, label = "reduced_embeddings_initial_val")
    reduced_embeddings <- reducingServer("reduction_ui", df = df)
    output$reduced_embeddings_sample <- renderPrint({
      reduced_embeddings()
    })
    
    modelling_outputs <- modellingServer("modelling_selection", df = df, reduced_embeddings = reduced_embeddings)
    
    clusters <- modelling_outputs$clusters
    model <- modelling_outputs$model
    cluster_model <- modelling_outputs$cluster_model

    output$cluster_plot_display <- shiny::renderUI({
      if(!is.null(reduced_embeddings())){
       plotly::plotlyOutput(ns("cluster_plot"))
        print("is not null reduced embeddings")
        print(reduced_embeddings())
        
      } else {
        print("is null reduced embeddings")
        shiny::tagList(
          shiny::h4("Warning: Embeddings have not been reduced."),
          shiny::p("To reduce embedding, go to the `Reduce Embeddings` tab, choose your desired parameters, and click `Reduce`.")
        )
        }
    }) # conditional display 

    output$cluster_plot <- plotly::renderPlotly({
      p <- createUmap("umap_clustering", df = df, colour_var = clusters,
                      title = "UMAP of document embeddings: Cluster investigation") # can remove the id here
      plotly::event_register(p, "plotly_selected")
      p
    })
    
    
    display_data <- shiny::reactive({
      selected <- plotly::event_data("plotly_selected")
      df_temp <- df() %>% dplyr::select(-c(embeddings, v1, v2))
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
         df = df)

  })
}






