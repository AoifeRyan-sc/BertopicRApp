clusteringUploadUi <- function(id){
  ns <- NS(id)
  
  shiny::tagList(
    shiny::fileInput(
      ns("data_upload"), "Upload your data",
      accept = c(".xlsx", ".csv", ".tsv", ".rds"), multiple = FALSE),
    shiny::uiOutput(ns("data_upload_error_message")),
    shiny::verbatimTextOutput(ns("reduced_embeddings_sample"))
  )
}

clusteringUploadServer <- function(id, r){
  shiny::moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    r$df <- shiny::reactive({
      shiny::req(input$data_upload)
      
      ext <- tools::file_ext(input$data_upload$name)
      df <- switch(ext,
                   csv = readr::read_csv(input$data_upload$datapath),
                   tsv = vroom::vroom(input$data_upload$datapath, delim = "\t"),
                   xlsx = readxl::read_xlsx(input$data_upload$datapath),
                   rds = readRDS(input$data_upload$datapath),
      ) %>%
        dplyr::mutate(rowid = dplyr::row_number(), .before = 1)
      
    })
    
    output$data_upload_error_message <- shiny::renderUI({
      
      required_cols <- c("docs", "embeddings", "v1", "v2")  # Add your required column names here
      missing_cols <- setdiff(required_cols, colnames(r$df()))
      
      if (length(missing_cols) > 0) {
        shiny::tagList(
          shiny::p(shiny::HTML(paste("<b>Error:</b> Required columns missing from data:", paste(missing_cols, collapse = ", "))))
        )
      }
    })
    
    shiny::observeEvent(input$data_upload, {
      shinyjs::reset("reduced_embeddings_upload")
      r$reduced_embeddings <- NULL 
    })
    
    
  })
}

clusteringReduceUi <- function(id){
  ns <- NS(id)
  
  shiny::tagList(
    shiny::radioButtons(ns("load_or_reduce_embeddings"),
                        "Do you want to load pre-calculate reduced embeddings or do it here?",
                        choices = c("Load in reduced embeddings",
                                    "Calculate in app")),
    shiny::conditionalPanel(
      condition = "input.load_or_reduce_embeddings == 'Load in reduced embeddings'", ns = ns,
      shiny::fileInput(ns("reduced_embeddings_upload"), "Upload Reduced Embeddings",
                       accept = c(".xlsx", ".csv", ".tsv", ".rds"), multiple = FALSE)
    ),
    shiny::conditionalPanel(
      condition = "input.load_or_reduce_embeddings == 'Calculate in app'", ns = ns,
      reducingCalcUi(ns("reduction_ui"))
    )
  )
}

clusteringReduceServer <- function(id, r){
  shiny::moduleServer(id, function(input, output, session){
    
    ns <- session$ns
    
    reduced_embeddings_calculated <- reducingCalcServer("reduction_ui", df = df)
    reduced_embeddings_loaded <- shiny::reactive({
      shiny::req(input$reduced_embeddings_upload)
      
      ext <- tools::file_ext(input$reduced_embeddings_upload$name)
      r$reduced_embeddings <- switch(ext,
                                   csv = readr::read_csv(input$reduced_embeddings_upload$datapath, show_col_types = FALSE),
                                   tsv = vroom::vroom(input$reduced_embeddings_upload$datapath, delim = "\t"),
                                   xlsx = readxl::read_xlsx(input$reduced_embeddings_upload$datapath),
                                   rds = readRDS(input$reduced_embeddings_upload$datapath),
                                   shiny::validate("Invalid file; Please upload a .xlsx, .rds, or .csv file")
      )
    })
    
    # r <- shiny::reactiveValues(reduced_embeddings = NULL)
    
    observe({
      r$reduced_embeddings <- 
        if (input$load_or_reduce_embeddings == "Calculate in app"){
          reduced_embeddings_calculated()
        } else {
          reduced_embeddings_loaded()
        }
    })
    
    # modelling_outputs <- modellingServer("modelling_selection", df = df, reduced_embeddings = reduced_embeddings)
    # modellingServer("modelling_selection", r)
    
  })
}

clusteringMainPanelUi <- function(id){
  ns <- NS(id)
  
  shiny::tagList(
    shiny::conditionalPanel(
      condition = "input.clustering_main_panel-data_prep_panel == 'upload'", ns = ns,
      DT::dataTableOutput(ns("uploaded_data")),
    ),
    shiny::uiOutput(ns("cluster_plot_display")),
    DT::dataTableOutput(ns("selected_data_df")),
    shiny::downloadButton(ns("data_download_clustering"), label = "Download Data Table")
  )
}

clusteringMainPanelServer <- function(id, r){
  shiny::moduleServer(id, function(input, output, session){
    ns <- session$ns 
    
    output$uploaded_data <- DT::renderDataTable({
      printdf <- r$df() %>% dplyr::select(-embeddings)
      printdf
    })
    
    output$cluster_plot_display <- shiny::renderUI({
      if (!is.null(r$df())) {
        plotly::plotlyOutput(ns("cluster_plot"))
        
      } else {
        shiny::tagList(
          shiny::h4("Warning: Embeddings have not been reduced."),
          shiny::p("To reduce embedding, go to the `Reduce Embeddings` tab, choose your desired parameters, and click `Reduce`.")
        )
      }
    }) 

    output$cluster_plot <- plotly::renderPlotly({
      shiny::req(is.array(r$reduced_embeddings) | is.data.frame(r$reduced_embeddings))
      print(r$clusters)
      message("time for plots")
      createUmap("umap_clustering", df = r$df, colour_var = r$clusters,
                 title = "UMAP of document embeddings: Cluster investigation") # can remove the id here
    })

    display_data <- shiny::reactive({
      # shiny::req(reduced_embeddings()) # delaying this to avoid error message
      shiny::req(is.array(r$reduced_embeddings) | is.data.frame(r$reduced_embeddings))
      selected <- plotly::event_data(event = "plotly_selected", source = "umap_clustering")
      df_temp <- r$df() %>% dplyr::select(-c(embeddings, v1, v2))
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
    
    
  })
}

