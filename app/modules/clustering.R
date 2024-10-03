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
        # shiny::tabPanel(
        #   value = "upload",
        #   "Upload Data",
        #   shiny::br(),
        #   clusteringUploadUi(ns("clustering_upload"))
        # ),
        shiny::tabPanel(
          value = "reduce",
          "Reduce Embeddings",
          shiny::br(),
          clusteringReduceUi(ns("clustering_reduce"))
        ),
        shiny::tabPanel(
          value = "cluster",
          "Cluster",
          shiny::br(),
          modellingUi(ns("modelling_selection"))
          )
      ) 
    ),
  shiny::mainPanel(
   # clusteringMainPanelUi(ns("clustering_main_panel"))
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
    
    # clusteringUploadServer("clustering_upload", r)
    clusteringReduceServer("clustering_reduce", r)
    # clusteringMainPanelServer("clustering_main_panel", r)
    modellingServer("modelling_selection", r)
    # df <- shiny::reactive({
    #   shiny::req(input$data_upload)
    # 
    #   ext <- tools::file_ext(input$data_upload$name)
    #   df <- switch(ext,
    #                csv = readr::read_csv(input$data_upload$datapath),
    #                tsv = vroom::vroom(input$data_upload$datapath, delim = "\t"),
    #                xlsx = readxl::read_xlsx(input$data_upload$datapath),
    #                rds = readRDS(input$data_upload$datapath),
    #                shiny::validate("Invalid file; Please upload a .xlsx, .rds, or .csv file")
    #   ) %>%
    #     dplyr::mutate(rowid = dplyr::row_number(), .before = 1)
    # 
    # })
    
    # output$data_upload_error_message <- shiny::renderUI({
    #   
    #   required_cols <- c("docs", "embeddings", "v1", "v2")  # Add your required column names here
    #   missing_cols <- setdiff(required_cols, colnames(df()))
    # 
    #   if (length(missing_cols) > 0) {
    #       shiny::tagList(
    #        f shiny::p(shiny::HTML(paste("<b>Error:</b> Required columns missing from data:", paste(missing_cols, collapse = ", "))))
    #     )
    #   }
    # })
    
    # output$uploaded_data <- DT::renderDataTable({
    #   printdf <- df() %>% dplyr::select(-embeddings)
    #   printdf
    # })
    # 
    # reduced_embeddings_calculated <- reducingCalcServer("reduction_ui", df = df)
    # reduced_embeddings_loaded <- shiny::reactive({
    #   shiny::req(input$reduced_embeddings_upload)
    # 
    #         ext <- tools::file_ext(input$reduced_embeddings_upload$name)
    #         reduced_embeddings <- switch(ext,
    #                      csv = readr::read_csv(input$reduced_embeddings_upload$datapath, show_col_types = FALSE),
    #                      tsv = vroom::vroom(input$reduced_embeddings_upload$datapath, delim = "\t"),
    #                      xlsx = readxl::read_xlsx(input$reduced_embeddings_upload$datapath),
    #                      rds = readRDS(input$reduced_embeddings_upload$datapath),
    #                      shiny::validate("Invalid file; Please upload a .xlsx, .rds, or .csv file")
    #         )
    # })
    
    # r <- shiny::reactiveValues(reduced_embeddings = NULL)
    # 
    # observe({
    #   r$reduced_embeddings <- 
    #     if (input$load_or_reduce_embeddings == "Calculate in app"){
    #       reduced_embeddings_calculated()
    #     } else {
    #       reduced_embeddings_loaded()
    #     }
    # })
    
    # shiny::observeEvent(input$data_upload, {
    #   shinyjs::reset("reduced_embeddings_upload")
    #   r$reduced_embeddings <- NULL 
    # })
    # 
    ###### I HAVE JUST REMOVED THIS CODE -------
    # reduced_embeddings <- shiny::reactive({
    #   r$reduced_embeddings
    # }) # this is a lazy way of dealing with the fact that I added r as a container so that I don't have to change subsequent code
    ####################################
    # modelling_outputs <- modellingServer("modelling_selection", df = df, reduced_embeddings = reduced_embeddings)
    
    # clusters <- modelling_outputs$clusters
    # model <- modelling_outputs$model
    # cluster_model <- modelling_outputs$cluster_model

    # output$cluster_plot_display <- shiny::renderUI({
    #   if (!is.null(df())) {
    #    plotly::plotlyOutput(ns("cluster_plot"))
    #     
    #   } else {
    #     shiny::tagList(
    #       shiny::h4("Warning: Embeddings have not been reduced."),
    #       shiny::p("To reduce embedding, go to the `Reduce Embeddings` tab, choose your desired parameters, and click `Reduce`.")
    #     )
    #     }
    # }) # conditional display 
    # 

    # output$cluster_plot <- plotly::renderPlotly({
    #   shiny::req(is.array(reduced_embeddings()) | is.data.frame(reduced_embeddings()))
    #   createUmap("umap_clustering", df = df, colour_var = clusters,
    #                   title = "UMAP of document embeddings: Cluster investigation") # can remove the id here
    # })
    # 
    
    # display_data <- shiny::reactive({
    #   # shiny::req(reduced_embeddings()) # delaying this to avoid error message
    #   shiny::req(is.array(reduced_embeddings()) | is.data.frame(reduced_embeddings()))
    #   selected <- plotly::event_data(event = "plotly_selected", source = "umap_clustering")
    #   df_temp <- df() %>% dplyr::select(-c(embeddings, v1, v2))
    #   df_temp[df_temp$rowid %in% selected$customdata, ] %>%
    #     dplyr::select(-rowid)
    #   
    # })

    # output$selected_data_df <- DT::renderDataTable({
    #  display_data()
    # })
    
    # output$data_download_clustering <- shiny::downloadHandler(
    #   filename = function() {
    #     paste0("clustering_data_", format(Sys.time(), "%d-%m-%Y_%H:%M:%S"), ".csv")
    #   },
    #   content = function(file) {
    #     utils::write.csv(display_data(), file, row.names = FALSE)
    #   }
    # )

    list(clusters = reactive({r$clusters()}),
         model = reactive({r$model()}),
         cluster_model = reactive({r$cluster_model()}),
         df = reactive({r$df()})
    )

  })
}






