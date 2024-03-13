df <- data
# embedder <- bt_make_embedder_st("all-miniLM-L6-v2")


ui <- fluidPage(
  
  theme = shinythemes::shinytheme("readable"),
  shinyjs::useShinyjs(),
  navbarPage(
    "BertopicR",
    tabPanel("Clustering",
             titlePanel("Clustering"),
             clusteringUi("clustering_panel")
    ), # tabPanel
    
    tabPanel("Explore the Model", 
             # tableOutput("topic_overview")),
             # verbatimTextOutput("test")),
             tableOutput("test")),
    
    tabPanel("Outlier Manipulation", 
             titlePanel("Outlier Manipulation"),
             
             outlierUi("outlier_panel")
    )
  ) # navbarPage
  
) # fluidPage

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  clustering_output <- clusteringServer("clustering_panel", df = df)
  clusters <- clustering_output$clusters
  model <- clustering_output$model
  
  
  output$complete_message <- renderPrint({
    if (input$reset_model) {
      isolate("model params")# NEED TO POPULATE THIS
    }
  })
  
  # model exploration ----
  output$topic_overview <- renderTable(
    model()$get_topic_info() %>% select(-Representative_Docs)
  )
  output$test <- renderTable(
    model()$get_topic_info() %>% select(-Representative_Docs)
  )
  # Outlier ----
  
  outlierServer("outlier_panel", df = df, model = model, clusters = clusters, embedder = embedder)
  
}

# Run the application 
shinyApp(ui = ui, server = server)
