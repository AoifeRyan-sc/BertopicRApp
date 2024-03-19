df <- data
# embedder <- bt_make_embedder_st("all-miniLM-L6-v2")


ui <- fluidPage(
  
  theme = shinythemes::shinytheme("readable"),
  shinyjs::useShinyjs(),
  navbarPage(
    "BertopicR",
    tabPanel("test",
             shiny::fluidRow(
               shiny::verbatimTextOutput("test123")
             )
    ),
    tabPanel("Clustering",
             titlePanel("Clustering"),
             clusteringUi("clustering_panel")
    ), # tabPanel
    
    tabPanel("Explore the Model", 
             modelExploreUi("explore_model_panel")
             ),
    
    tabPanel("Outlier Manipulation", 
             titlePanel("Outlier Manipulation"),
             
             outlierUi("outlier_panel")
    )
  ) # navbarPage
  
) # fluidPage

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  clustering_output <- clusteringServer("clustering_panel", df = df)
  clusters <- clustering_output$clusters
  model <- clustering_output$model
  
  modelExploreServer("explore_model_panel", model = model, df= df)
  outlierServer("outlier_panel", df = df, model = model, clusters = clusters, embedder = embedder)
  
}

# Run the application 
shinyApp(ui = ui, server = server)
