library(shiny)
library(ggplot2)
library(dplyr)

# I think for this we will need to input embeddings and (various options?) for reduced 
# embeddings and we can adjust clustering parameters in the app.

df <- BertopicR::test_data

# if I keep this it definitely needs to move somewhere else
# gg_color_hue <- function(n) {
#   hues = seq(15, 375, length = n)
#   pal <- c("grey80", hcl(h = hues, l = 65, c = 100)[1:n-1])
#   return(pal)
# }

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("cerulean"),
  # Navbar page
  navbarPage(
    "BertopicR",
    # Tab panel for clustering
    tabPanel("Clustering",
             
             # Title panel
             titlePanel("HDBSCAN Clustering"),
             
             # Sidebar layout
             sidebarLayout(
               # Sidebar panel with inputs
               sidebarPanel(
                 sliderInput("min_cluster_size",
                             "Minimum cluster size:",
                             min = 2,
                             max = nrow(df)/2, # arbitrarily setting this max to be half the data
                             value = 10), # arbitrarily setting the default to be 20 as in Python library
               sliderInput("min_sample_size",
                           "Minimum number of samples:",
                           min = 1,
                           max = 10, # this will be updated in server based on min_cluster_size
                           value = 1) # arbitrarily setting default in the server to be 0.5 min_cluster_size
             ),
               
               
               
               # Main panel with the plot
               mainPanel(
                 plotOutput("hdbscan_plot")
               )
             ), # sidebarLayout
             
             titlePanel("K-Means Clustering"),
             
             # Sidebar layout
             sidebarLayout(
               # Sidebar panel with inputs
               sidebarPanel(
                 numericInput("n_clusters",
                             "Number of Clusters:",
                             value = 10)
               ),
               # Main panel with the plot
               mainPanel(
                 plotOutput("kmeans_plot")
               )
             ) 
    ), # tabPanel
    tabPanel("Navbar2", "this is a test"),
    tabPanel("Navbar3", "this is another test")
  ) # navbarPage
) # fluidPage

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # hdbscan ----
  
  min_cluster <- reactive(input$min_cluster_size)
  min_samples <- reactive(input$min_sample_size)
  
  observeEvent(min_cluster(), {
    updateSliderInput(inputId = "min_sample_size", max = min_cluster(), value = min_cluster()*0.5)
  })
  
  output$hdbscan_plot <- renderPlot({
    
    hdbscan_clusterer <- bt_make_clusterer_hdbscan(min_cluster_size = min_cluster(), min_samples = min_samples())
    hdbscan_clusters <- bt_do_clustering(hdbscan_clusterer, df$reduced_embeddings)
    
    hdbscan_pal <- gg_color_hue(length(unique(hdbscan_clusters)))
    
    df %>%
      mutate(topics = as.factor(hdbscan_clusters)) %>%
      ggplot(aes(x = v1, y = v2, colour = topics)) +
      geom_point() +
      scale_color_manual(values = hdbscan_pal) +
      theme_minimal()
  })
  
  # kmeans ----
  num_clusters <- reactive(input$n_clusters)
  
  output$kmeans_plot <- renderPlot({
    
    kmeans_clusterer <- bt_make_clusterer_kmeans(n_clusters = num_clusters())
    kmeans_clusters <- bt_do_clustering(kmeans_clusterer, df$reduced_embeddings)
    
    df %>%
      mutate(topics = as.factor(kmeans_clusters)) %>%
      ggplot(aes(x = v1, y = v2, colour = topics)) +
      geom_point() +
      theme_minimal()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
