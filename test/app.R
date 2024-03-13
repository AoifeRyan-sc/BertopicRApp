library(shiny)
library(shinyjs)
library(ggplot2)
library(dplyr)
library(BertopicR)

# I think for this we will need to input embeddings and (various options?) for reduced 
# embeddings and we can adjust clustering parameters in the app.

# df <- readRDS("test_data.rds")

# if I keep this it definitely needs to move somewhere else
# gg_color_hue <- function(n) {
#   hues = seq(15, 375, length = n)
#   pal <- c("grey80", hcl(h = hues, l = 65, c = 100)[1:n-1])
#   return(pal)
# }
df <- data
# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinythemes::shinytheme("readable"),
                # Navbar page
                shinyjs::useShinyjs(),
                navbarPage(
                  "BertopicR",
                  # Tab panel for clustering ----
                  tabPanel("Clustering",
                           
                           # Title panel
                           titlePanel("Clustering"),
                           
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
                                 sliderInput("min_cluster_size", 
                                             "Minimum cluster size:",
                                             min = 2, max = nrow(df)/2, value = 20), # arbitrarily setting the defaults
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
                               verbatimTextOutput("complete_message")
                             ),
                             
                             # Main panel with the plot
                             mainPanel(
                               plotOutput("cluster_plot")
                             )
                           ) # sidebarLayout,
                           
                  ), # tabPanel
                  
                  # Tab panel for exploring the model ----
                  tabPanel("Explore the Model", 
                           # tableOutput("topic_overview")),
                           # verbatimTextOutput("test")),
                           tableOutput("test")),
                  
                  # Outlier manipulation ----
                  tabPanel("Outlier Manipulation", 
                           titlePanel("Outlier Manipulation"),
                           
                           sidebarLayout(
                             sidebarPanel(
                               selectInput("outlier_method", "Outlier Reduction Method", 
                                           choices = c("c-tf-idf",
                                                       "embeddings",
                                                       "tokenset similarity")),
                               sliderInput("outlier_threshold",
                                           "Threshold:",
                                           min = 0,
                                           max = 1,
                                           value = 0.3)
                             ),
                             
                             # Show a plot of the generated distribution
                             mainPanel(
                               plotOutput("outlier_plot")
                             )
                           )
                  )
                ) # navbarPage
                
                # end ----
) # fluidPage


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # clustering ----
  
  min_cluster <- reactive(input$min_cluster_size)
  min_samples <- reactive(input$min_sample_size)
  num_clusters <- reactive(input$n_clusters)
  select_method <- reactive(input$hdbscan_cluster_selection)
  hdb_metric <- reactive(input$hdbscan_metric)
  
  clusterer <- reactive({
    if (input$cluster_method == "HDBSCAN"){
      bt_make_clusterer_hdbscan(min_cluster_size = min_cluster(), min_samples = min_samples(), cluster_selection_method = select_method(), metric = hdb_metric())
    } else if (input$cluster_method == "K-Means"){
      bt_make_clusterer_kmeans(n_clusters = num_clusters())
    }
  })
  
  clusters <- reactive(bt_do_clustering(clusterer(), df$reduced_embeddings))
  
  observeEvent(min_cluster(), {
    updateSliderInput(inputId = "min_sample_size", max = min_cluster(), value = min_cluster()*0.5)
  })
  
  output$cluster_plot <- renderPlot({
    
    cluster_pal <- metafolio::gg_color_hue(length(unique(clusters())))
    
    df %>%
      mutate(topics = as.factor(clusters())) %>%
      ggplot(aes(x = v1, y = v2, colour = topics)) +
      geom_point() +
      scale_color_manual(values = cluster_pal) +
      theme_bw()
  })
  
  
  
  # modelling ----
  model <- shiny::eventReactive(input$do_modelling, {
    bt_compile_model(embedding_model = bt_empty_embedder(),
                     reduction_model = bt_empty_reducer(),
                     clustering_model = clusterer())
  })
  
  observeEvent(input$do_modelling, { #maybe this should just be observe?
    bt_fit_model(model = model(), documents = df$docs, embeddings = df$reduced_embeddings)
  })
  
  
  output$complete_message <- renderPrint({
    if (input$do_modelling) { 
      isolate("Model generated with paramters...")# NEED TO POPULATE THIS
    } else {
      return("No model generated.")
    }
  })
  
  # disable buttons on modelling -----
  observeEvent(input$do_modelling, { 
    shinyjs::disable("min_cluster_size")
  })
  
  observeEvent(input$do_modelling, {
    shinyjs::disable("min_sample_size")
  })
  
  observeEvent(input$do_modelling, {
    shinyjs::disable("n_clustsers")
  })
  
  observeEvent(input$do_modelling, {
    shinyjs::disable("hdbscan_metric")
  })
  
  observeEvent(input$do_modelling, {
    shinyjs::disable("hdbscan_cluster_selection")
  })
  
  observeEvent(input$do_modelling, {
    shinyjs::disable("cluster_method")
  })
  
  # reset button ----
  # model <- shiny::eventReactive(input$reset_model, {
  #   rm(model)
  # })
  # 
  # observeEvent(input$do_modelling, { #maybe this should just be observe?
  #   bt_fit_model(model = model(), documents = df$docs, embeddings = df$reduced_embeddings)
  # })
  
  
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
  
  method <- reactive(input$outlier_method)
  threshold <- reactive(input$outlier_threshold)
  
  
  output$outlier_plot <- renderPlot({
    
    if (method() == "c-tf-idf"){
      outliers <- bt_outliers_ctfidf(fitted_model = model(),
                                     documents = df$docs,
                                     topics = hdbscan_clusters(), # THIS NEEDS TO CHANGE WHEN I INTEGRATE KMEANS
                                     threshold = threshold())
    } else if (method() == "embeddings"){
      outliers <- bt_outliers_embeddings(fitted_model = model(),
                                         documents = df$docs,
                                         topics = hdbscan_clusters(), # THIS NEEDS TO CHANGE WHEN I INTEGRATE KMEANS
                                         embeddings = embeddings,
                                         embedding_model = embedder,
                                         threshold = threshold())
    } else if (method() == "tokenset similarity"){
      outliers <- bt_outliers_tokenset_similarity(fitted_model = model(),
                                                  documents = df$docs,
                                                  topics = hdbscan_clusters(), # THIS NEEDS TO CHANGE WHEN I INTEGRATE KMEANS
                                                  threshold = threshold())
    }
    
    colour_pal <- gg_color_hue(length(unique(outliers$current_topics)))
    
    df %>%
      mutate(new_topics = as.factor(outliers$new_topics)) %>%
      ggplot(aes(x = v1, y = v2, colour = new_topics)) +
      geom_point() +
      scale_color_manual(values = colour_pal)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
