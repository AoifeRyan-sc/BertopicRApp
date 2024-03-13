df <- data


ui <- fluidPage(
  
  # theme = shinytheme("readable"),
  # Navbar page
  # shinyjs::useShinyjs(),
  navbarPage(
    "BertopicR",
    # Tab panel for clustering ----
    tabPanel("Clustering",
             # clustering_ui("clustering_panel")

             # Title panel
             titlePanel("Clustering"),

             # sidebarLayout(
               # sidebarPanel(
                 clustering_ui("clustering_panel")
                 # ),
               # mainPanel(
               #   plotOutput("cluster_plot")
               # )
             # )
             
    ), # tabPanel
    
    # Tab panel for exploring the model ----
    tabPanel("Explore the Model", 
             # tableOutput("topic_overview")),
             # verbatimTextOutput("test")),
             tableOutput("test")),
    
    # Outlier manipulation ----
    tabPanel("Outlier Manipulation", 
             titlePanel("Outlier Manipulation"),
             
             outlier_ui("outlier_ui")
    )
  ) # navbarPage
  
  # end ----
) # fluidPage

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # clustering ----

  clustering_server("clustering_panel", df = df)
  
  
  # modelling ----
  model <- shiny::eventReactive(input$do_modelling, {
    bt_compile_model(embedding_model = BertopicR::bt_empty_embedder(),
                     reduction_model = BertopicR::bt_empty_reducer(),
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
