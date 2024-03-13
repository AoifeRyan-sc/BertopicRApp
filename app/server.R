library(shiny)
library(shinyjs)
library(ggplot2)
library(dplyr)
library(BertopicR)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
# clustering_server(input, output, session)
  clustering_server("clustering_page")

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

  # outlier_server(input, output, session)

 
}

# Run the application 
# shinyApp(ui = ui, server = server)
