library(shiny)
library(ggplot2)
library(dplyr)

# data required
# sentences <- stringr::sentences
# embedder <- bt_make_embedder_st("all-miniLM-L6-v2")
# embeddings <- bt_do_embedding(embedder, sentences)
# reducer <- bt_make_reducer_umap()
# reduced_embeddings <- bt_do_reducing(reducer, embeddings)
# model <- bt_compile_model(embedding_model = bt_empty_embedder(),
#                           reduction_model = bt_empty_reducer())
# bt_fit_model(model, sentences, reduced_embeddings)
# reducer2d <- bt_make_reducer_umap(n_components = 2L)
# embeddings2d <- bt_do_reducing(reducer2d, embeddings)
# df <- data.frame(docs = sentences, topics = model$topics_,
#                  v1 = embeddings2d[,1], v2 = embeddings2d[,2])
df <- BertopicR::test_data
model <- bt_compile_model(embedding_model = bt_empty_embedder(),
                          reduction_model = bt_empty_reducer())
bt_fit_model(model = model, documents = df$docs)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Outlier Manipulation"),

    # Sidebar with a slider input for number of bins 
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

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  method <- reactive(input$outlier_method)
  threshold <- reactive(input$outlier_threshold)
  

    output$outlier_plot <- renderPlot({
      
      if (method() == "c-tf-idf"){
        outliers <- bt_outliers_ctfidf(fitted_model = model,
                                       documents = df$docs,
                                       topics = df$topics,
                                       threshold = threshold())
      } else if (method() == "embeddings"){
        outliers <- bt_outliers_embeddings(fitted_model = model,
                                           documents = df$docs,
                                           topics = df$topics,
                                           embeddings = embeddings,
                                           embedding_model = embedder,
                                           threshold = threshold())
      } else if (method() == "tokenset similarity"){
        outliers <- bt_outliers_tokenset_similarity(fitted_model = model,
                                                    documents = df$docs,
                                                    topics = df$topics,
                                                    threshold = threshold())
      }
      
      df %>%
        mutate(new_topics = as.factor(outliers$new_topics)) %>%
        ggplot(aes(x = v1, y = v2, colour = new_topics)) +
        geom_point()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
