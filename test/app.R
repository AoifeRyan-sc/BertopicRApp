ui <- fluidPage(
  shiny::plotOutput("umap")
)

server <- function(input, output, session) {
  colour_var <- reactive({sample(1:5, nrow(data_test), replace = TRUE)})
 
  df_test <- reactive({data_test %>%
      dplyr::rename(docs = message_gpt) %>%
      dplyr::mutate(rowid = dplyr::row_number(),
                    topic_var = colour_var())})
  
  output$umap <- shiny::renderPlot({
    # print(df_test() %>% head())
    # createUmap("umap", df = df_test, colour_var = colour_var, title = "test")
    calculate_wlos_app(df = df_test(),
                       topic_var = topic_var,
                       text_var = docs)
  })
}

shiny::shinyApp(ui, server)