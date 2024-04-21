test_that("Test modelExploreUi works as expected", {
  ui <- modelExploreUi(id = "test")
  
  # browser()
  expect_true("shiny.tag" %in% class(ui)) # maybe I should be using tagList
  
  # checking html 
  ui_char <- as.character(ui)
  expect_equal(stringr::str_count(ui_char, "div"), 28)
  expect_equal(stringr::str_count(ui_char, "data-toggle=\"tab\""), 4)
  expect_true(stringr::str_detect(ui_char, "class=\"well\" role=\"complementary\"") &
                 stringr::str_detect(ui_char, "class=\"col-sm-8\" role=\"main\""))

})

test_that("modelExploreServer calculates reduced emeddings and returns them", {
  
  df <- reactive({
    data.frame(docs = c("this is a doc", "this is another doc"),
               v1 = c(0.1, 0.2),
               v2 = c(0.3, 0.4)) %>%
      dplyr::mutate(embeddings = matrix(runif(10), nrow = 2))
  }) 
  model <- reactive({
    model = bt_compile_model(embedding_model = bt_empty_embedder(),
                             reduction_model = bt_empty_reducer())
    bt_fit_model(model, df()$docs, df()$embeddings)
    })
  
  testServer(
    app = modelExploreServer,
    args = list(id = "test",
                df = df,
                model = model),
    exp = {
      expect_true(grepl("datatables", output$topic_summary_display$html))
      browser()
    }
  )
})
