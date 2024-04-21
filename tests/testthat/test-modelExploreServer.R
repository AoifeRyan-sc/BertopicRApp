test_that("Test reducingUi works as expected", {
  ui <- modelExploreUi(id = "test")
  
  # browser()
  expect_true("shiny.tag" %in% class(ui)) # maybe I should be using tagList
  
  # checking html 
  ui_char <- as.character(ui)
  expect_equal(stringr::str_count(ui_char, "div"), 34)
  expect_equal(stringr::str_count(ui_char, "input-select"), 2)
  expect_equal(stringr::str_count(ui_char, "shiny-panel-conditional"), 2)
  expect_equal(stringr::str_count(ui_char, "action-button"), 1)
  expect_equal(stringr::str_count(ui_char, "type=\"number\""), 4)
  
  # If I build out the PCA options I should probably update this - or is this test pedantic?
})

test_that("ReducingServer calculates reduced emeddings and returns them", {
  # this is a working progress in working_docs/shiny-testing.Rmd but I am struggling
  testServer(
    app = reducingServer,
    args = list(id = "test",
                df = df),
    exp = {
      # library(BertopicR)
      # inputs to the reduceServer
      session$setInputs(
        n_neighbours1 = 10,
        n_components1 = 5,
        min_dist1 = 0.0,
        reducing_metric1 = "cosine",
        do_reducing_option1 = NULL,
        `backgroundReduce-n_neighbours` = 10,
        `backgroundReduce-n_components` = 5,
        `backgroundReduce-min_dist` = 0.0,
        `backgroundReduce-reducing_metric` = "cosine",
        `backgroundReduce-embeddings` = df()$embeddings,
        `backgroundReduce-wait_for_event` = TRUE,
      )
      
      browser()
      session$setInputs(do_reducing_option1 = 1)
      session$elapse(millis = 10000)
      # print(reduced_embeddings1$get_result())
      # Sys.sleep(10)
      # browser()
      print(reduced_embeddings1$get_result())
      # browser()
      # expect_true(inherits(reduced_embeddings(), "data.frame"))
    }
  )
})
