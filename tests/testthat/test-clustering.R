test_that("Test clusteringUi works as expected", {
  ui <- clusteringUi(id = "test")
  
  # browser()
  expect_true("shiny.tag.list" %in% class(ui))
  
  # checking html 
  ui_char <- as.character(ui)
  selectOptions <- stringr::str_extract_all(ui_char, "(?<=option value=\")[^\"]+")[[1]] # options available in select inputs
  displayedOptions <- stringr::str_extract_all(ui_char,"(?<=\" value=\")[^\"]+")[[1]]
  
  # test general format ----
  expect_equal(stringr::str_count(ui_char, "div"), 112)
  expect_equal(stringr::str_count(ui_char, "shiny-input-container"), 15)
  expect_equal(stringr::str_count(ui_char, "shiny-input-file"), 2)
  expect_equal(stringr::str_count(ui_char, "accept=\\\".xlsx,.csv,.tsv,.rds\\\""), 2)
  expect_equal(stringr::str_count(ui_char, "shiny-input-radiogroup"), 2)
  expect_equal(length(displayedOptions), 9)
  expect_equal(stringr::str_count(ui_char, "shiny-input-select"), 4)
  expect_equal(length(selectOptions), 26)
  expect_equal(stringr::str_count(ui_char, "shiny-input-number"), 5)
  expect_equal(stringr::str_count(ui_char, "btn-default action-button"), 3)
  expect_equal(stringr::str_count(ui_char, "shiny-html-output"), 2) # datatables
  
  # test data upload ---
  expect_true(stringr::str_detect(ui_char, "No file selected"))
  
  # test reduce_embeddings panel ----
  expect_true(all(c("UMAP") %in% selectOptions[1:2]))
  expect_true(all(c("cosine", "euclidean") %in% selectOptions[2:3]))
  expect_true(all(c("Load in reduced embeddings", "Calculate in app") %in% displayedOptions[1:2]))
  
  # test clustering panel --- -
  expect_true(all(c("HDBSCAN", "K-Means") %in% selectOptions[4:5]))
  expect_true(all(c("euclidean", "yule") %in% selectOptions[c(6,26)]))
  expect_true(all(c("Load in reduced embeddings", "Calculate in app") %in% displayedOptions[1:2]))
  expect_true(all(c("eom", "leaf") %in% displayedOptions[8:9]))
  expect_equal(stringr::str_count(ui_char, "btn btn-default shiny-download-link"), 1)
  
})

test_that("clusteringServer takes correctly formatted df, doesn't accept incorrectly formatted df and displays the appropriate output", {
  testServer(
    app = clusteringServer,
    args = list(),
    exp = {
      ns <- session$ns
      
      # df with correct colnames
      session$setInputs(data_upload = 
                          list(
                            name = "testdata/example_rds.rds",
                            datapath = "testdata/example_rds.rds"))
      
      expect_true(is.data.frame(df())) # df() is generated
      expect_null(output$data_upload_error_message$html)
      
      # df with incorrect colnames
      session$setInputs(data_upload = 
                          list(
                            name = "testdata/df_not_working.rds",
                            datapath = "testdata/df_not_working.rds"))
      
      expect_true(stringr::str_detect(output$data_upload_error_message$html, "Required columns missing"))
      
    }
  )
})

# These tests about reduced_embeddings are not working rn -----
# test_that("clusteringServer takes uploaded embeddings when specified", {
#   testServer(
#     app = clusteringServer,
#     args = list(id = "test"),
#     exp = {
#       session$setInputs(data_upload = 
#                           list(
#                             name = "testdata/example_rds.rds",
#                             datapath = "testdata/example_rds.rds"))
#       session$setInputs(load_or_reduce_embeddings = "Load in reduced embeddings")
#       expect_error(reduced_embeddings()) # no data uploaded yet
# 
#       session$setInputs(
#         reduced_embeddings_upload = list(
#           name = "testdata/reduced_embeddings.csv",
#           datapath = "testdata/reduced_embeddings.csv")) # upload data
# 
#       expect_true(inherits(reduced_embeddings(), "tbl")) # reduced_embeddings is a tbl
#     }
#   )
# })


# test_that("clusteringServer correctly obtains reduced embeddings from child servers", {
#   embeddings <- matrix(runif(5), nrow = 1)
#   
#   df <- reactive({
#     data.frame(docs = "this is a doc",
#                v1 = 0.1,
#                v2 = 0.3) %>%
#       dplyr::mutate(embeddings,
#                     rowid = dplyr::row_number())
#   })
#   
#   testServer(
#     app = clusteringServer,
#     args = list(
#       id = "test"
#     ),
#     exp = {
#       session$setInputs(
#         load_or_reduce_embeddings = "Calculate in app",
#         `reducingServer-n_neighbours1` = 10,
#         `reducingServer-n_components1` = 5,
#         `reducingServer-min_dist1` = 0.0,
#         `reducingServer-reducing_metric1` = "cosine",
#         `reducingServer-do_reducing_option1` = NULL,
#         `reducingServer-backgroundReduce-n_neighbours` = 10,
#         `reducingServer-backgroundReduce-n_components` = 5,
#         `reducingServer-backgroundReduce-min_dist` = 0.0,
#         `reducingServer-backgroundReduce-metric` = "cosine",
#         `reducingServer-backgroundReduce-embeddings` = embeddings,
#         `reducingServer-backgroundReduce-wait_for_event` = TRUE,
#       )
#       
#       browser()
#       expect_null(reduced_embeddings_calculated())
# 
#       df <- reactive({
#         data.frame(docs = "this is a doc",
#                    v1 = 0.1,
#                    v2 = 0.3) %>%
#           dplyr::mutate(embeddings,
#                         rowid = dplyr::row_number())
#       })
#       
#       # df()
#       session$setInputs(`reducingServer-do_reducing_option1` = 1) # mimics action button
#       reduced_embeddings_calculated()
# 
#       Sys.sleep(5) # give time for operation to complete
#       session$elapse(millis = 250) # allow session to fastforward past invalidateLater(250) function
# 
#       expect_true(is.array(reduced_embeddings1$get_result()))
#       expect_true(is.array(session$returned()))
#     }
#   )
# })



test_that("mdoel")