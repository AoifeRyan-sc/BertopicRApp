test_that("Test clusteringUi works as expected", {
  ui <- clusteringUi(id = "test")
  
  # browser()
  expect_true("shiny.tag.list" %in% class(ui))
  
  # checking html 
  ui_char <- as.character(ui)
  expect_equal(stringr::str_count(ui_char, "div"), 112)
  expect_true(stringr::str_detect(ui_char, "No file selected"))
  

})

test_that("clusteringServer takes correctly formatted df and doesn't accept incorrectly formatted df", {
  testServer(
    app = clusteringServer,
    args = list(),
    exp = {
      ns <- session$ns
    
      session$setInputs(data_upload = 
                          list(
                            name = "data/example_rds.rds",
                            datapath = "data/example_rds.rds"))
      
      expect_true(is.data.frame(df())) # df() is generated
      
      expect_true(stringr::str_detect(output$uploaded_data, "dataTables")) # outputs DT::datatable
      expect_null(output$data_upload_error_message$html)
      
      # browser() 

      session$setInputs(data_upload = 
                          list(
                            name = "inst/testdata/df_not_working.rds",
                            datapath = "inst/testdata/df_not_working.rds"))
      
      expect_true(stringr::str_detect(output$data_upload_error_message$html, "Required columns missing"))
      
    }
  )
})

test_that("Reduced embeddings are correctly calculated or uploaded", {
  # embeddings <- matrix(runif(5), nrow = 1)
  # df <- function(){
  #   data.frame(docs = "this is a doc",
  #              embeddings = embeddings,
  #              v1 = 0.1,
  #              v2 = 0.3) 
  # }
  testServer(
    app = clusteringServer,
    args = list(),
    exp = {
      ns <- session$ns
      session$setInputs(# can I make a toy df()
        data_upload = list(name = "data/example_rds.rds",
                           datapath = "data/example_rds.rds"),
        load_or_reduce_embeddings = "Load in reduced embeddings",
        reduced_embeddings_upload = list(
            name = "inst/testdata/reduced_embeddings.csv",
            datapath = "inst/testdata/reduced_embeddings.csv"))
      
      # browser()
      expect_true(5 %in% dim(reduced_embeddings()))
      
      # browser()
    }
  )
})
