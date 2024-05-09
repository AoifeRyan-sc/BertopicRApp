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

test_that("clusteringServer takes correctly formatted df, 
          doesn't accept incorrectly formatted df and displays the appropriate output", {
  testServer(
    app = clusteringServer,
    args = list(id = "test"),
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

test_that("clusteringServer takes uploaded embeddings when specified", {
  testServer(
    app = clusteringServer,
    args = list(id = "test"),
    exp = {
      session$setInputs(data_upload =
                          list(
                            name = "testdata/example_rds.rds",
                            datapath = "testdata/example_rds.rds"))
      session$setInputs(load_or_reduce_embeddings = "Load in reduced embeddings")
      expect_error(reduced_embeddings()) # no data uploaded yet

      session$setInputs(
        reduced_embeddings_upload = list(
          name = "testdata/reduced_embeddings.csv",
          datapath = "testdata/reduced_embeddings.csv")) # upload data

      expect_true(inherits(reduced_embeddings(), "tbl")) # reduced_embeddings is a tbl
    }
  )
})


test_that("clusteringServer correctly obtains reduced embeddings from child servers", {
  embeddings <- matrix(runif(5), nrow = 1)
  # embeddings <- matrix(runif(5), nrow = 394)

  testServer(
    app = clusteringServer,
    args = list(
      id = "test"
    ),
    exp = {
      session$setInputs(
        data_upload =
          list(
            name = "testdata/example_rds.rds",
            datapath = "testdata/example_rds.rds"
          ),
        load_or_reduce_embeddings = "Calculate in app",
        `reduction_ui-n_neighbours1` = 10,
        `reduction_ui-n_components1` = 5,
        `reduction_ui-min_dist1` = 0.0,
        `reduction_ui-reducing_metric1` = "cosine",
        `reduction_ui-do_reducing_option1` = NULL,
        `reduction_ui-backgroundReduce-n_neighbours` = 10,
        `reduction_ui-backgroundReduce-n_components` = 5,
        `reduction_ui-backgroundReduce-min_dist` = 0.0,
        `reduction_ui-backgroundReduce-metric` = "cosine",
        `reduction_ui-backgroundReduce-embeddings` = embeddings,
        `reduction_ui-backgroundReduce-wait_for_event` = TRUE,
      )
      
      # df <- reactive({
      #   data.frame(docs = "this is a doc",
      #              v1 = 0.1,
      #              v2 = 0.3) %>%
      #     dplyr::mutate(embeddings,
      #                   rowid = dplyr::row_number())
      # })
      # 
      # session$flushReact()

      # browser()
      session$setInputs(`reduction_ui-do_reducing_option1` = 1) # mimics action button
      Sys.sleep(15)
      session$elapse(millis = 500)
      expect_true(is.array(reduced_embeddings_calculated()))
      expect_true(is.array(reduced_embeddings()))
    }
  )
})

test_that("The child module modellingServer outputs fitted model and other params", {
  testServer(
    app = clusteringServer,
    args = list(id = "test"),
    exp = {
      session$setInputs(
        data_upload =
          list(
            name = "testdata/example_rds.rds",
            datapath = "testdata/example_rds.rds"
            ),  
        load_or_reduce_embeddings = "Load in reduced embeddings",
        reduced_embeddings_upload = 
          list(
            name = "testdata/reduced_embeddings.csv",
            datapath = "testdata/reduced_embeddings.csv"
            ),
        `modelling_selection-cluster_method` = "HDBSCAN",
        `modelling_selection-min_cluster_size` = 2,
        `modelling_selection-min_sample_size` = 5,
        `modelling_selection-hdbscan_cluster_selection` = "eom",
        `modelling_selection-hdbscan_metric` = "euclidean"
      )
      
      expect_equal(length(modelling_outputs$clusters()), nrow(df())) # outputs clusters
      expect_true(is.character(modelling_outputs$cluster_model())) # outputs cluster model
      expect_null(modelling_outputs$model()) # action button not yet clicked
       
      session$setInputs(`modelling_selection-do_modelling` = 1) # mimic action button
  
      expect_true(inherits(modelling_outputs$model(), "bertopic._bertopic.BERTopic")) # outputs Bertopic model
      expect_true(is.numeric(modelling_outputs$model()$topics_)) # outputs fitted Bertopic model
    
    }
  )
})

test_that("clusteringServer generates umap when modelling and reducing params provided", {
  # embeddings <- matrix(runif(5), nrow = 1)
  embeddings <- matrix(runif(5), nrow = 394)
  
  testServer(
    app = clusteringServer,
    args = list(
      id = "test"
    ),
    exp = {
      session$setInputs(load_or_reduce_embeddings = "Load in reduced embeddings",
                        data_upload =
                          list(
                            name = "testdata/example_rds.rds",
                            datapath = "testdata/example_rds.rds"
                            ),
                        reduced_embeddings_upload = 
                          list(
                            name = "testdata/reduced_embeddings.csv",
                            datapath = "testdata/reduced_embeddings.csv"
                            ),
                        `modelling_selection-cluster_method` = "HDBSCAN",
                        `modelling_selection-min_cluster_size` = 2,
                        `modelling_selection-min_sample_size` = 5,
                        `modelling_selection-hdbscan_cluster_selection` = "eom",
                        `modelling_selection-hdbscan_metric` = "euclidean",
                        `modelling_selection-do_modelling` = 1
                        ) # upload data
      # browser()
      # expect_true(inherits(output$cluster_plot, "json"))
      if(requireNamespace("jsonlite")){ # check the plotly select capabilities
        plotly_data <- jsonlite::fromJSON(output$cluster_plot[[1]])
        
        expect_contains(
          names(plotly_data$deps), c("name", "version", "src", "script", "stylesheet")
        )
        
        expect_contains(
          plotly_data$x$shinyEvents, c("plotly_hover", "plotly_selected", "plotly_click")
        )
        
        expect_equal(length(unlist(plotly_data$x$data$x)), nrow(df())) # plotting correct data
      }
    }
  )
})