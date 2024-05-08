test_that("Test outlierUi works as expected", {
  ui <- outlierUi(id = "test")
  
  # browser()
  expect_true("shiny.tag" %in% class(ui))
  
  # checking html 
  ui_char <- as.character(ui)
  selectOptions <- stringr::str_extract_all(ui_char, "(?<=option value=\")[^\"]+")[[1]] # options available in select inputs

  expect_equal(stringr::str_count(ui_char, "shiny-input-select"), 1)
  expect_equal(length(selectOptions), 2)
  expect_true(all(c("c-tf-idf", "tokenset similarity") %in% selectOptions))
  expect_equal(stringr::str_count(ui_char, "js-range-slider"), 1)
  expect_true(stringr::str_detect(ui_char, "data-min=\"0\" data-max=\"1\""))
  expect_equal(stringr::str_count(ui_char, "btn btn-default shiny-download-link"), 2)
  expect_equal(stringr::str_count(ui_char, "shiny-text-output"), 1)
  expect_equal(stringr::str_count(ui_char, "shiny-html-output"), 1) # datatable
  # there is a load of other stuff about the embedding outlier reduction method but as I am not using that as an option atm I am not testing for it - I should change this though....

})

test_that("outlierServer displays error message when no model generated", {
  
  sentences <- stringr::sentences[1:50]
  embeddings <- array(runif(100), dim = c(50, 2))
  
  df <- function(){
    data.frame(docs = sentences,
               v1 = embeddings[,1],
               v2 = embeddings[,2]) %>%
      dplyr::mutate(embeddings = embeddings,
                    rowid = dplyr::row_number())
  }
  
  
  clusters <- function(){
    set.seed(123)
    sample(-1:2, 50, replace = TRUE)
  }
  
  model <- reactive({ NULL })
  
  testServer( 
    app = outlierServer,
    args = list(id = "test",
                df = df(),
                model = model,
                clusters = clusters()
    ),
    exp = {
      session$setInputs(outlier_threshold = 0.1,
                        outlier_method = "c-tf-idf")
      
      # error message displayd when model is null
      expect_true(stringr::str_detect(as.character(output$outlier_display$html), "No model has been generated"))
    }
  )
  
})

test_that("outlierServer calculates outliers", {
  set.seed(123)
  sentences <- stringr::sentences[1:50]
  embeddings <- array(runif(100), dim = c(50, 2))
  
  df <- reactive({
    
    data.frame(docs = sentences,
               v1 = embeddings[,1],
               v2 = embeddings[,2]) %>%
      dplyr::mutate(embeddings = embeddings,
                    rowid = dplyr::row_number())
  })
  
  model <- reactive({
    model <- bt_compile_model(embedding_model = bt_empty_embedder(),
                              reduction_model = bt_empty_reducer(),
                              clustering_model = bt_make_clusterer_hdbscan(min_samples = 2, min_cluster_size = 2L))
    bt_fit_model(model, df()$docs, embeddings = embeddings)
    
    return(model)
  })
  
  clusters <- reactive({
    model()$topics_
  })
    
  testServer(
    app = outlierServer,
    args = list(id = "test",
                df = df,
                model = model,
                clusters = clusters
                ),
    exp = {
      
      # first testing ctfidf
      session$setInputs(outlier_threshold = 0,
                        outlier_method = "c-tf-idf")
      

      ctfidf_outliers <- new_topics()
      
      expect_true(length(ctfidf_outliers) == nrow(df())) # calculated outliers same length as df
      expect_false(all(ctfidf_outliers == clusters())) # original clusters change
      
      # switch outlier reduction and expect different output
      session$setInputs(outlier_method = "tokenset similarity")
      tknset_outliers <- new_topics()
      expect_false(all(tknset_outliers == ctfidf_outliers)) # outliers change depending on method
      expect_false(all(tknset_outliers == clusters()))
      
    }
  )
})

test_that("outlierServer returns umap with correct properties when model generated", {
  sentences <- stringr::sentences[1:50]
  embeddings <- array(runif(100), dim = c(50, 2))

  df <- reactive({
    data.frame(docs = sentences,
               v1 = embeddings[,1],
               v2 = embeddings[,2]) %>%
      dplyr::mutate(embeddings = embeddings,
                    rowid = dplyr::row_number())
  })
  
  clusters <- reactive({
    set.seed(123)
    sample(-1:2, 50, replace = TRUE)
  })
  
  model <- reactive({
    model <- bt_compile_model(embedding_model = bt_empty_embedder(),
                              reduction_model = bt_empty_reducer(),
                              clustering_model = bt_empty_clusterer())
    bt_fit_model(model, df()$docs, embeddings = embeddings, topic_labels = clusters())
    
    return(model)
  })
  
  testServer(
    app = outlierServer,
    args = list(id = "test",
                df = df,
                model = model,
                # clusters = clusters()
                clusters = clusters
    ),
    exp = {
      session$setInputs(outlier_threshold = 0.1,
                        outlier_method = "c-tf-idf")
      
      expect_true(output$outlier_display$deps[[3]]$name == "plotly-binding") # plot generated
      
      if(requireNamespace("jsonlite")){ # check the plotly select capabilities
        plotly_data <- jsonlite::fromJSON(output$outlier_plot[[1]])
        
        expect_contains(
          names(plotly_data$deps), c("name", "version", "src", "script", "stylesheet")
        )
        
        expect_contains(
          plotly_data$x$shinyEvents, c("plotly_hover", "plotly_selected", "plotly_click")
        )
      }
      
      # how would I check that this can be made non-null?
      expect_null(plotly::event_data("plotly_selected", source = "umap_outliers"))
    
    }
  )
})

