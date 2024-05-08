test_that("Test modelExploreUi works as expected", {
  ui <- modelExploreUi(id = "test")
  
  # browser()
  expect_true("shiny.tag" %in% class(ui)) # maybe I should be using tagList
  
  # checking html 
  ui_char <- as.character(ui)
  
  # general structure
  expect_equal(stringr::str_count(ui_char, "div"), 28)
  expect_equal(stringr::str_count(ui_char, "shiny-input-container"), 0)
  expect_equal(stringr::str_count(ui_char, "shiny-html-output"), 5) # display dictated server side

})

test_that("modelExploreServer renders error message when no model generated", {
  sentences <- stringr::sentences[1:50]
  embeddings <- array(runif(100), dim = c(50, 2))
  
  df <- function(){
    data.frame(docs = sentences,
               v1 = embeddings[,1],
               v2 = embeddings[,2]) %>%
      dplyr::mutate(embeddings = embeddings,
                    rowid = dplyr::row_number())
  }
  
  model <- reactive({ NULL })
  
  
  testServer(
    app = modelExploreServer,
    args = list(id = "test",
                df = df,
                model = model),
    exp = {
      
      display_opts <- c("topic_summary_display", "doc_breakdown_display", 
                        "model_explore_umap_display", "wlo_display", "representation_display")
      
      for (display_opt in display_opts) {
        # Run the test for each display option
        expect_true(stringr::str_detect(as.character(output[[display_opt]]$html), "No model has been generated"))
      }
    }
  )
})

test_that("modelExploreServer renders correct output when model generated", {
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
    app = modelExploreServer,
    args = list(id = "test",
                df = df,
                model = model
    ),
    exp = {

      # test html for each rendered display 
      expect_true(stringr::str_detect(output$topic_summary_display$html, "datatables"))
      expect_true(stringr::str_detect(output$doc_breakdown_display$html, "datatables"))
      expect_true(stringr::str_detect(output$model_explore_umap_display$html, "plotly html-widget"))
      expect_true(stringr::str_detect(output$wlo_display$html, "shiny-plot-output"))
      expect_true(stringr::str_detect(output$representation_display$html, "<br/>")) # I haven't implemented anything here yet
      
    }
  )
})

test_that("modelExploreServer uses crosstalk correctly? do I need to test this?", {
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
    app = modelExploreServer,
    args = list(id = "test",
                df = df,
                model = model
    ),
    exp = {
      
      # # test html for each rendered display 
      # expect_true(stringr::str_detect(output$topic_summary_display$html, "datatables"))
      # expect_true(stringr::str_detect(output$doc_breakdown_display$html, "datatables"))
      # expect_true(stringr::str_detect(output$model_explore_umap_display$html, "plotly html-widget"))
      # expect_true(stringr::str_detect(output$wlo_display$html, "shiny-plot-output"))
      # expect_true(stringr::str_detect(output$representation_display$html, "<br/>")) # I haven't implemented anything here yet
      
      # expect_true(output$outlier_display$deps[[3]]$name == "plotly-binding") # plot generated
      # 
      # if(requireNamespace("jsonlite")){ # check the plotly select capabilities
      #   plotly_data <- jsonlite::fromJSON(output$outlier_plot[[1]])
      #   
      #   expect_contains(
      #     names(plotly_data$deps), c("name", "version", "src", "script", "stylesheet")
      #   )
      #   
      #   expect_contains(
      #     plotly_data$x$shinyEvents, c("plotly_hover", "plotly_selected", "plotly_click")
      #   )
      # }
      # 
      # # how would I check that this can be made non-null?
      # expect_null(plotly::event_data("plotly_selected", source = "umap_outliers"))
      
    }
  )
})
