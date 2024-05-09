test_that("modellingServer clusters and models as expected", {
  
  sentences <- stringr::sentences[1:50]
  embeddings <- array(runif(100), dim = c(50, 2))
  
  df <- function(){
    data.frame(docs = sentences,
               v1 = embeddings[,1],
               v2 = embeddings[,2]) %>%
      dplyr::mutate(embeddings = embeddings,
                    rowid = dplyr::row_number())
  }
  
  reduced_embeddings <- embeddings
  
  testServer(
    app = modellingServer,
    args = list(
      id = "test",
      df = df,
      reduced_embeddings = reactive({reduced_embeddings})
    ),
    exp = {
      session$setInputs(
        cluster_method = "HDBSCAN",
        min_cluster_size = 2,
        min_sample_size = 5,
        hdbscan_cluster_selection = "eom",
        hdbscan_metric = "euclidean"
      )
      
      expect_true(inherits(clusterer(), "hdbscan.hdbscan_.HDBSCAN")) # hdscan clusterer created
      expect_equal(clusterer()$metric, "euclidean") # takes on correct inputs
      
      session$setInputs(
        cluster_method = "K-Means",
        n_clusters = 10
      )
      
      session$flushReact()
      expect_true(inherits(clusterer(), "sklearn.cluster._kmeans.KMeans"))
      expect_equal(clusterer()$n_clusters, 10)
      
      expect_true(inherits(clusters(), "integer"))
      expect_equal(length(clusters()), nrow(df()))
      
      session$setInputs(do_modelling = 1)
      
      expect_true(inherits(r$model, "bertopic._bertopic.BERTopic")) # model is created
      expect_equal(length(r$model$topics_), nrow(df())) # model is fitted
      
      # test returned values
      expect_true(is.list(session$returned))
      expect_true(inherits(session$returned$cluster_model(), "character"))
      expect_equal(length(session$returned$clusters()), nrow(df()))
      expect_true(inherits(session$returned$model(), "bertopic._bertopic.BERTopic")) # model is created
    }
  )
})
