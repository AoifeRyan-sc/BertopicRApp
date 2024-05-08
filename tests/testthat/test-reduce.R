test_that("backgroundReduce function reduces embeddings", {

  embeddings <- matrix(runif(5), nrow = 1)
  
  df <- reactive({
    data.frame(docs = "this is a doc",
               v1 = 0.1,
               v2 = 0.3) %>%
      dplyr::mutate(embeddings,
                    rowid = dplyr::row_number())
  })
  
  testServer(
    app = backgroundReduce,
    args = list( 
      id = "test",
      n_neighbours = 10,
      n_components = 5,
      min_dist = 0.0,
      metric = "cosine",
      embeddings = embeddings,
      wait_for_event = TRUE),
    exp = {
      
      # wait_for_event means job is not triggered yet
      expect_false(trigger_job())
      expect_null(session$returned$get_result())
      expect_true(inherits(session$returned$start_job, "function"))
      
      # check returned values
      expect_true(is.list(session$returned))
      
      # not triggered yet
      expect_error(inherits(bg_job(), "r_process"))
      
      trigger_job(TRUE) # mimics action button in reduceServer
      expect_true(inherits(bg_job(), "r_process")) # job created
      expect_true(bg_job()$is_alive()) # running
      Sys.sleep(7) # wait for job to run
      expect_false(bg_job()$is_alive()) # finished running
      expect_true(is.array(bg_job()$get_result())) # outputs matrix
  
      # check array returned
      session$flushReact() # Manually trigger evaluation of reactives
      expect_true(is.array(session$returned$get_result())) #
      
    }
  )
})

test_that("ReducingServer calculates reduced emeddings and returns them", {
  
  embeddings <- matrix(runif(5), nrow = 1)
  
  df <- reactive({
    data.frame(docs = "this is a doc",
               v1 = 0.1,
               v2 = 0.3) %>%
      dplyr::mutate(embeddings,
                    rowid = dplyr::row_number())
  })
  
  testServer(
    app = reducingServer,
    args = list(id = "test",
                df = df),
    exp = {
      session$setInputs(
        n_neighbours1 = 10,
        n_components1 = 5,
        min_dist1 = 0.0,
        reducing_metric1 = "cosine",
        do_reducing_option1 = NULL,
        `backgroundReduce-n_neighbours` = 10,
        `backgroundReduce-n_components` = 5,
        `backgroundReduce-min_dist` = 0.0,
        `backgroundReduce-metric` = "cosine",
        `backgroundReduce-embeddings` = embeddings,
        `backgroundReduce-wait_for_event` = TRUE,
      )

      expect_null(reduced_embeddings1$get_result()) # no output before action button pressed
      
      session$setInputs(do_reducing_option1 = 1) # mimics action button
      
      Sys.sleep(5) # give time for operation to complete
      session$elapse(millis = 250) # allow session to fastforward past invalidateLater(250) function
     
      expect_true(is.array(reduced_embeddings1$get_result()))
      expect_true(is.array(session$returned()))
    }
  )
})
