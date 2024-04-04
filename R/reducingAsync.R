reducingAsyncServer <- function(id, 
                                n_neighbours, n_components, min_dist, metric,
                                embeddings, wait_for_event = TRUE) {
  shiny::moduleServer( id, function(input, output, session){
    ns <- session$ns
    res_rct <- shiny::reactiveVal(NULL)
    poll_rct <- shiny::reactiveVal(FALSE)
    
    if (isFALSE(wait_for_event)) {
      poll_rct(TRUE)
    }
    
    
    bg_job <- eventReactive(poll_rct(), {
      req(isTRUE(poll_rct()))
      callr::r_bg(function(n_neighbours, n_components, min_dist, metric, embeddings){
        # tryCatch(
        #   {
            reducer <- BertopicR::bt_make_reducer_umap(n_neighbours = n_neighbours, 
                                                       n_components = n_components, 
                                                       min_dist = min_dist, 
                                                       metric = metric)
            reduced_embeddings <- BertopicR::bt_do_reducing(reducer, embeddings)
            return(reduced_embeddings)
          # },
          # error = function(e) {
          #   # Handle the error here
          #   message("Error occurred in background process:", e$message)
          #   # You might want to return some default value or signal the error condition
          #   print(e)
          #   # return(NULL)
          # }
        # )
          },
          args = list(n_neighbours = n_neighbours, n_components = n_components, min_dist = min_dist, metric = metric, embeddings = embeddings),
          supervise = TRUE)
    }) 
    
    observe({
      req(isTRUE(poll_rct()))
      invalidateLater(250)
      message(sprintf("checking: %s", id))
      
      alive <- bg_job()$is_alive()
      if (isFALSE(alive)) {
        res_rct(bg_job()$get_result())
        message(sprintf("done: %s", id))
        poll_rct(FALSE)
      }
    })
    
    return(list(
      start_job = function() poll_rct(TRUE),
      get_result = reactive(res_rct())
    ))
    
  })
}

