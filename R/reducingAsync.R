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
    
    
    bg_job <- shiny::eventReactive(poll_rct(), {
      shiny::req(isTRUE(poll_rct()))
      callr::r_bg(function(n_neighbours, n_components, min_dist, metric, embeddings){
            reducer <- BertopicR::bt_make_reducer_umap(n_neighbours = n_neighbours, 
                                                       n_components = n_components, 
                                                       min_dist = min_dist, 
                                                       metric = metric)
            reduced_embeddings <- BertopicR::bt_do_reducing(reducer, embeddings)
          },
          args = list(n_neighbours = n_neighbours, n_components = n_components, min_dist = min_dist, metric = metric, embeddings = embeddings),
          supervise = TRUE)
    }) 
    
    shiny::observe({
      shiny::req(isTRUE(poll_rct()))
      shiny::invalidateLater(250)
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
      get_result = shiny::reactive(res_rct())
    ))
    
  })
}

