backgroundReduce <- function(id, 
                                n_neighbours, n_components, min_dist, metric,
                                embeddings, wait_for_event = TRUE) {
  shiny::moduleServer( id, function(input, output, session){
    ns <- session$ns
    job_result <- shiny::reactiveVal(NULL)
    trigger_job <- shiny::reactiveVal(FALSE)
    
    if (isFALSE(wait_for_event)) {
      trigger_job(TRUE)
    }
     
    bg_job <- shiny::eventReactive(trigger_job(), {
      shiny::req(isTRUE(trigger_job()))
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
      shiny::req(isTRUE(trigger_job()))
      shiny::invalidateLater(250)
      # message(sprintf("checking: %s", id))
      error <- bg_job()$read_error()
      if (stringr::str_detect(error, "Epochs completed")){
        progress_regex <- "Epochs completed:(.*?)\\]"
        progress_message <- paste(stringr::str_extract_all(error, progress_regex)[[1]], collapse = "\n")
        # message(progress_message)
      } 
      
      alive <- bg_job()$is_alive()
      if (isFALSE(alive)) {
        job_result(bg_job()$get_result())
        trigger_job(FALSE)
      }
    })
    
    progress <- shiny::reactive({
 
        shiny::invalidateLater(250)

      error <- bg_job()$read_error()
      if (stringr::str_detect(error, "Epochs completed")){
        progress_regex <- "Epochs completed:(.*?)\\]"
        progress_message <- paste(stringr::str_extract_all(error, progress_regex)[[1]], collapse = "\n")
        progress_message
      } else {
        progress_message <- "Reducing in progress"
        progress_message
      }
      
    })
    
    return(list(
      start_job = function() trigger_job(TRUE),
      get_result = shiny::reactive(job_result()),
      is_alive = shiny::reactive(bg_job()$is_alive()),
      progress_message = progress
    ))
    
  })
}
