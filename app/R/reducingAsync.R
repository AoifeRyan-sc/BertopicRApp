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
        message(progress_message)
      } else { 
        progress_message <- "Reducing in progress"
        message(progress_message)
        }
      
      alive <- bg_job()$is_alive()
      if (isFALSE(alive)) {
        job_result(bg_job()$get_result())
        # message(sprintf("done: %s", id))
        trigger_job(FALSE)
      }
    })
    
    progress <- shiny::reactive({
      # shiny::req(isTRUE(trigger_job()))
      # message <- ""
      # if(trigger_job()){
      #   # message(message)
        shiny::invalidateLater(250)
      #   error <- bg_job()$read_error()
      # 
      #   if (stringr::str_detect(error, "Epochs completed")){
      #     progress_regex <- "Epochs completed:(.*?)\\]"
      #     # initial_message <- paste(stringr::str_extract_all(error, progress_regex), sep = "\n")
      #     # message <- paste(message, paste(stringr::str_extract_all(error, progress_regex), sep = "\n"), sep = "\n")
      #     message <- paste(message, paste(stringr::str_extract_all(error, progress_regex)[[1]], collapse = "\n"), collapse = "\n")
      #     message
      #   } 
      #   if (message == "") { 
      #     "Reducing in progress"
      #   } 
      #   
      # } else {
      #   # if (message == "") {
      #   #   "Use the 'Reduce Button' to begin reducing embeddings."
      #   # } else {
      #   #   message
      #   # }
      #   "Not sure what to print here"
      # }
      # 
      error <- bg_job()$read_error()
      if (stringr::str_detect(error, "Epochs completed")){
        progress_regex <- "Epochs completed:(.*?)\\]"
        progress_message <- paste(stringr::str_extract_all(error, progress_regex)[[1]], collapse = "\n")
        # message(progress_message)
        progress_message
      } else {
        progress_message <- "Reducing in progress"
        # message(progress_message)
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
