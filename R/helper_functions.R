do_reducing_asyncSave <- function(reducer = reducer, embeddings = embeddings){
  # args <- list(reducer = reducer(), df = df())
  reducing_process <- callr::r_bg(function(reducer = reducer, embeddings = unlist(embeddings)){
    BertopicR::bt_do_reducing(reducer = reducer, embeddings = embeddings)
  })
  return(reducing_process)
}

do_reducing_async <- function(x, sleep) {
  args <- shiny::reactiveValuesToList(list(head_six = BertopicR::bt_do_reducing, x = x, sleep = sleep))
  bg_process <- callr::r_bg(
    func = function(head_six, x, sleep) {
      head_six(x, sleep)
    },
    args = args,
    supervise = TRUE
  )
  return(bg_process)
}
