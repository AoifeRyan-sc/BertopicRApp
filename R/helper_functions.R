reducingParamsUi <- function(id, id_num){
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      class = "row",
      shiny::div(
        class = "col-md-6",
        shiny::numericInput(ns(paste0("n_neighbours", id_num)), "No. of Nearest Neighbours", value = 15)
      ),
      shiny::div(
        class= "col-md-6",
        shiny::numericInput(ns(paste0("n_components", id_num)), "No. of Dimensions", value = 5)
      ),
      style = "margin-top: 30px"
    ),
    shiny::div(
      class = "row",
      shiny::div(
        class = "col-md-7",
        shiny::numericInput(ns(paste0("min_dist", id_num)), "Min Distance Between Points", value = 0)
      ),
      shiny::div(
        class= "col-md-5",
        shiny::selectInput(ns(paste0("reducing_metric", id_num)), "Distance Metric", choices = c("cosine", "euclidean")) # expand this
      )
    ),
    shiny::actionButton(ns(paste0("do_reducing_option", id_num)), label = shiny::HTML("<strong>Reduce</strong>"), class = "btn-succes", 
                        width = "100%", style = "margin-bottom: 30px; border-width: 2px;")
  )
}

