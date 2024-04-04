library(shiny)
library(callr)

head_six <- function(x, sleep) {
  Sys.sleep(sleep)
  head(x)
}

head_six_async <- function(x, sleep) {
  args <- list(head_six = head_six, x = x, sleep = sleep)
  bg_process <- callr::r_bg(
    func = function(head_six, x, sleep) {
      head_six(x, sleep)
    },
    args = args,
    supervise = TRUE
  )
  return(bg_process)
}

mod_async_srv <- function(id, fun_async, fun_args, wait_for_event = FALSE) {
  moduleServer( id, function(input, output, session){
    res_rct <- shiny::reactiveVal(NULL)
    poll_rct <- shiny::reactiveVal(TRUE)
    
    if (isTRUE(wait_for_event)) {
      poll_rct(FALSE)
    }
    
    bg_job <- reactive({
      req(isTRUE(poll_rct()))
      do.call(fun_async, fun_args)
    }) |> bindEvent(poll_rct())
    
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

ui <- shiny::fluidPage(
  shiny::actionButton("go_iris", "Go Iris"),
  shiny::verbatimTextOutput("iris"),
  
  # shiny::actionButton("go_mtcars", "Go Mtcars"),
  shiny::verbatimTextOutput("mtcars"),
  
  shiny::sliderInput("slider", "Observations", min= 10, max = 100, value = 50),
  shiny::plotOutput("some_plot")
)

server <- function(input, output, session) {
  
  iris_result <- mod_async_srv(
    id = "iris_srv", 
    fun_async = "head_six_async",
    fun_args = list(x = iris, sleep = 5),
    wait_for_event = TRUE
  )
  
  observe({
    iris_result$start_job()
  }) |>  bindEvent(input$go_iris)
  
  output$iris <- shiny::renderPrint({
    iris_result$get_result()
  })
  
  mtcars_result <- mod_async_srv(
    id = "mtcars_srv", 
    fun_async = "head_six_async",
    fun_args = list(x = mtcars, sleep = 2),
    wait_for_event = FALSE
  )
  
  output$mtcars <- shiny::renderPrint({
    mtcars_result$get_result()
  }) 
  
  output$some_plot <- shiny::renderPlot({
    hist(sample(1000, input$slider))
  })
}

shiny::shinyApp(ui, server)
