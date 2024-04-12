# # 
# library(shiny)
# library(pheatmap)
# # 
# # # Define UI for dataset viewer app ----
# ui <- fluidPage(
# 
#   # App title ----
#   titlePanel("plot"),
# 
#   # Sidebar layout with input and output definitions ----
#   sidebarLayout(
# 
#     # Sidebar panel for inputs ----
#     sidebarPanel("Sidebar panel",
# 
#                  # Input: Selector for choosing dataset ----
#                  fileInput("file1", "Choose CSV File",
#                            accept = c(
#                              "text/csv",
#                              "text/comma-separated-values,text/plain",
#                              ".csv")
#                  ),
#                  tags$hr(),
#                  checkboxInput("header", "Header", TRUE)
#     ),
# 
#     # tags$hr(),
# 
#     sidebarPanel('get heatmap',
#                  actionButton('getHmap', 'get heatmap')
#     )
#   ),
# 
#     # Main panel for displaying outputs ----
#     mainPanel("Plot",
#               #column(6,
#               plotOutput("themap"),
#               tableOutput("table.output"))
#     #)
# 
# )
# 
# 
# server = function(input, output, session) {
#   a <- reactive({
#     inFile <- input$file1
#     if (is.null(inFile))
#       return(NULL)
#     tbl <- read.csv(inFile$datapath, header=input$header) #, sep=input$sep,  dec = input$dec)
#     return(tbl)
#   })
# 
#   output$table.output <- renderTable({
#     a()
#   })
# 
#   plotdata <- eventReactive(input$getHmap, {
#     a <- as.matrix(a()[-1])
#     row.names(a) <- a()$ID
#     a[is.na(a)] <- 0
#     a
#   })
# 
#   output$themap = renderPlot({
#     pheatmap(plotdata())
#   })
# }

# 
# ui <- fluidPage(
#   navbarPage(
#     navlistPanel(
#       tabPanel("tab1", 
#                sidebarLayout(
#                  sidebarPanel("sidebar1"),
#                  mainPanel("tab1 panel contents")
#                )
#       ),
#       tabPanel("tab2", "tab2 panel contents"),
#       tabPanel("tab3", "tab3 panel contents")
#     )
#   )
# )
# 
# 
# server <- function(input, output, session) {}

# shinyApp(ui, server)

library(shiny)
ui <- fluidPage(
  sidebarPanel(
    tabsetPanel(
      tabPanel("analysis-settings1",
               textInput("settings1",label = "set some options")),
      tabPanel("analysis-settings2",
               textInput("settings2",label = "Some other settings"))
    ),
    actionButton(inputId = "go", label="Go"),
    verbatimTextOutput("showsummarysettings")),
  
  
  
  mainPanel("..")
)

server <- function(input, output) {
  observeEvent(input$go,  ignoreInit=TRUE, {
    
    output$showsummarysettings <- renderText({
      "analysis-settings1 include ... and analysis-settings2 include ..."
    })
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)