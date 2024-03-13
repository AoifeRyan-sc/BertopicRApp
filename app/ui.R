library(shiny)
library(shinyjs)
library(ggplot2)
library(dplyr)
# library(BertopicR)

# I think for this we will need to input embeddings and (various options?) for reduced 
# embeddings and we can adjust clustering parameters in the app.

# df <- readRDS("test_data.rds")

# if I keep this it definitely needs to move somewhere else
# gg_color_hue <- function(n) {
#   hues = seq(15, 375, length = n)
#   pal <- c("grey80", hcl(h = hues, l = 65, c = 100)[1:n-1])
#   return(pal)
# }

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinythemes::shinytheme("readable"),
                # Navbar page
                shinyjs::useShinyjs(),
                navbarPage(
                  "BertopicR",
                  # Tab panel for clustering ----
                  # tabPanel("Clustering",
                  #          
                  #          # Title panel
                  #          titlePanel("Clustering"),
                  #          
                  #          clustering_ui("clustering_ui"),
                  #          
                  # ), # tabPanel
                  
                  # Tab panel for exploring the model ----
                  tabPanel("Explore the Model", 
                           # tableOutput("topic_overview")),
                           # verbatimTextOutput("test")),
                           tableOutput("test")),
                  
                  # Outlier manipulation ----
                  tabPanel("Outlier Manipulation", 
                           titlePanel("Outlier Manipulation"),
                           
                           outlier_ui("outlier_ui")
                  )
                ) # navbarPage
                
                # end ----
) # fluidPage
