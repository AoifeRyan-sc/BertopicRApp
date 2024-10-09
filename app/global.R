# This assumes we're in a deployed environment, so we'll use file paths that start at app/
library(shiny)
library(here)
library(magrittr)

source("modules/upload.R")
source("modules/embed_reduce.R")
source("modules/reduce.R")
source("modules/modelExplore.R")
source("modules/clustering.R")
source("modules/outlier.R")

source("R/helper_functions.R")
source("R/reducingAsync.R")

# source("www/styles.css")
