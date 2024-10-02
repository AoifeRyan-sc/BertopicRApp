# This assumes we're in a deployed environment, so we'll use file paths that start at app/
library(shiny)
library(here)
library(magrittr)

source("modules/reduce.R")
source("modules/modelling.R")
source("modules/modelExplore.R")
source("modules/clustering.R")
source("modules/outlier.R")
source("modules/clustering_panel_modules.R")


source("R/helper_functions.R")
source("R/reducingAsync.R")
