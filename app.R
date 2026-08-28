library(shiny)
library(dplyr)
library(plotly)
library(stats)
library(tibble)
library(ggridges)
library(readr)
library(scales)
library(tidyr)

source("2-ui.R")
source("3-server.R")

shinyApp(ui = ui, server = server)