required_packages <- c("shiny", "tidyverse", "scales", "ggridges", "plotly",
                       "shinyjs")

missing_packages <- required_packages[
  !required_packages %in% rownames(installed.packages())
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

invisible(lapply(required_packages, library, character.only = TRUE))

library(shiny)
library(tidyverse)
library(scales)
library(ggridges)
library(plotly)

source("2-ui.R")
source("3-server.R")

shinyApp(ui = ui, server = server)