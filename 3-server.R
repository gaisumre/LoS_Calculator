library(tidyverse)
library(ggridges)
source("3.1-pred_los.R")
source("3.2-server_core.R")
source("3.3-server_ui_pages.R")

# Server function ----
# This server function consists of two parts: individual tab and facility tab.
# It implements the computation of two tabs separately. 
server <- function(input, output, session) {
  
  disclaimer_accepted <- reactiveVal(FALSE)
  
  observeEvent(input$continue_btn, {
    if (isTRUE(input$accept_disclaimer)) {
      disclaimer_accepted(TRUE)
    } else {
      showNotification(
        "Please confirm that you have read and understood the disclaimer.",
        type = "warning",
        duration = 7
      )
    }
  })
  
  output$page_ui <- renderUI({
    if (!disclaimer_accepted()) {
      main_page()
    } else {
      calculator_page()
    }
  })

  server_core(input, output, session)
}