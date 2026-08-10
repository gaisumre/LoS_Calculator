main_page <- function() {
  div(
    titlePanel("Welcome to the Residential Aged Care Length of Stay Calculator"),
    
    h6("Version: 1.0.0"),
    
    div(
      class = "main-page-content",
      
      p("This tool provides estimates of typical length of stay in permanent
        residential aged care facilities."),
      
      hr(),
      
      h3("Important information before using the calculator"),
      
      div(
        class = "disclaimer-box",
        
        p("The results provided by this calculator are estimates only.
          Individual experiences may differ substantially."),
        
        p("The estimates are based on the information you enter and a statistical
          model developed by ",
          tags$a(href = "https://doi.org/10.1016/j.insmatheco.2025.03.006",
            "Xu and Yan (2025)",
            target = "_blank"),
          ". Their study examined residents
          first admitted to permanent residential aged care in Australia in 2008
          and followed their records until 30 June 2022."),
        
        p("The calculator has two tabs: one for estimating results for
          individual residents and another for analysing results at the
          facility level."),
        
        p("The calculator considers only certain information when estimating
          length of stay. Other factors not included in the model may increase 
          or decrease a person’s actual length of stay.")
      ),
      
      checkboxInput(
        inputId = "accept_disclaimer",
        label = "I acknowledge that I have read and understood this information.",
        value = FALSE,
        width = "100%"
      ),
      
      actionButton(
        inputId = "continue_btn",
        label = "Continue",
        class = "btn-primary"
      )
    )
  )
}

calculator_page <- function() {
  div(
    titlePanel("Length of Stay Calculator for Long-term Aged Care"),
    
    tabsetPanel(
      individual_tab(),
      facility_tab()
    )
  )
}