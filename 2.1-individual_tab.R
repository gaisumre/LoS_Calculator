source("2.1.1-input.R")

individual_tab <- function() {
  tabPanel(
    "Residents / Families",
    
    fluidPage(
      
      h3("Moving into Aged Care"),
      
      p("This calculator helps users estimate the typical length of stay in
          different types of aged care. Predictions can be generated for both
          first-time aged-care admissions and individuals who have previous
          aged-care episodes and intend to move to a new one, based on
          their personal and facility characteristics."),
      
      wellPanel(
        h4("Resident details"),
        
        radioButtons(
          "ind_sex",
          "Sex",
          choices = c("Female" = "F", "Male" = "M"),
          inline = TRUE
        ),
        
        selectInput(
          inputId = "ind_age",
          label = "Age at first admission",
          choices = c("65-69", "70-74",
                      "75-79", "80-84", "85-89", "90-94", "95-99", "100+"),
          width = "200px"
        ),
        
        selectInput(
          inputId = "ind_state",
          label = "State",
          choices = state_choices,
          width = "200px"
        ),
        
        radioButtons(
          "ind_birth",
          "Was the resident born in Australia?",
          choices = c("Yes" = "AUS", "No" = "OTHER"),
          inline = TRUE
        ),
        
        radioButtons(
          "ind_language",
          "Does the resident mainly speak English?",
          choices = c("Yes" = "eng", "No" = "other"),
          inline = TRUE
        ),
        
        selectInput(
          inputId = "ind_remote",
          label = "Which area is the intended aged care home located in?",
          choices = remoteness_choices,
          width = "400px"
        ),
        
        radioButtons(
          inputId = "ind_has_history",
          label = "Does the resident have previous aged-care episodes?",
          choices = c("No" = "no", "Yes" = "yes"),
          selected = "no",
          inline = TRUE
        ),
        
        conditionalPanel(
          condition = "input.ind_has_history == 'yes'",
          
          h4("Resident episode history"),
          
          p(
            "Each row represents one previous episode of care. 
     Select the facility characteristics and enter the length of stay for each episode."
          ),
          
          uiOutput("ind_episode_table_ui"),
          
          uiOutput("ind_episode_warning_ui"),
          
          actionButton(
            inputId = "ind_add_episode",
            label = "Add episode",
            class = "btn-primary"
          )
        )
      ),
      
      h4("Typical predicted length of stay (LoS) in Months"),
      
      p("This chart shows the estimated typical length of stay, in months,
          across different aged-care facility types and sizes. If this is not
          the first admission to an aged care, you can add episodes in the
          section named Resident episode history above."),
      
      plotOutput("ind_future_los_plot", width = "100%", height = "300px")
      
    )
  )
}