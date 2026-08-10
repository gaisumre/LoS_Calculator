facility_tab <- function() {
  tabPanel(
    "Facilities",
    
    fluidRow(
      column(
        width = 12,
        
        h3("Facility Characteristics"),
        
        p("This calculator offers detailed projections for different resident
          profiles and facility characteristics, including predicted percentiles
          and hazard rates. "),
        
        p("Select the organisation type, service size, state and remoteness of 
          your facility."),
        
        wellPanel(
          fluidRow(
            column(
              width = 3,
              selectInput(
                inputId = "fac_org",
                label = "Organisation type",
                choices = organisation_type_choices,
                selected = "Not-for-profit"
              )
            ),
            
            column(
              width = 3,
              selectInput(
                inputId = "fac_service",
                label = "Service size",
                choices = service_size_choices,
                selected = "100+"
              )
            ),
            
            column(
              width = 3,
              selectInput(
                inputId = "fac_state",
                label = "State",
                choices = state_choices,
                selected = "NSW"
              )
            ),
            
            column(
              width = 3,
              selectInput(
                inputId = "fac_remote",
                label = "Area",
                choices = remoteness_choices,
                selected = "Major Cities"
              )
            )
          )
        ),
        
        h4("Resident profile dataset"),
        
        p("Add any previous aged-care episodes here. For each episode, select
          the facility details and enter the length of stay."),
        
        uiOutput("fac_profile_table_ui"),
        
        actionButton(
          inputId = "fac_add_profile",
          label = "Add profile",
          class = "btn-primary"
        ),
        
        hr(),
        
        h4("Summary"),
        tableOutput("fac_summary"),
        
        h4("Predicted LoS by resident profile"),
        
        radioButtons(
          inputId = "fac_plot_type",
          label = "Choose plot type",
          choices = c(
            "Bar plot" = "bar",
            "Ridge plot" = "ridge",
            # "Box plot" = "box",
            "Retention table" = "tab",
            "Hazard rate" = "haz"
          ),
          selected = "bar",
          inline = TRUE
        ),
        
        conditionalPanel(
          condition = "input.fac_plot_type == 'bar'",
          plotOutput("fac_bar_plot")
        ),
        
        conditionalPanel(
          condition = "input.fac_plot_type == 'ridge'",
          p(
            "This ridge plot shows the predicted length-of-stay distribution
            for each resident profile. The vertical lines mark the 25th
            percentile, median and 75th percentile."
          ),
          plotOutput("fac_ridge_plot", width = "100%", height = "400px"),
          tableOutput("fac_ridge_quartile_table")
        ),
        
        conditionalPanel(
          condition = "input.fac_plot_type == 'box'",
          plotOutput("fac_box_plot")
        ),
        
        conditionalPanel(
          condition = "input.fac_plot_type == 'tab'",
          
          h4("Expected residents remaining in facility"),
          
          p(
            "Each profile column shows the estimated percentage of residents in that ",
            "profile who are expected to still be in the facility after the selected ",
            "number of months since admission."
          ),
          
          tableOutput("fac_survival_retention_table")
        ),
        
        conditionalPanel(
          condition = "input.fac_plot_type == 'haz'",
          plotly::plotlyOutput("fac_hazard_plot")
        )
      )
    )
    # end fluidRow
  )
}