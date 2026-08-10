source("3.1.1-genf.R")


# Model ----------------------------------------------------------------------

theta <- as.vector(readr::read_csv(
  "theta_aft_genf_params.csv",
  show_col_types = FALSE
)[, 3L])[[1L]]

hessian <- as.matrix(readr::read_csv(
  "theta_aft_genf_hessian.csv",
  show_col_types = FALSE
)[, -1L])

vStdErrors <- sqrt(diag(solve(hessian)))


# General helpers ------------------------------------------------------------

#' Require individual resident inputs
#'
#' @param input Shiny input object.
#'
#' @return Invisibly returns `NULL` after validating required inputs.
#'
#' @keywords internal
req_individual_inputs <- function(input) {
  req(
    input$ind_sex,
    input$ind_age,
    input$ind_birth,
    input$ind_language,
    input$ind_remote,
    input$ind_state
  )
}


# Individual tab -------------------------------------------------------------

## Profile -------------------------------------------------------------------

#' Build individual resident profiles
#'
#' Construct all organisation-type and service-size combinations for the
#' resident characteristics selected by the user.
#'
#' @param input Shiny input object.
#'
#' @return A tibble containing model input profiles.
#'
#' @keywords internal
build_individual_profile <- function(input) {
  req_individual_inputs(input)
  
  tidyr::expand_grid(
    ORGANISATION_TYPE = organisation_type_choices,
    SERVICE_SIZE = service_size_choices
  ) %>%
    dplyr::mutate(
      SEX = input$ind_sex,
      ADM_AGE_GROUP = input$ind_age,
      COUNTRY_OF_BIRTH = input$ind_birth,
      PREFERRED_LANGUAGE = input$ind_language,
      ACPR_SES = "Q5",
      REMOTENESS = input$ind_remote,
      STATE = input$ind_state
    ) %>%
    dplyr::select(
      SEX, COUNTRY_OF_BIRTH, PREFERRED_LANGUAGE, ADM_AGE_GROUP,
      ORGANISATION_TYPE, SERVICE_SIZE, ACPR_SES, REMOTENESS, STATE
    )
}


## Episode UI ----------------------------------------------------------------

#' Create an empty individual episode table
#'
#' @return An empty tibble used to initialise episode history.
#'
#' @keywords internal
empty_individual_episode_data <- function() {
  tibble::tibble(
    id = integer(),
    ORGANISATION_TYPE = character(),
    REMOTENESS = character(),
    SERVICE_SIZE = character(),
    episode_los = character()
  )
}


#' Build individual episode input table
#'
#' The internal `id` is a permanent unique identifier used to construct
#' Shiny input IDs. The displayed episode number is based on the current
#' row position and may therefore be renumbered visually after deletion.
#'
#' @param episodes Episode data stored in the reactive value.
#'
#' @return Shiny UI containing the episode input table.
#'
#' @keywords internal
individual_episode_table_ui <- function(episodes) {
  if (nrow(episodes) == 0) {
    return(p("No episodes have been added yet."))
  }
  
  tags$table(
    class = "table table-striped table-bordered table-sm",
    
    tags$thead(
      tags$tr(
        tags$th("Episode"),
        tags$th("Organisation type"),
        tags$th("Area"),
        tags$th("Facility size"),
        tags$th("Length of stay (in months)"),
        tags$th("Remove")
      )
    ),
    
    tags$tbody(
      lapply(seq_len(nrow(episodes)), function(i) {
        row <- episodes[i, ]
        
        tags$tr(
          tags$td(paste0("Episode ", i)),
          
          tags$td(
            selectInput(
              inputId = paste0("ind_org_type_", row$id),
              label = NULL,
              choices = organisation_type_choices,
              selected = row$ORGANISATION_TYPE,
              width = "120px"
            )
          ),
          
          tags$td(
            selectInput(
              inputId = paste0("ind_remote_", row$id),
              label = NULL,
              choices = remoteness_choices,
              selected = row$REMOTENESS,
              width = "120px"
            )
          ),
          
          tags$td(
            selectInput(
              inputId = paste0("ind_service_size_", row$id),
              label = NULL,
              choices = c(
                "Small\n(0–20)" = "0-20",
                "Medium\n(21–40)" = "21-40",
                "Large\n(41–60)" = "41-60",
                "Very Large\n(60+)" = "61-80"
              ),
              selected = row$SERVICE_SIZE,
              width = "120px"
            )
          ),
          
          tags$td(
            textInput(
              inputId = paste0("ind_episode_los_", row$id),
              label = NULL,
              value = row$episode_los,
              placeholder = "LoS in months",
              width = "120px"
            )
          ),
          
          tags$td(
            actionButton(
              inputId = paste0("ind_remove_episode_", row$id),
              label = "Remove",
              class = "btn-danger btn-sm"
            )
          )
        )
      })
    )
  )
}


## Episode data --------------------------------------------------------------

#' Build model-ready episode history
#'
#' @param input Shiny input object.
#' @param episode_data Episode history data.
#'
#' @return A tibble containing model predictors and episode LoS.
#'
#' @keywords internal
build_individual_episodes <- function(input, episode_data) {
  req_individual_inputs(input)
  
  episode_data %>%
    dplyr::mutate(episode_los = as.numeric(episode_los)) %>%
    dplyr::filter(episode_los > 0) %>%
    dplyr::mutate(
      SEX = input$ind_sex,
      ADM_AGE_GROUP = input$ind_age,
      COUNTRY_OF_BIRTH = input$ind_birth,
      PREFERRED_LANGUAGE = input$ind_language,
      ACPR_SES = "Q5",
      STATE = input$ind_state
    ) %>%
    dplyr::select(
      SEX, COUNTRY_OF_BIRTH, PREFERRED_LANGUAGE, ADM_AGE_GROUP,
      ORGANISATION_TYPE, SERVICE_SIZE, ACPR_SES, REMOTENESS, STATE,
      episode_los
    )
}


## Individual plot -----------------------------------------------------------

#' Plot future individual LoS
#'
#' @param plot_data Output from `future_los()`.
#'
#' @return A ggplot object.
#'
#' @keywords internal
plot_individual_future_los <- function(plot_data) {
  plot_data <- plot_data %>%
    dplyr::mutate(
      median_label = round(median, 0),
      SERVICE_SIZE = factor(SERVICE_SIZE, levels = service_size_choices),
      ORGANISATION_TYPE = factor(
        ORGANISATION_TYPE,
        levels = organisation_type_choices
      )
    ) %>%
    dplyr::filter(SERVICE_SIZE %in% c("0-20", "21-40", "41-60", "61-80"))
  
  ggplot(
    plot_data,
    aes(x = SERVICE_SIZE, y = ORGANISATION_TYPE, fill = median)
  ) +
    geom_tile(color = "grey30", linewidth = 0.6) +
    geom_text(aes(label = median_label), size = 6) +
    scale_x_discrete(
      labels = c(
        "0-20" = "Small\n(0–20)",
        "21-40" = "Medium\n(21–40)",
        "41-60" = "Large\n(41–60)",
        "61-80" = "Very Large\n(60+)"
      )
    ) +
    scale_fill_gradient(
      low = "#EFEFEF",
      high = "#4064A3",
      guide = "none"
    ) +
    labs(
      title = "Typical future LoS across facility types (months)",
      x = "Service size",
      y = "Organisation type"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 18, face = "bold"),
      axis.title.x = element_text(size = 15),
      axis.title.y = element_text(size = 15),
      axis.text.x = element_text(size = 13),
      axis.text.y = element_text(size = 13, angle = 45)
    )
}


## Individual server ---------------------------------------------------------

#' Register individual-tab server logic
#'
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object.
#'
#' @return Invisibly returns individual-tab reactive objects.
#'
#' @keywords internal
register_individual_server <- function(input, output, session) {
  
  ### Profile ---------------------------------------------------------------
  
  individual_profile <- reactive({
    build_individual_profile(input)
  }) %>%
    debounce(100)
  
  individual_prediction <- reactive({
    predict_los(
      new_data = individual_profile(),
      theta = theta,
      vStdErrors = vStdErrors,
      n_sim = 1
    )
  })
  
  
  ### Episode state ---------------------------------------------------------
  
  individual_episode_data <- reactiveVal(
    empty_individual_episode_data()
  )
  
  next_episode_id <- reactiveVal(0L)
  
  observeEvent(input$ind_add_episode, {
    new_id <- next_episode_id() + 1L
    next_episode_id(new_id)
    
    new_row <- tibble::tibble(
      id = new_id,
      ORGANISATION_TYPE = "Government",
      REMOTENESS = "Major Cities",
      SERVICE_SIZE = "0-20",
      episode_los = "1"
    )
    
    individual_episode_data(
      dplyr::bind_rows(individual_episode_data(), new_row)
    )
  })
  
  
  ### Episode UI ------------------------------------------------------------
  
  output$ind_episode_table_ui <- renderUI({
    individual_episode_table_ui(individual_episode_data())
  })
  
  
  ### Episode table interaction --------------------------------------------
  
  registered_episode_ids <- reactiveVal(integer())
  
  observe({
    episodes <- individual_episode_data()
    new_ids <- setdiff(episodes$id, registered_episode_ids())
    
    if (length(new_ids) == 0L) return()
    
    lapply(new_ids, function(episode_id) {
      
      observeEvent(input[[paste0("ind_org_type_", episode_id)]], {
        current_data <- individual_episode_data()
        new_value <- input[[paste0("ind_org_type_", episode_id)]]
        
        if (!is.null(new_value)) {
          individual_episode_data(
            current_data %>%
              dplyr::mutate(
                ORGANISATION_TYPE = dplyr::if_else(
                  id == episode_id, new_value, ORGANISATION_TYPE
                )
              )
          )
        }
      }, ignoreInit = TRUE)
      
      observeEvent(input[[paste0("ind_remote_", episode_id)]], {
        current_data <- individual_episode_data()
        new_value <- input[[paste0("ind_remote_", episode_id)]]
        
        if (!is.null(new_value)) {
          individual_episode_data(
            current_data %>%
              dplyr::mutate(
                REMOTENESS = dplyr::if_else(
                  id == episode_id, new_value, REMOTENESS
                )
              )
          )
        }
      }, ignoreInit = TRUE)
      
      observeEvent(input[[paste0("ind_service_size_", episode_id)]], {
        current_data <- individual_episode_data()
        new_value <- input[[paste0("ind_service_size_", episode_id)]]
        
        if (!is.null(new_value)) {
          individual_episode_data(
            current_data %>%
              dplyr::mutate(
                SERVICE_SIZE = dplyr::if_else(
                  id == episode_id, new_value, SERVICE_SIZE
                )
              )
          )
        }
      }, ignoreInit = TRUE)
      
      observeEvent(input[[paste0("ind_episode_los_", episode_id)]], {
        current_data <- individual_episode_data()
        new_los <- input[[paste0("ind_episode_los_", episode_id)]]
        
        if (!is.null(new_los)) {
          individual_episode_data(
            current_data %>%
              dplyr::mutate(
                episode_los = dplyr::if_else(
                  id == episode_id, new_los, episode_los
                )
              )
          )
        }
      }, ignoreInit = TRUE)
      
      observeEvent(input[[paste0("ind_remove_episode_", episode_id)]], {
        current_data <- individual_episode_data()
        
        individual_episode_data(
          current_data %>%
            dplyr::filter(id != episode_id)
        )
      }, ignoreInit = TRUE)
    })
    
    registered_episode_ids(
      union(registered_episode_ids(), new_ids)
    )
  })
  
  
  ### Episode data I/O ------------------------------------------------------
  
  individual_episodes <- reactive({
    build_individual_episodes(
      input,
      individual_episode_data()
    )
  }) %>%
    debounce(100)
  
  
  ### Episode warning -------------------------------------------------------
  
  individual_episode_los_limit <- reactive({
    req(input$ind_has_history == "yes")
    
    episodes <- individual_episodes()
    
    if (nrow(episodes) == 0) {
      return(NA_real_)
    }
    
    first_episode_profile <- episodes %>%
      dplyr::slice(1) %>%
      dplyr::select(-episode_los)
    
    pred <- predict_los(
      new_data = first_episode_profile,
      theta = theta,
      vStdErrors = vStdErrors
    )
    
    limit <- pred$res$median[1]
    
    if (!is.finite(limit)) {
      return(NA_real_)
    }
    
    limit
  }) %>%
    debounce(100)
  
  output$ind_episode_warning_ui <- renderUI({
    req(input$ind_has_history == "yes")
    
    episodes <- individual_episode_data()
    
    if (nrow(episodes) == 0) {
      return(NULL)
    }
    
    episode_los_values <- vapply(episodes$id, function(id) {
      value <- input[[paste0("ind_episode_los_", id)]]
      
      if (is.null(value) || length(value) == 0) {
        return(NA_real_)
      }
      
      suppressWarnings(as.numeric(value)[1])
    }, numeric(1))
    
    total_past_los <- sum(episode_los_values, na.rm = TRUE)
    dynamic_limit <- individual_episode_los_limit()
    
    if (
      !is.finite(total_past_los) ||
      !is.finite(dynamic_limit) ||
      total_past_los <= dynamic_limit
    ) {
      return(NULL)
    }
    
    tags$div(
      class = "alert alert-warning",
      style = "margin-top: 12px;",
      tags$strong("Please note: "),
      paste0(
        "The resident has already spent approximately ",
        round(total_past_los, 0),
        " months in aged care, which is longer than the typical predicted ",
        "length of stay for similar residents (approximately ",
        round(dynamic_limit, 0),
        " months). Predictions for residents with unusually long stays ",
        "may be less representative."
      )
    )
  })
  
  
  ### Result presentation ---------------------------------------------------
  
  output$ind_ind_data_check <- renderTable({
    episodes <- individual_episodes()
    ind_profile <- individual_profile()
    future_los(episodes, ind_profile, theta)
  })
  
  output$ind_episode_data_check <- renderTable({
    individual_episodes()
  })
  
  output$ind_future_los_plot <- renderPlot({
    episodes <- individual_episodes()
    ind_profile <- individual_profile()
    
    plot_data <- future_los(
      episodes,
      ind_profile,
      theta
    )
    
    plot_individual_future_los(plot_data)
  })
  
  
  invisible(
    list(
      profile = individual_profile,
      prediction = individual_prediction,
      episode_data = individual_episode_data,
      episodes = individual_episodes
    )
  )
}


# Facility tab ---------------------------------------------------------------

## Profile UI ----------------------------------------------------------------

#' Create an empty facility profile table
#'
#' @return Empty facility-profile tibble.
#'
#' @keywords internal
empty_facility_profile_data <- function() {
  tibble::tibble(
    id = integer(),
    count = integer(),
    SEX = character(),
    ADM_AGE_GROUP = character(),
    COUNTRY_OF_BIRTH = character(),
    PREFERRED_LANGUAGE = character(),
    ACPR_SES = character()
  )
}


#' Renumber facility profiles
#'
#' @param df Facility profile data.
#'
#' @return Sequentially renumbered profile data.
#'
#' @keywords internal
renumber_facility_profiles <- function(df) {
  if (nrow(df) == 0) {
    return(df)
  }
  
  df %>%
    dplyr::arrange(id) %>%
    dplyr::mutate(id = dplyr::row_number())
}


#' Build facility profile UI table
#'
#' @param profiles Facility resident profiles.
#'
#' @return Shiny profile table.
#'
#' @keywords internal
facility_profile_table_ui <- function(profiles) {
  if (nrow(profiles) == 0) {
    return(p("No resident profiles have been added yet."))
  }
  
  tags$table(
    class = "table table-striped table-bordered table-sm",
    
    tags$thead(
      tags$tr(
        tags$th("Profile"),
        tags$th("Sex"),
        tags$th("Age group"),
        tags$th("Language"),
        tags$th("Profile count"),
        tags$th("Remove")
      )
    ),
    
    tags$tbody(
      lapply(seq_len(nrow(profiles)), function(i) {
        row <- profiles[i, ]
        
        tags$tr(
          tags$td(paste0("Profile ", i)),
          
          tags$td(
            selectInput(
              inputId = paste0("fac_sex_", row$id),
              label = NULL,
              choices = c("Female" = "F", "Male" = "M"),
              selected = row$SEX,
              width = "120px"
            )
          ),
          
          tags$td(
            selectInput(
              inputId = paste0("fac_age_group_", row$id),
              label = NULL,
              choices = c(
                "65-69", "70-74", "75-79", "80-84",
                "85-89", "90-94", "95-99", "100+"
              ),
              selected = row$ADM_AGE_GROUP,
              width = "130px"
            )
          ),
          
          tags$td(
            selectInput(
              inputId = paste0("fac_language_", row$id),
              label = NULL,
              choices = language_choices,
              selected = row$PREFERRED_LANGUAGE,
              width = "130px"
            )
          ),
          
          tags$td(
            numericInput(
              inputId = paste0("fac_count_", row$id),
              label = NULL,
              value = row$count,
              min = 0,
              step = 1,
              width = "100px"
            )
          ),
          
          tags$td(
            actionButton(
              inputId = paste0("fac_remove_", row$id),
              label = "Remove",
              class = "btn-danger btn-sm"
            )
          )
        )
      })
    )
  )
}


## Facility plots ------------------------------------------------------------

#' Plot facility ridge distributions
#'
#' @param plot_data Facility ridge simulation data.
#'
#' @return A ggplot ridge plot.
#'
#' @keywords internal
plot_facility_ridge <- function(plot_data) {
  plot_data <- plot_data %>%
    dplyr::filter(
      is.finite(estimate),
      estimate >= 0,
      estimate <= 200
    ) %>%
    dplyr::mutate(
      profile_num = readr::parse_number(profile_label),
      profile_label = factor(
        profile_label,
        levels = unique(
          profile_label[order(profile_num, decreasing = TRUE)]
        )
      )
    )
  
  x_upper <- min(max(60, plot_data$estimate, na.rm = TRUE), 200)
  n_profiles <- dplyr::n_distinct(plot_data$profile_label)
  ridge_scale <- if (n_profiles == 1) 10 else 1.1
  
  ggplot(
    plot_data,
    aes(x = estimate, y = profile_label, fill = profile_label)
  ) +
    ggridges::stat_density_ridges(
      geom = "density_ridges",
      quantile_lines = TRUE,
      quantiles = c(0.25, 0.75),
      vline_linetype = "dashed",
      alpha = 0.7,
      scale = ridge_scale,
      rel_min_height = 0.01,
      show.legend = FALSE
    ) +
    ggridges::stat_density_ridges(
      geom = "density_ridges",
      quantile_lines = TRUE,
      quantiles = 0.5,
      vline_linetype = "solid",
      alpha = 0,
      scale = ridge_scale,
      rel_min_height = 0.01,
      show.legend = FALSE
    ) +
    geom_segment(
      data = tibble::tibble(
        x = c(0, 0),
        xend = c(1, 1),
        y = c(NA_real_, NA_real_),
        yend = c(NA_real_, NA_real_),
        line_type = c("25th / 75th percentile", "Median")
      ),
      aes(
        x = x,
        xend = xend,
        y = y,
        yend = yend,
        linetype = line_type
      ),
      inherit.aes = FALSE,
      colour = "black",
      linewidth = 0.8,
      show.legend = TRUE,
      na.rm = TRUE
    ) +
    coord_cartesian(xlim = c(0, x_upper)) +
    scale_x_continuous(
      breaks = seq(0, floor(x_upper / 12) * 12, by = 12),
      expand = expansion(mult = c(0, 0.08))
    ) +
    scale_y_discrete(
      expand = expansion(mult = c(0.4, 0.4))
    ) +
    labs(
      title = "LoS distribution by resident profile",
      x = "Predicted LoS (months)",
      y = NULL,
      fill = NULL
    ) +
    theme_minimal() +
    scale_linetype_manual(
      name = "Quantile lines",
      values = c(
        "25th / 75th percentile" = "dashed",
        "Median" = "solid"
      )
    ) +
    guides(
      fill = "none",
      linetype = guide_legend(
        override.aes = list(
          colour = "black",
          linewidth = 0.5
        )
      )
    ) +
    theme(
      plot.title = element_text(size = 18, face = "bold"),
      axis.title.x = element_text(size = 15),
      axis.text.x = element_text(size = 13),
      axis.text.y = element_text(size = 13),
      legend.position = "right"
    )
}


#' Plot facility mean and median LoS
#'
#' @param prediction_data Facility prediction results.
#'
#' @return A ggplot bar chart.
#'
#' @keywords internal
plot_facility_bar <- function(prediction_data) {
  plot_data <- prediction_data %>%
    dplyr::select(
      profile_label,
      mean, median,
      mean_lb, mean_ub,
      median_lb, median_ub
    ) %>%
    tidyr::pivot_longer(
      cols = c(mean, median),
      names_to = "statistic",
      values_to = "estimate"
    ) %>%
    dplyr::mutate(
      lower = dplyr::if_else(statistic == "mean", mean_lb, median_lb),
      upper = dplyr::if_else(statistic == "mean", mean_ub, median_ub),
      statistic = factor(statistic, levels = c("mean", "median"))
    )
  
  x_upper <- max(60, plot_data$upper, na.rm = TRUE)
  
  ggplot(plot_data, aes(x = estimate, y = profile_label, fill = statistic)) +
    geom_col(
      width = 0.4,
      position = position_dodge(width = 0.45),
      alpha = 0.9
    ) +
    geom_errorbar(
      aes(
        xmin = lower,
        xmax = upper,
        linetype = "95% Confidence Interval"
      ),
      width = 0.18,
      linewidth = 0.5,
      position = position_dodge(width = 0.45),
      orientation = "y"
    ) +
    geom_text(
      aes(
        x = estimate / 2,
        label = round(estimate, 2)
      ),
      position = position_dodge(width = 0.45),
      hjust = 0.5,
      size = 6
    ) +
    scale_x_continuous(
      breaks = seq(0, floor(x_upper / 12) * 12, by = 12),
      expand = expansion(mult = c(0, 0.08))
    ) +
    scale_y_discrete(limits = rev) +
    scale_fill_manual(
      values = c(
        "mean" = "mediumseagreen",
        "median" = "salmon"
      ),
      labels = c(
        "mean" = "Mean",
        "median" = "Median"
      ),
      guide = guide_legend(reverse = TRUE)
    ) +
    scale_linetype_manual(
      name = NULL,
      values = c("95% Confidence Interval" = "solid")
    ) +
    labs(
      title = "Predicted LoS (months) by resident profile",
      x = "Predicted LoS (months)",
      y = NULL,
      fill = NULL
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 18, face = "bold"),
      axis.title.x = element_text(size = 15),
      axis.title.y = element_text(size = 15),
      axis.text.x = element_text(size = 13),
      axis.text.y = element_text(size = 13),
      legend.title = element_text(size = 13),
      legend.text = element_text(size = 12)
    )
}


#' Plot facility LoS boxplots
#'
#' @param plot_data Facility LoS distribution data.
#'
#' @return A ggplot boxplot.
#'
#' @keywords internal
plot_facility_box <- function(plot_data) {
  x_upper <- max(60, plot_data$estimate, na.rm = TRUE)
  
  ggplot(
    plot_data,
    aes(x = estimate, y = profile_label, fill = profile_label)
  ) +
    geom_boxplot(
      width = 0.55,
      alpha = 0.8,
      outlier.alpha = 0.4
    ) +
    scale_x_continuous(
      limits = c(0, x_upper),
      expand = expansion(mult = c(0, 0.08))
    ) +
    scale_y_discrete(limits = rev) +
    labs(
      title = "LoS distribution by resident profile",
      x = "Predicted LoS (months)",
      y = NULL,
      fill = NULL
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 18, face = "bold"),
      axis.title.x = element_text(size = 15),
      axis.text.x = element_text(size = 13),
      axis.text.y = element_text(size = 13),
      legend.position = "none"
    )
}


#' Plot facility hazard rates
#'
#' @param plot_data Facility distribution and hazard data.
#'
#' @return A plotly object.
#'
#' @keywords internal
plot_facility_hazard <- function(plot_data) {
  plot_data <- plot_data %>%
    dplyr::filter(
      is.finite(estimate),
      is.finite(hazard),
      estimate >= 0,
      estimate <= 130,
      hazard >= 0
    )
  
  validate(
    need(nrow(plot_data) > 0, "No hazard rate data available.")
  )
  
  x_upper <- min(max(60, plot_data$estimate, na.rm = TRUE), 120)
  
  p <- ggplot(
    plot_data,
    aes(
      x = estimate,
      y = hazard,
      colour = profile_label,
      group = profile_label,
      text = paste0(
        "Resident profile: ", profile_label,
        "<br>Predicted LoS: ", round(estimate, 1), " months",
        "<br>Hazard rate: ", round(hazard, 4)
      )
    )
  ) +
    geom_line(linewidth = 1) +
    coord_cartesian(
      xlim = c(0, x_upper),
      ylim = c(0, 0.1)
    ) +
    labs(
      title = "Hazard rate by resident profile",
      x = "Predicted length of stay (months)",
      y = "Hazard rate",
      colour = "Resident profile"
    ) +
    scale_x_continuous(
      breaks = seq(0, floor(x_upper / 12) * 12, by = 12),
      expand = expansion(mult = c(0, 0.08))
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 18, face = "bold"),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 11)
    )
  
  plotly::ggplotly(p, tooltip = "text") %>%
    plotly::layout(
      dragmode = "zoom",
      xaxis = list(
        title = "Predicted length of stay (months)",
        range = c(0, x_upper),
        fixedrange = FALSE
      ),
      yaxis = list(
        title = "Hazard rate",
        range = c(0, 0.1),
        fixedrange = FALSE
      )
    ) %>%
    plotly::config(
      displayModeBar = TRUE,
      scrollZoom = TRUE,
      modeBarButtonsToRemove = c(
        "select2d",
        "lasso2d"
      )
    )
}


## Facility server -----------------------------------------------------------

#' Register facility-tab server logic
#'
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object.
#'
#' @return Invisibly returns facility-tab reactive objects.
#'
#' @keywords internal
register_facility_server <- function(input, output, session) {
  
  ### Profile state ---------------------------------------------------------
  
  facility_profile_data <- reactiveVal(
    empty_facility_profile_data()
  )
  
  next_profile_id <- reactiveVal(0L)
  
  observeEvent(input$fac_add_profile, {
    new_id <- next_profile_id() + 1L
    next_profile_id(new_id)
    
    new_row <- tibble::tibble(
      id = new_id,
      count = 1L,
      SEX = "F",
      ADM_AGE_GROUP = "85-89",
      COUNTRY_OF_BIRTH = "AUS",
      PREFERRED_LANGUAGE = "eng",
      ACPR_SES = "Q5"
    )
    
    facility_profile_data(
      dplyr::bind_rows(facility_profile_data(), new_row)
    )
  })
  
  
  ### Profile UI ------------------------------------------------------------
  
  output$fac_profile_table_ui <- renderUI({
    facility_profile_table_ui(facility_profile_data())
  })
  
  
  ### Profile table interaction --------------------------------------------
  
  registered_profile_ids <- reactiveVal(integer())
  
  observe({
    profiles <- facility_profile_data()
    new_ids <- setdiff(profiles$id, registered_profile_ids())
    
    if (length(new_ids) == 0L) return()
    
    lapply(new_ids, function(profile_id) {
      
      observeEvent(input[[paste0("fac_sex_", profile_id)]], {
        current_data <- facility_profile_data()
        new_value <- input[[paste0("fac_sex_", profile_id)]]
        
        if (!is.null(new_value)) {
          facility_profile_data(
            current_data %>%
              dplyr::mutate(
                SEX = dplyr::if_else(
                  id == profile_id, new_value, SEX
                )
              )
          )
        }
      }, ignoreInit = TRUE)
      
      observeEvent(input[[paste0("fac_age_group_", profile_id)]], {
        current_data <- facility_profile_data()
        new_value <- input[[paste0("fac_age_group_", profile_id)]]
        
        if (!is.null(new_value)) {
          facility_profile_data(
            current_data %>%
              dplyr::mutate(
                ADM_AGE_GROUP = dplyr::if_else(
                  id == profile_id, new_value, ADM_AGE_GROUP
                )
              )
          )
        }
      }, ignoreInit = TRUE)
      
      observeEvent(input[[paste0("fac_language_", profile_id)]], {
        current_data <- facility_profile_data()
        new_value <- input[[paste0("fac_language_", profile_id)]]
        
        if (!is.null(new_value)) {
          facility_profile_data(
            current_data %>%
              dplyr::mutate(
                PREFERRED_LANGUAGE = dplyr::if_else(
                  id == profile_id, new_value, PREFERRED_LANGUAGE
                )
              )
          )
        }
      }, ignoreInit = TRUE)
      
      observeEvent(input[[paste0("fac_count_", profile_id)]], {
        current_data <- facility_profile_data()
        new_count <- input[[paste0("fac_count_", profile_id)]]
        
        if (!is.null(new_count)) {
          facility_profile_data(
            current_data %>%
              dplyr::mutate(
                count = dplyr::if_else(
                  id == profile_id,
                  as.integer(new_count),
                  count
                )
              )
          )
        }
      }, ignoreInit = TRUE)
      
      observeEvent(input[[paste0("fac_remove_", profile_id)]], {
        current_data <- facility_profile_data()
        
        facility_profile_data(
          current_data %>%
            dplyr::filter(id != profile_id)
        )
      }, ignoreInit = TRUE)
    })
    
    registered_profile_ids(
      union(registered_profile_ids(), new_ids)
    )
  })
  
  
  ### Model-ready profiles --------------------------------------------------
  
  facility_profiles <- reactive({
    facility_profile_data() %>%
      dplyr::filter(count > 0) %>%
      dplyr::mutate(
        ORGANISATION_TYPE = input$fac_org,
        SERVICE_SIZE = input$fac_service,
        ACPR_SES = "Q5",
        REMOTENESS = input$fac_remote,
        STATE = input$fac_state
      )
  })
  
  
  ### Prediction ------------------------------------------------------------
  
  facility_prediction <- reactive({
    profiles <- facility_profiles()
    
    validate(
      need(
        nrow(profiles) > 0,
        "Add at least one resident profile."
      )
    )
    
    pred <- predict_los(
      new_data = profiles %>%
        dplyr::select(
          SEX,
          COUNTRY_OF_BIRTH,
          PREFERRED_LANGUAGE,
          ADM_AGE_GROUP,
          ORGANISATION_TYPE,
          SERVICE_SIZE,
          ACPR_SES,
          REMOTENESS,
          STATE
        ),
      theta = theta,
      vStdErrors = vStdErrors,
      n_sim = 500
    )
    
    profile_info <- profiles %>%
      dplyr::mutate(
        profile_label = paste0(
          "Profile ",
          dplyr::row_number()
        )
      ) %>%
      dplyr::select(
        id, profile_label, count, SEX, ADM_AGE_GROUP,
        PREFERRED_LANGUAGE, ACPR_SES
      )
    
    list(
      res = pred$res %>%
        dplyr::bind_cols(profile_info) %>%
        dplyr::relocate(
          id, profile_label, count, SEX, ADM_AGE_GROUP,
          PREFERRED_LANGUAGE, ACPR_SES
        ),
      
      sim = pred$df
    )
  })
  
  
  ### Summary ---------------------------------------------------------------
  
  output$fac_summary <- renderTable({
    pred <- facility_prediction()$res
    total_residents <- sum(pred$count)
    
    tibble::tibble(
      `Total residents` = round(total_residents, 0),
      `Weighted average LoS (months)` = weighted.mean(
        pred$mean,
        pred$count
      )
    ) %>%
      dplyr::mutate(
        dplyr::across(
          where(is.numeric),
          ~ round(.x, 2)
        )
      )
  })
  
  
  ### Distribution data -----------------------------------------------------
  
  facility_los_distribution_data <- reactive({
    pred <- facility_prediction()
    
    profile_info <- pred$res %>%
      dplyr::select(id, profile_label, count)
    
    phi <- pred$sim$phi
    param_GF <- pred$sim$param_GF
    
    n_sim <- 10000
    seq_probs <- seq(
      0.000001,
      0.999,
      length.out = n_sim
    )
    
    dplyr::bind_rows(
      lapply(seq_along(phi), function(i) {
        
        baseline_t <- qgenf(
          seq_probs,
          param_GF[1],
          param_GF[2],
          param_GF[3],
          param_GF[4]
        )
        
        estimate <- baseline_t / phi[i]
        
        survival <- 1 - pgenf(
          baseline_t,
          param_GF[1],
          param_GF[2],
          param_GF[3],
          param_GF[4]
        )
        
        density <- dgenf(
          baseline_t,
          param_GF[1],
          param_GF[2],
          param_GF[3],
          param_GF[4]
        )
        
        baseline_hazard <- density / survival
        
        tibble::tibble(
          id = profile_info$id[i],
          profile_label = profile_info$profile_label[i],
          count = profile_info$count[i],
          estimate = estimate,
          hazard = phi[i] * baseline_hazard,
          cum_hazard = -log(survival)
        )
      })
    )
  })
  
  
  ### Survival retention ----------------------------------------------------
  
  facility_survival_retention_data <- reactive({
    pred <- facility_prediction()
    
    profile_info <- pred$res %>%
      dplyr::select(id, profile_label, count)
    
    phi <- pred$sim$phi
    param_GF <- pred$sim$param_GF
    time_points <- c(12, 24, 36, 60)
    
    long_data <- dplyr::bind_rows(
      lapply(seq_along(phi), function(i) {
        
        survival <- 1 - pgenf(
          time_points * phi[i],
          param_GF[1],
          param_GF[2],
          param_GF[3],
          param_GF[4]
        )
        
        tibble::tibble(
          id = profile_info$id[i],
          profile_label = profile_info$profile_label[i],
          count = profile_info$count[i],
          time_after_admission = time_points,
          expected_still_in_facility = survival
        )
      })
    )
    
    long_data
  })
  
  facility_survival_retention_table <- reactive({
    facility_survival_retention_data() %>%
      dplyr::mutate(
        `Time after admission` = paste0(
          time_after_admission,
          " months"
        ),
        `Expected still in facility` = scales::percent(
          expected_still_in_facility,
          accuracy = 0.1
        )
      ) %>%
      dplyr::select(
        `Time after admission`,
        profile_label,
        `Expected still in facility`
      ) %>%
      tidyr::pivot_wider(
        names_from = profile_label,
        values_from = `Expected still in facility`
      )
  })
  
  
  ### Ridge simulation data -------------------------------------------------
  
  facility_los_ridge_data <- reactive({
    pred <- facility_prediction()
    
    profile_info <- pred$res %>%
      dplyr::select(
        id,
        profile_label,
        count
      )
    
    phi <- pred$sim$phi
    param_GF <- pred$sim$param_GF
    n_sim <- 10000
    
    dplyr::bind_rows(
      lapply(seq_along(phi), function(i) {
        
        baseline_t <- rgenf(
          n_sim,
          param_GF[1],
          param_GF[2],
          param_GF[3],
          param_GF[4]
        )
        
        tibble::tibble(
          id = profile_info$id[i],
          profile_label = profile_info$profile_label[i],
          count = profile_info$count[i],
          estimate = baseline_t / phi[i]
        )
      })
    )
  })
  
  
  ### Ridge outputs ---------------------------------------------------------
  
  output$fac_ridge_plot <- renderPlot({
    plot_facility_ridge(
      facility_los_ridge_data()
    )
  })
  
  output$fac_ridge_quartile_table <- renderTable({
    facility_los_distribution_data() %>%
      dplyr::filter(
        is.finite(estimate),
        estimate >= 0
      ) %>%
      dplyr::group_by(
        profile_label
      ) %>%
      dplyr::summarise(
        `25th percentile` = round(
          stats::quantile(
            estimate,
            0.25,
            na.rm = TRUE
          ),
          2
        ),
        `Median` = round(
          stats::quantile(
            estimate,
            0.50,
            na.rm = TRUE
          ),
          2
        ),
        `75th percentile` = round(
          stats::quantile(
            estimate,
            0.75,
            na.rm = TRUE
          ),
          2
        ),
        .groups = "drop"
      ) %>%
      dplyr::rename(
        `Resident profile` = profile_label
      )
  })
  
  
  ### Retention output ------------------------------------------------------
  
  output$fac_survival_retention_table <- renderTable({
    facility_survival_retention_table()
  })
  
  
  ### Bar plot --------------------------------------------------------------
  
  output$fac_bar_plot <- renderPlot({
    plot_facility_bar(
      facility_prediction()$res
    )
  })
  
  
  ### Box plot --------------------------------------------------------------
  
  output$fac_box_plot <- renderPlot({
    plot_facility_box(
      facility_los_distribution_data()
    )
  })
  
  
  ### Hazard plot -----------------------------------------------------------
  
  output$fac_hazard_plot <- plotly::renderPlotly({
    plot_facility_hazard(
      facility_los_distribution_data()
    )
  })
  
  
  invisible(
    list(
      profile_data = facility_profile_data,
      profiles = facility_profiles,
      prediction = facility_prediction,
      distribution = facility_los_distribution_data,
      survival = facility_survival_retention_data,
      ridge = facility_los_ridge_data
    )
  )
}


# Server core ----------------------------------------------------------------

#' LoS calculator core server
#'
#' Register the individual and facility server components.
#'
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object.
#'
#' @return The Shiny output object.
#'
#' @export
server_core <- function(input, output, session) {
  
  # Individual tab ----------------------------------------------------------
  
  register_individual_server(
    input,
    output,
    session
  )
  
  
  # Facility tab ------------------------------------------------------------
  
  register_facility_server(
    input,
    output,
    session
  )
  
  
  return(output)
}