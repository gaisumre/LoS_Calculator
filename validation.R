# This is to validate that the outcome of prediction core functions is equal to
# the provided samples

library(dplyr)
library(readr)
library(tibble)

source("genf.R")
source("pred_los_revised.R")


# ------------------------------------------------------------
# Parameters
# ------------------------------------------------------------

input_path <- "z1-validation_pred_outcome/pred_los_time_dependent.csv"

output_path <- "z1-validation_pred_outcome/pred_los_time_dependent_comparison.csv"


# Load model
theta <- as.vector(readr::read_csv(
  "theta_aft_genf_params.csv",
  show_col_types = FALSE
)[, 3L])[[1L]]

hessian <- as.matrix(readr::read_csv(
  "theta_aft_genf_hessian.csv",
  show_col_types = FALSE
)[, -1L])

vStdErrors <- sqrt(diag(solve(hessian)))


# ------------------------------------------------------------
# Read validation data
# ------------------------------------------------------------

validation_data <- read_csv(
  input_path,
  show_col_types = FALSE
)


# ------------------------------------------------------------
# Clean categorical variables
#
# The validation CSV stores values as:
#   'F'
#   'AUS'
#   'Not-for-profit'
#
# Remove the literal single quotes before passing them to
# predict_los().
# ------------------------------------------------------------

predictor_cols <- c(
  "SEX",
  "COUNTRY_OF_BIRTH",
  "PREFERRED_LANGUAGE",
  "ADM_AGE_GROUP",
  "ORGANISATION_TYPE",
  "SERVICE_SIZE",
  "ACPR_SES",
  "REMOTENESS",
  "STATE"
)

new_data <- validation_data %>%
  mutate(
    across(
      all_of(predictor_cols),
      ~ gsub("^'|'$", "", .x)
    )
  ) %>%
  select(all_of(predictor_cols))


# ------------------------------------------------------------
# Run R prediction
#
# Assumes theta and vStdErrors have already been loaded into
# the environment using the same parameterisation as
# predict_los().
# ------------------------------------------------------------

pred_r <- predict_los(
  new_data = new_data,
  theta = theta,
  vStdErrors = vStdErrors,
  alpha = 0.05,
  n_sim = 1000,
  seed = 20240522
)$res


# ------------------------------------------------------------
# Rename R predictions
# ------------------------------------------------------------

pred_r <- pred_r %>%
  rename(
    mean_los_r = mean,
    median_los_r = median,
    mean_los_lb_r = mean_lb,
    mean_los_ub_r = mean_ub,
    median_los_lb_r = median_lb,
    median_los_ub_r = median_ub
  )


# ------------------------------------------------------------
# Combine with validation outcomes
# ------------------------------------------------------------

comparison <- validation_data %>%
  bind_cols(pred_r) %>%
  mutate(
    mean_los_diff =
      mean_los_r - mean_los,
    
    median_los_diff =
      median_los_r - median_los,
    
    mean_los_lb_diff =
      mean_los_lb_r - mean_los_lb,
    
    mean_los_ub_diff =
      mean_los_ub_r - mean_los_ub,
    
    median_los_lb_diff =
      median_los_lb_r - median_los_lb,
    
    median_los_ub_diff =
      median_los_ub_r - median_los_ub
  )


# ------------------------------------------------------------
# Save full comparison
# ------------------------------------------------------------

write_csv(
  comparison,
  output_path
)


# ------------------------------------------------------------
# Print predictions
# ------------------------------------------------------------

print(
  comparison %>%
    select(
      all_of(predictor_cols),
      
      # Existing validation results
      mean_los,
      median_los,
      
      # R implementation
      mean_los_r,
      median_los_r,
      
      # Differences
      mean_los_diff,
      median_los_diff
    ),
  n = Inf
)


# ------------------------------------------------------------
# Numerical validation summary
# ------------------------------------------------------------

validation_summary <- comparison %>%
  summarise(
    max_abs_mean_diff =
      max(abs(mean_los_diff), na.rm = TRUE),
    
    max_abs_median_diff =
      max(abs(median_los_diff), na.rm = TRUE),
    
    max_abs_mean_lb_diff =
      max(abs(mean_los_lb_diff), na.rm = TRUE),
    
    max_abs_mean_ub_diff =
      max(abs(mean_los_ub_diff), na.rm = TRUE),
    
    max_abs_median_lb_diff =
      max(abs(median_los_lb_diff), na.rm = TRUE),
    
    max_abs_median_ub_diff =
      max(abs(median_los_ub_diff), na.rm = TRUE)
  )

print(validation_summary)