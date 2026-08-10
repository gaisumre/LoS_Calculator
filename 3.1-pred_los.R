# genf_prediction.R
#
# Prediction utilities for the time-dependent Generalised F AFT model.

source("3.1.1-genf.R")


# Model specification -----------------------------------------------------

.AFT_FACTOR_LEVELS <- list(
  SEX = c("F", "M"),
  
  ADM_AGE_GROUP = c(
    "85-89", "50-54", "55-59", "60-64", "65-69",
    "70-74", "75-79", "80-84", "90-94", "95-99", "100+"
  ),
  
  COUNTRY_OF_BIRTH = c(
    "AUS", "OTHER"
  ),
  
  PREFERRED_LANGUAGE = c(
    "eng", "other"
  ),
  
  ORGANISATION_TYPE = c(
    "Not-for-profit", "Government", "Private"
  ),
  
  SERVICE_SIZE = c(
    "100+", "0-20", "21-40", "41-60", "61-80", "81-100"
  ),
  
  ACPR_SES = c(
    "Q5", "Q1", "Q2", "Q3", "Q4"
  ),
  
  REMOTENESS = c(
    "Major Cities",
    "Inner Regional",
    "Outer Regional/Remote/Very Remote"
  ),
  
  STATE = c(
    "NSW", "VIC", "QLD", "WA", "SA", "TAS", "ACT", "NT"
  )
)


.AFT_FORMULA <- ~
  SEX +
  ADM_AGE_GROUP +
  COUNTRY_OF_BIRTH +
  PREFERRED_LANGUAGE +
  ORGANISATION_TYPE +
  SERVICE_SIZE +
  ACPR_SES +
  REMOTENESS +
  STATE



# Internal utilities ------------------------------------------------------


#' Standardise predictors for the AFT model
#'
#' Convert categorical predictors to factors using the factor levels used
#' when fitting the time-dependent AFT model.
#'
#' @param data A data frame containing all predictors required by the
#'   AFT model.
#'
#' @return A copy of `data` with the model predictors converted to factors
#'   with the required reference levels.
#'
#' @keywords internal
.standardise_aft_data <- function(data) {
  
  required_vars <- names(.AFT_FACTOR_LEVELS)
  
  missing_vars <- setdiff(
    required_vars,
    names(data)
  )
  
  if (length(missing_vars) > 0L) {
    stop(
      "Missing required predictor(s): ",
      paste(missing_vars, collapse = ", "),
      call. = FALSE
    )
  }
  
  data <- as.data.frame(data)
  
  for (var in required_vars) {
    data[[var]] <- factor(
      data[[var]],
      levels = .AFT_FACTOR_LEVELS[[var]]
    )
  }
  
  invalid_rows <- vapply(
    required_vars,
    function(var) anyNA(data[[var]]),
    logical(1)
  )
  
  if (any(invalid_rows)) {
    stop(
      "Unknown or missing factor level detected in: ",
      paste(required_vars[invalid_rows], collapse = ", "),
      call. = FALSE
    )
  }
  
  data
}


#' Construct the AFT design matrix
#'
#' @param data A data frame containing the required model predictors.
#'
#' @return A numeric design matrix without the intercept column.
#'
#' @keywords internal
.aft_model_matrix <- function(data) {
  
  data <- .standardise_aft_data(data)
  
  stats::model.matrix(
    .AFT_FORMULA,
    data = data
  )[, -1L, drop = FALSE]
}


#' Transform Generalised F model parameters
#'
#' The fitted parameter vector stores sigma and P on the log scale.
#' This function converts them back to their natural positive scale.
#'
#' @param theta Numeric parameter vector. The first four elements must be
#'   `mu`, `log(sigma)`, `Q`, and `log(P)`.
#'
#' @return A numeric vector containing `mu`, `sigma`, `Q`, and `P`.
#'
#' @keywords internal
.genf_parameters <- function(theta) {
  
  if (length(theta) < 4L) {
    stop(
      "`theta` must contain at least four Generalised F parameters.",
      call. = FALSE
    )
  }
  
  param <- theta[1:4]
  
  param[2] <- exp(param[2])
  param[4] <- exp(param[4])
  
  param
}


#' Calculate AFT acceleration factors
#'
#' Calculate
#'
#' \deqn{\phi = \exp(-X\beta)}
#'
#' for each row of a design matrix.
#'
#' @param X Numeric design matrix.
#' @param coef Numeric vector of regression coefficients.
#'
#' @return A numeric vector of acceleration factors.
#'
#' @keywords internal
.aft_phi <- function(X, coef) {
  
  if (ncol(X) != length(coef)) {
    stop(
      "Number of regression coefficients does not match ",
      "the number of design-matrix columns.",
      call. = FALSE
    )
  }
  
  exp(-drop(X %*% coef))
}


#' Calculate the baseline Generalised F mean
#'
#' The mean is evaluated by integrating the baseline survival function,
#'
#' \deqn{E(T_0) = \int_0^\infty S_0(t)\,dt.}
#'
#' This calculation is required only once for each parameter vector,
#' because the AFT model implies
#'
#' \deqn{E(T \mid X) = E(T_0) / \phi.}
#'
#' @param param_GF Numeric vector containing `mu`, `sigma`, `Q`, and `P`.
#'
#' @return Numeric scalar containing the baseline mean.
#'
#' @keywords internal
.genf_mean <- function(param_GF) {
  
  survival <- function(t) {
    1 - pgenf(
      t,
      param_GF[1],
      param_GF[2],
      param_GF[3],
      param_GF[4]
    )
  }
  
  stats::integrate(
    survival,
    lower = 0,
    upper = Inf
  )$value
}


#' Calculate the baseline Generalised F median
#'
#' @param param_GF Numeric vector containing `mu`, `sigma`, `Q`, and `P`.
#'
#' @return Numeric scalar containing the baseline median.
#'
#' @keywords internal
.genf_median <- function(param_GF) {
  
  qgenf(
    0.5,
    param_GF[1],
    param_GF[2],
    param_GF[3],
    param_GF[4]
  )
}



# Standard LoS prediction -------------------------------------------------


#' Predict length of stay from the Generalised F AFT model
#'
#' Generate point predictions and simulation-based confidence intervals
#' for the mean and median length of stay under a Generalised F
#' accelerated failure time (AFT) model.
#'
#' @param new_data A data frame containing the resident and facility
#'   predictors required by the fitted AFT model.
#'
#' @param theta Numeric parameter vector. The first four elements are
#'   `mu`, `log(sigma)`, `Q`, and `log(P)`, followed by the regression
#'   coefficients.
#'
#' @param vStdErrors Numeric vector of standard errors corresponding to
#'   `theta`.
#'
#' @param alpha Significance level used to construct simulation intervals.
#'   Defaults to `0.05`.
#'
#' @param n_sim Number of parameter simulations used for interval
#'   estimation. Defaults to `1000`.
#'
#' @param seed Random seed used for parameter simulation.
#'
#' @details
#' Under the AFT formulation
#'
#' \deqn{
#' F(t \mid X) = F_0(\phi t),
#' \qquad
#' \phi = \exp(-X\beta),
#' }
#'
#' the conditional mean and quantiles satisfy
#'
#' \deqn{
#' E(T \mid X) = E(T_0)/\phi
#' }
#'
#' and
#'
#' \deqn{
#' Q_p(T \mid X) = Q_p(T_0)/\phi.
#' }
#'
#' Therefore, for each simulated parameter vector, the baseline mean and
#' median only need to be calculated once. Predictions for all profiles
#' are then obtained by AFT scaling.
#'
#' The simulation remains inside the original `for` loop so that calls to
#' `rnorm()` occur in the same order as in the original implementation.
#' This preserves reproducibility for a fixed seed.
#'
#' Parameter uncertainty is simulated independently using the supplied
#' marginal standard errors.
#'
#' @return A list with two components:
#'
#' * `res`: a tibble containing `mean`, `median`, `mean_lb`, `mean_ub`,
#'   `median_lb`, and `median_ub`.
#' * `df`: a list containing the simulated `mean` and `median` matrices,
#'   the point-estimate `param_GF`, and the point-estimate `phi`.
#'
#' @export
predict_los <- function(
    new_data,
    theta,
    vStdErrors,
    alpha = 0.05,
    n_sim = 1000,
    seed = 20240522
) {
  
  # Validate inputs
  if (length(theta) != length(vStdErrors)) {
    stop(
      "`theta` and `vStdErrors` must have the same length.",
      call. = FALSE
    )
  }
  
  if (any(vStdErrors < 0)) {
    stop(
      "`vStdErrors` must be non-negative.",
      call. = FALSE
    )
  }
  
  
  # Construct design matrix
  X <- .aft_model_matrix(new_data)
  
  n <- nrow(X)
  
  if (length(theta) != 4L + ncol(X)) {
    stop(
      "`theta` has ",
      length(theta),
      " parameters, but ",
      4L + ncol(X),
      " are required by the model matrix.",
      call. = FALSE
    )
  }
  
  
  # Allocate simulation results
  #
  # Structure is identical to the original implementation:
  # rows    = profiles
  # columns = parameter simulations
  
  df_mean <- matrix(
    NA_real_,
    nrow = n,
    ncol = n_sim
  )
  
  df_median <- matrix(
    NA_real_,
    nrow = n,
    ncol = n_sim
  )
  
  
  # Parameter simulation
  #
  # Keep the original loop and rnorm() call structure so that simulation
  # draws remain reproducible under the same seed.
  
  set.seed(seed)
  
  for (i in seq_len(n_sim)) {
    
    new_theta <- stats::rnorm(
      length(theta),
      mean = theta,
      sd = vStdErrors
    )
    
    
    # Transform Generalised F parameters
    param_GF_i <- .genf_parameters(
      new_theta
    )
    
    
    # Regression coefficients
    coef_i <- new_theta[-(1:4)]
    
    
    # Acceleration factors for all profiles
    phi_i <- .aft_phi(
      X = X,
      coef = coef_i
    )
    
    
    # Calculate baseline quantities ONCE per parameter simulation followed by
    # vectorised AFT scaling.
    
    baseline_mean_i <- .genf_mean(
      param_GF_i
    )
    
    baseline_median_i <- .genf_median(
      param_GF_i
    )
    
    
    # AFT scaling:
    #
    # E(T | X)      = E(T0) / phi
    # Median(T | X) = Median(T0) / phi
    
    df_mean[, i] <-
      baseline_mean_i / phi_i
    
    df_median[, i] <-
      baseline_median_i / phi_i
  }
  
  
  # Point estimates
  
  param_GF <- .genf_parameters(
    theta
  )
  
  coef <- theta[-(1:4)]
  
  phi <- .aft_phi(
    X = X,
    coef = coef
  )
  
  
  baseline_mean <- .genf_mean(
    param_GF
  )
  
  baseline_median <- .genf_median(
    param_GF
  )
  
  
  mean_estimate <-
    baseline_mean / phi
  
  median_estimate <-
    baseline_median / phi
  
  
  # Simulation intervals
  
  probs <- c(
    alpha / 2,
    1 - alpha / 2
  )
  
  mean_quantile <- t(
    apply(
      df_mean,
      1,
      stats::quantile,
      probs = probs,
      names = FALSE
    )
  )
  
  median_quantile <- t(
    apply(
      df_median,
      1,
      stats::quantile,
      probs = probs,
      names = FALSE
    )
  )
  
  
  # Return
    
  list(
    res = tibble::tibble(
      mean = mean_estimate,
      median = median_estimate,
      
      mean_lb = mean_quantile[, 1],
      mean_ub = mean_quantile[, 2],
      
      median_lb = median_quantile[, 1],
      median_ub = median_quantile[, 2]
    ),
    
    df = list(
      mean = df_mean,
      median = df_median,
      param_GF = param_GF,
      phi = phi
    )
  )
}


# Conditional prediction --------------------------------------------------


#' Calculate conditional future median length of stay
#'
#' Calculate the remaining median LoS conditional on a resident having
#' already accumulated a specified amount of baseline-equivalent exposure
#' time.
#'
#' Let
#'
#' \deqn{
#' a = \text{past accumulated baseline time}.
#' }
#'
#' The conditional future survival function is
#'
#' \deqn{
#' S_f(t)
#' =
#' \frac{S_0(a + \phi t)}{S_0(a)}.
#' }
#'
#' The conditional median solves
#'
#' \deqn{
#' S_0(a + \phi t_{0.5})
#' =
#' 0.5 S_0(a).
#' }
#'
#' Hence it can be obtained directly from the baseline quantile function,
#' without numerical root finding.
#'
#' @param phi Numeric vector of current AFT acceleration factors.
#'
#' @param past_acc_time Non-negative scalar giving previously accumulated
#'   baseline-equivalent LoS.
#'
#' @param theta Numeric fitted parameter vector for the AFT model.
#'
#' @return Numeric vector of conditional future median LoS values, with one
#'   value for each element of `phi`.
#'
#' @export
conditional_future_median <- function(
    phi,
    past_acc_time,
    theta
) {
  
  if (
    length(past_acc_time) != 1L ||
    !is.finite(past_acc_time) ||
    past_acc_time < 0
  ) {
    stop(
      "`past_acc_time` must be a finite non-negative scalar.",
      call. = FALSE
    )
  }
  
  if (
    any(!is.finite(phi)) ||
    any(phi <= 0)
  ) {
    stop(
      "All values of `phi` must be finite and positive.",
      call. = FALSE
    )
  }
  
  
  param_GF <- .genf_parameters(theta)
  
  
  # Survival probability at the accumulated baseline time.
  survival_past <- 1 - pgenf(
    past_acc_time,
    param_GF[1],
    param_GF[2],
    param_GF[3],
    param_GF[4]
  )
  
  
  if (
    !is.finite(survival_past) ||
    survival_past <= 0
  ) {
    stop(
      "Baseline survival at `past_acc_time` is numerically zero or invalid.",
      call. = FALSE
    )
  }
  
  
  # We require:
  #
  # S0(q) = 0.5 * S0(past)
  #
  # therefore:
  #
  # F0(q) = 1 - 0.5 * S0(past)
  conditional_prob <- 1 - 0.5 * survival_past
  
  
  total_baseline_median <- qgenf(
    conditional_prob,
    param_GF[1],
    param_GF[2],
    param_GF[3],
    param_GF[4]
  )
  
  
  # total_baseline_median = past_acc_time + phi * future_time
  (total_baseline_median - past_acc_time) / phi
}



# Time-dependent future LoS -----------------------------------------------


#' Predict future LoS from a time-dependent AFT history
#'
#' Convert completed historical episodes into accumulated baseline-equivalent
#' time and calculate the conditional future median under one or more current
#' resident/facility profiles.
#'
#' For historical episode `j`, observed LoS is transformed according to
#'
#' \deqn{
#' a_j = \phi_j t_j.
#' }
#'
#' Total accumulated baseline time is therefore
#'
#' \deqn{
#' a = \sum_j \phi_j t_j.
#' }
#'
#' The current profile is then used to calculate its acceleration factor,
#' and the remaining median LoS is obtained conditional on survival to
#' baseline time `a`.
#'
#' @param episodes A data frame containing previous episodes. It must contain
#'   the standard AFT predictors and an `episode_los` column giving the
#'   observed LoS of each completed episode.
#'
#'   A zero-row data frame is allowed and represents a resident with no
#'   previous episode history.
#'
#' @param ind_profile A data frame containing the current resident/facility
#'   profile or profiles for which future LoS should be predicted.
#'
#' @param theta Numeric fitted parameter vector. The first four elements are
#'   the Generalised F parameters and the remaining elements are regression
#'   coefficients.
#'
#' @return The standardised `ind_profile`, with two additional columns:
#'
#' * `phi`: current AFT acceleration factor.
#' * `median`: conditional median future LoS.
#'
#' @export
future_los <- function(
    episodes,
    ind_profile,
    theta
) {
  
  if (!"episode_los" %in% names(episodes)) {
    stop(
      "`episodes` must contain an `episode_los` column.",
      call. = FALSE
    )
  }
  
  if (
    any(!is.finite(episodes$episode_los)) ||
    any(episodes$episode_los < 0)
  ) {
    stop(
      "`episode_los` must contain finite non-negative values.",
      call. = FALSE
    )
  }
  
  
  # Current profile
  
  profile_std <- .standardise_aft_data(
    ind_profile
  )
  
  X_current <- .aft_model_matrix(
    profile_std
  )
  
  
  coef <- theta[-(1:4)]
  
  if (length(coef) != ncol(X_current)) {
    stop(
      "Number of coefficients in `theta` does not match the model matrix.",
      call. = FALSE
    )
  }
  
  # Historical accumulated baseline time
  
  if (nrow(episodes) == 0L) {
    
    past_acc_time <- 0
    
  } else {
    
    los <- episodes$episode_los
    
    episode_predictors <- episodes[
      ,
      setdiff(names(episodes), "episode_los"),
      drop = FALSE
    ]
    
    X_episode <- .aft_model_matrix(
      episode_predictors
    )
    
    phi_episode <- .aft_phi(
      X = X_episode,
      coef = coef
    )
    
    past_acc_time <- sum(
      phi_episode * los
    )
  }
  
  
  # Current acceleration factor
  
  phi_current <- .aft_phi(
    X = X_current,
    coef = coef
  )
  
  # Conditional remaining median

  median_future <- conditional_future_median(
    phi = phi_current,
    past_acc_time = past_acc_time,
    theta = theta
  )
  
  # Output

  
  dplyr::bind_cols(
    tibble::as_tibble(profile_std),
    tibble::tibble(
      phi = phi_current,
      median = median_future
    )
  )
}