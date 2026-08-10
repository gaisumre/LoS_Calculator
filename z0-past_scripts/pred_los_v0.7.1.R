source("genf.R")

predict_los <- function(new_data, theta, vStdErrors, alpha = 0.05, n_sim = 1000,
                        seed = 20240522) {
  
  df <- new_data %>% 
    dplyr::mutate(
      SEX = factor(SEX, c("F", "M")),
      ADM_AGE_GROUP = factor(ADM_AGE_GROUP,
                             c("85-89", "50-54", "55-59", "60-64", "65-69",
                               "70-74", "75-79", "80-84", "90-94", "95-99",
                               "100+")),
      COUNTRY_OF_BIRTH = factor(COUNTRY_OF_BIRTH, c("AUS", "OTHER")),
      PREFERRED_LANGUAGE = factor(PREFERRED_LANGUAGE, c("eng", "other")),
      ORGANISATION_TYPE = factor(ORGANISATION_TYPE,
                                 c("Not-for-profit", "Government", "Private")),
      SERVICE_SIZE = factor(SERVICE_SIZE, c("100+", "0-20", "21-40",
                                            "41-60", "61-80", "81-100")),
      ACPR_SES = factor(ACPR_SES, c("Q5", "Q1", "Q2", "Q3", "Q4")),
      REMOTENESS = factor(REMOTENESS,
                          c("Major Cities", "Inner Regional",
                            "Outer Regional/Remote/Very Remote")),
      STATE = factor(STATE, c("NSW", "VIC", "QLD", "WA", "SA", "TAS", "ACT", "NT"))
    )
  
  formula <- ~ SEX + ADM_AGE_GROUP + COUNTRY_OF_BIRTH + PREFERRED_LANGUAGE +
    ORGANISATION_TYPE + SERVICE_SIZE + ACPR_SES + REMOTENESS + STATE 
  
  X <- model.matrix(formula, data = df)[, -1L, drop = FALSE]
  
  n <- nrow(X)
  
  df_mean <- matrix(NA_real_, n, n_sim)
  df_median <- matrix(NA_real_, n, n_sim)
  
  set.seed(seed)
  
  for (i in seq_len(n_sim)) {
    new_theta <- rnorm(length(theta), theta, vStdErrors)
    
    param_GF <- new_theta[1:4]
    param_GF[2] <- exp(param_GF[2])
    param_GF[4] <- exp(param_GF[4])
    
    coef <- new_theta[-(1:4)]
    
    phi <- exp(drop(X %*% (-coef)))
    
    Fgen <- function(x, phi) {
      pgenf(
        x * phi,
        param_GF[1], param_GF[2],
        param_GF[3], param_GF[4]
      )
    }
    
    ub <- qgenf(
      0.999999,
      param_GF[1], param_GF[2],
      param_GF[3], param_GF[4]
    ) / min(phi)
    
    df_mean[, i] <- vapply(phi, function(ph) {
      integrate(function(x) 1 - Fgen(x, ph), 0, Inf)$value
    }, numeric(1))
    
    df_median[, i] <- vapply(phi, function(ph) {
      uniroot(function(x) Fgen(x, ph) - 0.5, c(0, ub))$root
    }, numeric(1))
  }
  
  param_GF <- theta[1:4]
  param_GF[2] <- exp(param_GF[2])
  param_GF[4] <- exp(param_GF[4])
  
  coef <- theta[-(1:4)]
  phi <- exp(drop(X %*% (-coef)))
  
  Fgen <- function(x, phi) {
    pgenf(
      x * phi,
      param_GF[1], param_GF[2],
      param_GF[3], param_GF[4]
    )
  }
  
  ub <- qgenf(
    0.9999,
    param_GF[1], param_GF[2],
    param_GF[3], param_GF[4]
  ) / min(phi)
  
  mean_quantile <- t(apply(df_mean, 1, quantile, c(alpha/2, 1-alpha/2)))
  median_quantile <- t(apply(df_median, 1, quantile, c(alpha/2, 1-alpha/2)))
  
  list(
    res = tibble::tibble(
      mean = vapply(phi, function(ph) {
        integrate(function(x) 1 - Fgen(x, ph), 0, Inf)$value
      }, numeric(1)),
      median = vapply(phi, function(ph) {
        uniroot(function(x) Fgen(x, ph) - 0.5, c(0, ub))$root
      }, numeric(1)),
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

# Note 1 ----
# Implementation of conditional future median
conditional_future_median <- function(phi, past_acc_time, theta) {
  
  param_GF <- theta[1:4]
  param_GF[2] <- exp(param_GF[2])
  param_GF[4] <- exp(param_GF[4])
  
  # baseline survival function
  S0 <- function(t) 1 - pgenf(
    t,
    param_GF[1], param_GF[2], param_GF[3], param_GF[4]
  )
  
  
  Sf <- function(t) {
    S0(past_acc_time + phi * t) / S0(past_acc_time)
  }
  
  # objective function of root-finding algorithm
  f <- function(u) Sf(u) - 0.5
  
  # upper limit for root-finding
  upper <- 1
  while (f(upper) > 0 && upper < 500) {
    upper <- upper * 2
  }
  
  
  uniroot(f, lower = 0, upper = upper)$root
}

# Compute future LoS:
# Consolidate individual episode data and individual profile data, and
# standardise them.
future_los <- function(episodes, ind_profile, theta) {

  los <- episodes$episode_los
  
  episodes <- episodes %>% 
    dplyr::select(-episode_los) %>% 
    dplyr::mutate(
      SEX = factor(SEX, c("F", "M")),
      ADM_AGE_GROUP = factor(ADM_AGE_GROUP,
                             c("85-89", "50-54", "55-59", "60-64", "65-69",
                               "70-74", "75-79", "80-84", "90-94", "95-99",
                               "100+")),
      COUNTRY_OF_BIRTH = factor(COUNTRY_OF_BIRTH, c("AUS", "OTHER")),
      PREFERRED_LANGUAGE = factor(PREFERRED_LANGUAGE, c("eng", "other")),
      ORGANISATION_TYPE = factor(ORGANISATION_TYPE,
                                 c("Not-for-profit", "Government", "Private")),
      SERVICE_SIZE = factor(SERVICE_SIZE, c("100+", "0-20", "21-40",
                                            "41-60", "61-80", "81-100")),
      ACPR_SES = factor(ACPR_SES, c("Q5", "Q1", "Q2", "Q3", "Q4")),
      REMOTENESS = factor(REMOTENESS,
                          c("Major Cities", "Inner Regional",
                            "Outer Regional/Remote/Very Remote")),
      STATE = factor(STATE, c("NSW", "VIC", "QLD", "WA", "SA", "TAS", "ACT", "NT"))
    )
  
  ind_profile <- ind_profile %>% 
    dplyr::mutate(
      SEX = factor(SEX, c("F", "M")),
      ADM_AGE_GROUP = factor(ADM_AGE_GROUP,
                             c("85-89", "50-54", "55-59", "60-64", "65-69",
                               "70-74", "75-79", "80-84", "90-94", "95-99",
                               "100+")),
      COUNTRY_OF_BIRTH = factor(COUNTRY_OF_BIRTH, c("AUS", "OTHER")),
      PREFERRED_LANGUAGE = factor(PREFERRED_LANGUAGE, c("eng", "other")),
      ORGANISATION_TYPE = factor(ORGANISATION_TYPE,
                                 c("Not-for-profit", "Government", "Private")),
      SERVICE_SIZE = factor(SERVICE_SIZE, c("100+", "0-20", "21-40",
                                            "41-60", "61-80", "81-100")),
      ACPR_SES = factor(ACPR_SES, c("Q5", "Q1", "Q2", "Q3", "Q4")),
      REMOTENESS = factor(REMOTENESS,
                          c("Major Cities", "Inner Regional",
                            "Outer Regional/Remote/Very Remote")),
      STATE = factor(STATE, c("NSW", "VIC", "QLD", "WA", "SA", "TAS", "ACT", "NT"))
    )
  
  formula <- ~ SEX + ADM_AGE_GROUP + COUNTRY_OF_BIRTH + PREFERRED_LANGUAGE +
    ORGANISATION_TYPE + SERVICE_SIZE + ACPR_SES + REMOTENESS + STATE 
  
  X_episode <- model.matrix(formula, data = episodes)[, -1L, drop = FALSE]
  X <- model.matrix(formula, data = ind_profile)[, -1L, drop = FALSE]
  
  param_GF <- theta[1:4]
  param_GF[2] <- exp(param_GF[2])
  param_GF[4] <- exp(param_GF[4])
  coef <- theta[-(1:4)]
    
  if (nrow(episodes) == 0) {
    past_acc_time <- 0
  } else {
    # print(los)
    # print(coef)
    # print(X_episode)
    past_acc_time <- sum(exp(drop(X_episode %*% (-coef))) * los)
    
    # print("# ----------------------------------------------------#")
  }
  
  phi_vec <- exp(drop(X %*% (-coef)))
  
  median_vec <- vapply(
    phi_vec,
    function(phi_i) {
      conditional_future_median(phi_i, past_acc_time, theta)
    },
    numeric(1)
  )
  
  ind_profile %>% 
    dplyr::bind_cols(
      tibble::tibble(
        phi = phi_vec,
        median = median_vec
      )
    )
}

