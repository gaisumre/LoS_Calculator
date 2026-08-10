# Generalised F distribution functions
# Parameterisation follows flexsurv / the supplied MATLAB functions.
# Constraints: sigma > 0, P > 0

pgenf <- function(q, mu, sigma, Q, P,
                  lower.tail = TRUE, log.p = FALSE) {
  
  if (length(sigma) != 1L || !is.finite(sigma) || sigma <= 0) {
    stop("'sigma' must be a finite scalar greater than 0.")
  }
  
  if (length(P) != 1L || !is.finite(P) || P <= 0) {
    stop("'P' must be a finite scalar greater than 0.")
  }
  
  d <- sqrt(Q^2 + 2 * P)
  
  s1 <- 2 / (Q^2 + 2 * P + Q * d)
  s2 <- 2 / (Q^2 + 2 * P - Q * d)
  
  result <- rep(NA_real_, length(q))
  
  result[q <= 0 & !is.na(q)] <- 0
  result[is.infinite(q) & q > 0] <- 1
  
  valid <- is.finite(q) & q > 0
  
  if (any(valid)) {
    w <- (log(q[valid]) - mu) * d / sigma
    
    # More numerically stable than computing exp(w) directly:
    # x = s2 / (s2 + s1 * exp(w))
    log_ratio <- log(s1 / s2) + w
    beta_x <- stats::plogis(-log_ratio)
    
    result[valid] <- stats::pbeta(
      beta_x,
      shape1 = s2,
      shape2 = s1,
      lower.tail = FALSE
    )
  }
  
  if (!lower.tail) {
    result <- 1 - result
  }
  
  if (log.p) {
    result <- log(result)
  }
  
  result
}


dgenf <- function(x, mu, sigma, Q, P, log = FALSE) {
  
  if (length(sigma) != 1L || !is.finite(sigma) || sigma <= 0) {
    stop("'sigma' must be a finite scalar greater than 0.")
  }
  
  if (length(P) != 1L || !is.finite(P) || P <= 0) {
    stop("'P' must be a finite scalar greater than 0.")
  }
  
  d <- sqrt(Q^2 + 2 * P)
  
  s1 <- 2 / (Q^2 + 2 * P + Q * d)
  s2 <- 2 / (Q^2 + 2 * P - Q * d)
  
  log_density <- rep(NaN, length(x))
  log_density[is.na(x)] <- NA_real_
  
  valid <- is.finite(x) & x > 0
  
  if (any(valid)) {
    w <- (log(x[valid]) - mu) * d / sigma
    
    # log(1 + s1 * exp(w) / s2), evaluated stably
    z <- log(s1 / s2) + w
    log_denom_term <- ifelse(
      z > 0,
      z + log1p(exp(-z)),
      log1p(exp(z))
    )
    
    log_density[valid] <-
      log(d) +
      s1 * log(s1 / s2) +
      s1 * w -
      log(sigma) -
      log(x[valid]) -
      (s1 + s2) * log_denom_term -
      lbeta(s1, s2)
  }
  
  if (log) {
    log_density
  } else {
    exp(log_density)
  }
}


qgenf <- function(p, mu, sigma, Q, P,
                  lower.tail = TRUE, log.p = FALSE) {
  
  if (length(sigma) != 1L || !is.finite(sigma) || sigma <= 0) {
    stop("'sigma' must be a finite scalar greater than 0.")
  }
  
  if (length(P) != 1L || !is.finite(P) || P <= 0) {
    stop("'P' must be a finite scalar greater than 0.")
  }
  
  if (log.p) {
    p <- exp(p)
  }
  
  if (any(p < 0 | p > 1, na.rm = TRUE)) {
    stop("'p' must contain probabilities between 0 and 1.")
  }
  
  d <- sqrt(Q^2 + 2 * P)
  
  s1 <- 2 / (Q^2 + 2 * P + Q * d)
  s2 <- 2 / (Q^2 + 2 * P - Q * d)
  
  # pgenf() uses the upper tail of Beta(s2, s1).
  beta_x <- stats::qbeta(
    p,
    shape1 = s2,
    shape2 = s1,
    lower.tail = !lower.tail
  )
  
  # From:
  # beta_x = s2 / (s2 + s1 * exp(w))
  #
  # exp(w) = s2 * (1 - beta_x) / (s1 * beta_x)
  #
  # log(q) = mu + sigma * w / d
  
  log_exp_w <-
    log(s2) +
    log1p(-beta_x) -
    log(s1) -
    log(beta_x)
  
  exp(mu + sigma * log_exp_w / d)
}


rgenf <- function(n, mu, sigma, Q, P) {
  
  if (length(n) > 1L) {
    n <- length(n)
  }
  
  n <- as.integer(n)
  
  if (is.na(n) || n < 0L) {
    stop("'n' must be a non-negative integer.")
  }
  
  if (n == 0L) {
    return(numeric(0))
  }
  
  qgenf(
    p = stats::runif(n),
    mu = mu,
    sigma = sigma,
    Q = Q,
    P = P
  )
}