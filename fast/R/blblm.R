#' @import furrr
#' @import parallel
#' @import purrr
#' @import stats
#' @import future
#' @importFrom magrittr %>%
#' @details
#' Linear Regression with Little Bag of Bootstraps
"_PACKAGE"


## quiets concerns of R CMD check re: the .'s that appear in pipelines
# from https://github.com/jennybc/googlesheets/blob/master/R/googlesheets.R
utils::globalVariables(c("."))


#' @export
blblm <- function(formula, data, m = 10, B = 5000, parallel = TRUE) {
  # Global Var isn't that bad acutally
  assign("para", parallel, envir = .GlobalEnv)

  # Parallel Method used, detect num of cores and assign
  if (para == TRUE) {
    num_of_cores_to_use <- as.integer(detectCores()/2)
    cl <- makeCluster(num_of_cores_to_use)
  }

  # Split Data
  data_list <- split_data(data, m)

  # Estimate
  #  A reason I don't use smart_map over here is because
  #   It seems that there may be problem with lazy evaluation when
  #   I try to parLapply over here
  estimates <- map(
    data_list,
    ~ lm_each_subsample(formula = formula, data = ., n = nrow(data), B = B))
  res <- list(estimates = estimates, formula = formula)

  # Stop Cluster if parallel
  if (para == TRUE) stopCluster(cl)

  # Class blblm
  class(res) <- "blblm"
  invisible(res)
}


#' split data into m parts of approximated equal sizes
split_data <- function(data, m) {
  idx <- sample.int(m, nrow(data), replace = TRUE)
  data %>% split(idx)
}


#' compute the estimates
lm_each_subsample <- function(formula, data, n, B) {
  replicate(B, lm_each_boot(formula, data, n), simplify = FALSE)
}


#' compute the regression estimates for a blb dataset
lm_each_boot <- function(formula, data, n) {
  freqs <- rmultinom(1, n, rep(1, nrow(data)))
  lm1_X(formula, data, freqs)
}


#' estimate the regression estimates based on given the number of repetitions
lm1_X <- function(formula, data, freqs) {
  # drop the original closure of formula,
  # otherwise the formula will pick a wront variable from the global scope.
  environment(formula) <- environment()

  ##-- THIS IS SUPER IMPORTANT!!!!
  # I don't know why, but calling the function by S3 method is much slower than
  #  called the C++ compiled code directly (It just a few lines more)
  mf <- model.frame(formula, data)
  x  <- model.matrix(attr(mf, "terms"), data=mf)
  y  <- model.response(mf)

  ## So here you will see I call fastLmXPure instead fastLmX
  ##  where fastLmX should use function and data as argument
  ##  I just do it over here
  ##  But faster LOL
  res <- fastLmXPure(x, y, freqs)
  res$call <- match.call()
  res$model <- mf
  res$formula <- formula
  res$intercept <- attr(attr(mf, "terms"), "intercept")

  list(coef = blbcoef(res), sigma = blbsigma(res))
}


#' compute the coefficients from fit
blbcoef <- function(fit) {
  coef(fit)
}


#' compute sigma from fit
blbsigma <- function(fit) {
  p <- fit$rank
  y <- model.extract(fit$model, "response")
  e <- fitted(fit) - y
  w <- fit$weights
  sqrt(sum(w * (e^2)) / (sum(w) - p))
}


#' @export
#' @method print blblm
print.blblm <- function(x, ...) {
  cat("blblm model:", capture.output(x$formula))
  cat("\n")
}


#' @export
#' @method sigma blblm
sigma.blblm <- function(object, confidence = FALSE, level = 0.95, ...) {
  est <- object$estimates
  sigma <- mean(smart_map_dbl(est, ~ mean(smart_map_dbl(., "sigma"))))
  if (confidence) {
    alpha <- 1 - 0.95
    limits <- est %>%
      smart_map_mean(~ quantile(smart_map_dbl(., "sigma"), c(alpha / 2, 1 - alpha / 2))) %>%
      set_names(NULL)
    return(c(sigma = sigma, lwr = limits[1], upr = limits[2]))
  } else {
    return(sigma)
  }
}


#' @export
#' @method coef blblm
coef.blblm <- function(object, ...) {
  est <- object$estimates
  smart_map_mean(est, ~ smart_map_cbind(., "coef") %>% rowMeans())
}


#' @export
#' @method confint blblm
confint.blblm <- function(object, parm = NULL, level = 0.95, ...) {
  if (is.null(parm)) {
    parm <- attr(terms(fit$formula), "term.labels")
  }
  alpha <- 1 - level
  est <- object$estimates
  out <- smart_map_rbind(parm, function(p) {
    smart_map_mean(est, ~ smart_map_dbl(., list("coef", p)) %>% quantile(c(alpha / 2, 1 - alpha / 2)))
  })
  if (is.vector(out)) {
    out <- as.matrix(t(out))
  }
  dimnames(out)[[1]] <- parm
  out
}


#' @export
#' @method predict blblm
predict.blblm <- function(object, new_data, confidence = FALSE, level = 0.95, ...) {
  est <- object$estimates
  X <- model.matrix(reformulate(attr(terms(object$formula), "term.labels")), new_data)
  if (confidence) {
    smart_map_mean(est, ~ smart_map_cbind(., ~ X %*% .$coef) %>%
               apply(1, mean_lwr_upr, level = level) %>%
               t())
  } else {
    smart_map_mean(est, ~ smart_map_cbind(., ~ X %*% .$coef) %>% rowMeans())
  }
}


mean_lwr_upr <- function(x, level = 0.95) {
  alpha <- 1 - level
  c(fit = mean(x), quantile(x, c(alpha / 2, 1 - alpha / 2)) %>% set_names(c("lwr", "upr")))
}


smart_map      <- function(.x, .f, ...) {
  ifelse(para==TRUE, parLapply(cl, .x, .f, ...), purrr::map(.x, .f, ...))
}


smart_map_dbl  <- function(.x, .f, ...) {
  ifelse(para==TRUE, parLapply(cl, .x, .f, ...) %>% as.double(), purrr::map_dbl(.x, .f, ...))
}


smart_map_mean <- function(.x, .f, ...) {
  smart_map(.x, .f, ...) %>% reduce(`+`) / length(.x)
}


smart_map_cbind <- function(.x, .f, ...) {
  smart_map(.x, .f, ...) %>% reduce(cbind)
}

smart_map_rbind <- function(.x, .f, ...) {
  smart_map(.x, .f, ...) %>% reduce(rbind)
}
