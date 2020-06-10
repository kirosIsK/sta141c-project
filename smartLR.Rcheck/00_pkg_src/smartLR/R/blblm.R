## quiets concerns of R CMD check re: the .'s that appear in pipelines
# from https://github.com/jennybc/googlesheets/blob/master/R/googlesheets.R
utils::globalVariables(c("."))


#' estimate the regression with Little Bag of Bootstraps
#'  It is strongly encouraged to use parallel when m and B is large
#'
#'  But Parallel method often faster than non-Parallel method regardless of m and B
#'   Since the Author attempts to use Parallel Computing as much places as he can...
#'
#' @param formula  formula for the regression
#' @param data     dataset for the regression
#' @param m        num of data to split to
#' @param B        length of Bootstraps
#' @param parallel True if to use parallel, True by Default
#' @export
blblm <- function(formula, data, m = 10, B = 5000, parallel = TRUE) {
  # Global Var isn't that bad acutally
  assign("para", parallel, envir = .GlobalEnv)

  # All information that need to know by a cluster
  all_inf <- c("formula","data","m","B","para","lm_each_subsample","lm_each_boot","smart_lm1","fastLmXPure","blbcoef","blbsigma","mean_lwr_upr","smart_map","smart_map_dbl","smart_map_mean","smart_map_cbind","smart_map_rbind")

  # Parallel Method used, detect num of cores and assign
  if (para == TRUE) {

    ## CRAN limits the number of cores available to packages to 2
    ##  This case is added completetly for CRAN test
    chk <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")

    if (nzchar(chk) && chk == "TRUE") {
      # use 2 cores in CRAN
      num_of_cores_to_use <- 2L
    }
    else {
      ## Generate Cluster
      num_of_cores_to_use <- as.integer(detectCores()/2)
    }
    CL <- makeCluster(num_of_cores_to_use)
    clusterExport(cl=CL, varlist= all_inf, envir=environment())
  }

  # Split Data
  data_list <- split_data(data, m)

  ## Here's some of my point of view
  ## Really, Really stuggle to make Cluster Global along with smart_map (see the end of the program)
  ## ^-------- Thank for some developers having a holy war on Global variable...Especially Static Global

  ## I would hardly use such code when I am writing C/C++
  ##  because I have to do GB Collection by myself (C, C++ is now much better)
  ## But the effect of such code and well mainstance of R has pursuaded me, R makes many things so easy

  ## Another reason I make such codes is because they look less clumsy....
  if (para == TRUE) assign("CL", CL, envir = .GlobalEnv)


  # Get BootStraps Estimate
  estimates <- smart_map(data_list, function(oj) {lm_each_subsample(formula = formula, data = oj, n = nrow(data), B = B)})
  head(estimates)

  # Get result
  res <- list(estimates = estimates, formula = formula)

  # Stop Cluster if parallel
  if (para == TRUE) stopCluster(CL)

  # Class blblm
  class(res) <- "blblm"
  invisible(res)
}


#' split data into m parts of approximated equal sizes
#'
#' @param data     dataset to split
#' @param m        num of data to split to
#' @export
split_data <- function(data, m) {
  idx <- sample.int(m, nrow(data), replace = TRUE)
  data %>% split(idx)
}


#' compute the estimates
#'
#' @param formula  formula for the regression
#' @param data     dataset for the regression
#' @param n        size of the objects that are put into K boxes in the typical multinomial experiment
#' @param B        length of Bootstraps
#' @export
lm_each_subsample <- function(formula, data, n, B) {
  replicate(B, lm_each_boot(formula, data, n), simplify = FALSE)
}


#' compute the regression estimates for a blb dataset
#'
#' @param formula  formula for the regression
#' @param data     dataset for the regression
#' @param n        size of the objects that are put into K boxes in the typical multinomial experiment
#' @export
lm_each_boot <- function(formula, data, n) {
  freqs <- rmultinom(1, n, rep(1, nrow(data)))
  freqs <- freqs %>% as.numeric()
  smart_lm1(formula, data, freqs)
}


#' estimate the regression estimates based on given the number of repetitions
#'  Automatically select Arma or lm.wfit based on the dimension of the dataset
#'
#' @param formula  formula for the regression
#' @param data     dataset for the regression
#' @param freqs    vector consist of weights for the regression
#' @export
smart_lm1 <- function(formula, data, freqs=replicate(nrow(data),1)) {
  # drop the original closure of formula,
  # otherwise the formula will pick a wront variable from the global scope.
  environment(formula) <- environment()

  # Avoid weak evaluation
  force(freqs)

  # Extract the data frame
  mf <- model.frame(formula, data)
  x  <- model.matrix(attr(mf, "terms"), data=mf)
  y  <- model.response(mf)

  ## IMPORTATNT!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ##  Automatically select Arma or lm.wfit based on the dimension of the dataset

  ## Doing this because lm works faster with dataset with larger dimension
  ##  Arma works faster dataset with smaller dimension

  ## Also, Pure is much faster than nonPure (i.e. fastLmPure ~ 0.29s, fastLm ~ 2.71s)
  ##  Since R is increbiliy fast in mancipating Data, we do data preparation part in R

  ## And leave the remainging job to Arma/lm()
  res <- smart_Pure(x, y, freqs)

  res$call <- match.call()
  res$model <- mf
  res$formula <- formula
  res$intercept <- attr(attr(mf, "terms"), "intercept")
  res$weights   <- freqs   # This code kinda bad, but fine...


  ## IMPORTANT !!!!!!!!!!!!!!!!!!!
  ##  Although lm.wfit does grant us speed
  ##   It doesn't register coef and fit values
  ##    We need to do it by ourself here

  ##  But We need no worry about the speed much affected
  ##   Since R is vey good at data manipulation
  if (is.null(coef(res))) {
    res$coefficients  <- as.vector(res[1])
    res$fitted.values <- as.vector(as.matrix(x) %*% unlist(res$coefficients))
    res$rank <- length(res[1])
  }
  list(coef = blbcoef(res), sigma = blbsigma(res))
}


#' get coefficients from fit
#' @param fit fit result
#' @export
blbcoef <- function(fit) {
  # Avoid Weak Evaluation
  force(fit)

  coef(fit)
}


#' compute sigma from fit
#' @param fit fit result
#' @export
blbsigma <- function(fit) {
  # Avoid Weak Evaluation
  force(fit)

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


##  Get 95% C.I
mean_lwr_upr <- function(x, level = 0.95) {
  alpha <- 1 - level
  c(fit = mean(x), quantile(x, c(alpha / 2, 1 - alpha / 2)) %>% set_names(c("lwr", "upr")))
}


##  Smart Map
smart_map      <- function(.x, .f, ...) {
  if(para==TRUE){
    # Avoid Weak Evaluation
    force(.x)
    force(.f)

    parLapply(CL, .x, .f, ...)
  } else {
    purrr::map(.x, .f, ...)
  }
}


##  Smart Map_DBL
smart_map_dbl  <- function(.x, .f, ...) {
  if(para==TRUE){
    # Avoid Weak Evaluation
    force(.x)
    force(.f)

    parLapply(CL, .x, .f, ...) %>% as.double()
  } else {
    purrr::map_dbl(.x, .f, ...)
  }
}


##  Smart Map Mean
smart_map_mean <- function(.x, .f, ...) {
  smart_map(.x, .f, ...) %>% reduce(`+`) / length(.x)
}


##  Smart Map Cbind
smart_map_cbind <- function(.x, .f, ...) {
  smart_map(.x, .f, ...) %>% reduce(cbind)
}


##  Smart Map Rbind
smart_map_rbind <- function(.x, .f, ...) {
  smart_map(.x, .f, ...) %>% reduce(rbind)
}
