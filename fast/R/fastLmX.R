#' A description of fastLmX
#'
#' A details of fastLmX
#'
#' @title fastLmX: Weighted Regression Rewritten using RCpp
#' @param X a data.frame consists of explantory variables
#' @param Y a Vector consists of output variables
#' @param Z a Vector consists of weights
#' @examples
#' class(a) <- "data.frame"
#' class(b) <- "numeric"
#' class(c) <- "numeric"
#' fastLmX(a, b, c)
#'
#' @rdname fastLmX
#' @export
fastLmX <- function(X, ...) UseMethod("fastLmX")


fastLmXPure <- function(X, Y, Z) {

  stopifnot(is.matrix(X), is.numeric(Y), is.numeric(Z), nrow(Y)==nrow(X), nrow(Y)==nrow(Z))

  .Call('_fast_fastLmX_impl', PACKAGE = 'fast', X, Y, Z)

}


#' @rdname fastLmX
#' @export
fastLmX.default <- function(X, Y, Z= replicate(length(y),1)) {

  if (is.matrix(X)  == FALSE) X <- as.matrix(X)
  if (is.numeric(Y) == FALSE) Y <- as.numeric(Y)

  res <- fastLmXPure(X, Y, Z)

  res$coefficients  <- as.vector(res$coefficient)
  res$fitted.values <- as.vector(X %*% res$coefficients)
  res$residuals     <- Y - res$fitted.values

  class(res) <- "fastLmX"
  res
}


#' @rdname fastLmX
#' @export
print.fastLmX <- function(x, ...) {
  cat("\nCall:\n")
  print(x$call)
  cat("\nCoefficients:\n")
  print(x$coefficients, digits=5)
}


#' @rdname fastLmX
#' @export
summary.fastLmX <- function(object, ...) {
  se <- object$stderr
  tval <- coef(object)/se

  TAB <- cbind(Estimate = coef(object),
               StdErr = se,
               t.value = tval,
               p.value = 2*pt(-abs(tval), df=object$df))

  # why do I need this here?
  rownames(TAB) <- names(object$coefficients)
  colnames(TAB) <- c("Estimate", "StdErr", "t.value", "p.value")

  ## cf src/library/stats/R/lm.R and case with no weights and an intercept
  f <- object$fitted.values
  r <- object$residuals
  #mss <- sum((f - mean(f))^2)
  mss <- if (object$intercept) sum((f - mean(f))^2) else sum(f^2)
  rss <- sum(r^2)

  r.squared <- mss/(mss + rss)
  df.int <- if (object$intercept) 1L else 0L

  n <- length(f)
  rdf <- object$df
  adj.r.squared <- 1 - (1 - r.squared) * ((n - df.int)/rdf)

  res <- list(call=object$call,
              coefficients=TAB,
              r.squared=r.squared,
              adj.r.squared=adj.r.squared,
              sigma=sqrt(sum((object$residuals)^2)/rdf),
              df=object$df,
              residSum=summary(object$residuals, digits=5)[-4])

  class(res) <- "summary.fastLm"
  res
}


#' @rdname fastLmX
#' @export
fastLmX.formula <- function(formula, data=list(), ...) {
  mf <- model.frame(formula, data)
  x  <- model.matrix(attr(mf, "terms"), data=mf)
  y  <- model.response(mf)

  res           <- fastLmX.default(x, y, ...)
  res$call      <- match.call()
  res$model     <- mf
  res$formula   <- formula
  res$intercept <- attr(attr(mf, "terms"), "intercept")
  res
}


#' @rdname fastLmX
#' @export
predict.fastLmX <- function(object, newdata=NULL, ...) {
  if (is.null(newdata)) {
    y <- fitted(object)
  } else {
    if (!is.null(object$formula)) {
      x <- model.matrix(object$formula, newdata)
    } else {
      x <- newdata
    }
    y <- as.vector(x %*% coef(object))
  }
  y
}
