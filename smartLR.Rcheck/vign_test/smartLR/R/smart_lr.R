#' smart Linear Regression that automatically select
#'  Armadillo method or lm method based on
#'  dimension of the dataset
#'
#' @param formula  formula for the regression
#' @param data     dataset for the regression
#' @param weight   vector for weight
#' @export
smart_lr <- function(formula, data, weight) {

  # Avoid weak evaluation
  force(weight)

  # Get dimension
  r = nrow(data); c = ncol(data)

  # Select Armadillo method if col or row is less than 300
  arma_method = ifelse((r < 300 && c < 300), TRUE, FALSE)

  # Automatically execute Arma method or lm method
  ifelse((arma_method == TRUE), fastLmX(formula, data, weight), lm(formula= formula, data = data, weights = weight))
 }


#' smart Pure method of Linear Regression that automatically select
#'  Armadillo method or lm.wfit method based on
#'  dimension of the dataset
#'
#' @param X a matrix for all explantory variables
#' @param Y a vector for output variables
#' @param Z a vector for weight
#' @export
smart_Pure <- function(X, Y, Z) {

  # Avoid weak evaluation
  force(Z)

  # Stop if data type is incorrect
  stopifnot(is.matrix(X), is.numeric(Y), is.numeric(Z), nrow(Y)==nrow(X), nrow(Y)==nrow(Z))

  # Get dimension
  r = nrow(X); c = ncol(X)

  # Select Armadillo method if col or row is less than 300
  arma_method = ifelse((r < 300 && c < 300), TRUE, FALSE)

  # Automatically execute Arma method or lm method
  ifelse((arma_method == TRUE), fastLmXPure(X, Y, Z), lm.wfit(X, Y, Z))
}


