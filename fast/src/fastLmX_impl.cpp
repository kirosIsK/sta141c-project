#include <RcppArmadillo.h>

using namespace Rcpp;

//' estimate the regression estimates based on given the number of repetitions
//'
//' @param x a Matrix
//' @param Y a Numeric Vector
//' @param Z a Numeric Vector for weight
//' @export
// [[Rcpp::export]]
List fastLmX_impl(const arma::mat& X, const arma::colvec& Y, const arma::colvec& Z) {
  int n = X.n_rows, k = X.n_cols;

  // slope coefficients
  // X^T * (W)^1/2 * (W)^1/2 * X = X^T * (W)^1/2 * (W)^1/2 * Y
  arma::mat diag_Z    = diagmat(sqrt(Z));
  arma::mat XStar     = diag_Z * X;
  arma::mat YStar     = diag_Z * Y;
  arma::colvec coef   = arma::solve(XStar,YStar);

  // residuals
  arma::colvec res    = Y - X*coef;

  // std.errors of coefficients
  double s2 = std::inner_product(res.begin(), res.end(), res.begin(), 0.0)/(n - k);
  arma::colvec std_err = arma::sqrt(s2 * arma::diagvec(arma::pinv(arma::trans(X)*X)));


  return List::create(Named("coefficients") = coef,
                      Named("stderr")       = std_err,
                      Named("df.residual")  = n - k);
}


