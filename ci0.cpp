#include <RcppArmadillo.h>
#include <Rcpp.h>
#include <string.h>
#include <iostream>
#include <map>

using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]

RcppExport SEXP ci0(arma::vec x, arma::vec y) {
  arma::vec r = arma::cor(x, y, 1);
  double t = abs(r(0))/sqrt((1-pow(r(0),2))/(x.n_elem-2));
  double p = 2 * R::pt(-abs(t), (x.n_elem-2), 1, 0);
  return wrap(p);
}
