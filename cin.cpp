#include <RcppArmadillo.h>
#include <Rcpp.h>
#include <string.h>
#include <iostream>
#include <map>

using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]

RcppExport SEXP cin(arma::vec x, arma::vec y, arma::mat z) {
  //select col
  arma::mat datasel;
  datasel = join_horiz(x, y);
  datasel = join_horiz(datasel, z);
  //pcor
  arma::mat covmat = arma::cov(datasel);
  arma::mat invcov = arma::inv(covmat);
  double r = -invcov(0,1)/sqrt(invcov(0,0) * invcov(1,1));
  double df = datasel.n_rows - 2 - (datasel.n_cols-2);
  double t = r * sqrt(df)/sqrt(1 - r * r);
  double p = 2 * R::pt(-abs(t), df, 1, 0);
  return wrap(p);
}
