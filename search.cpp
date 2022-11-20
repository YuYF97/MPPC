#include <Rcpp.h>
#include <string.h>
#include <iostream>

using namespace Rcpp;

// [[Rcpp::export]]
RcppExport SEXP search(SEXP orderr, SEXP kwr) {
  Rcpp::List order(orderr);
  std::vector<std::string> ofrom = order["from"];
  std::vector<std::string> oto = order["to"];
  
  Rcpp::List kw(kwr);
  std::vector<std::string> kfrom = kw["from"];
  std::vector<std::string> kto = kw["to"];
  
  std::vector<int> korder;
  int i=0;
  while (i<kfrom.size()){
    std::string kf = kfrom.at(i);
    std::string kt = kto.at(i);
    int j=0;
    while (j<ofrom.size()){
      std::string of = ofrom.at(j);
      std::string ot = oto.at(j);
      if ((kf.compare(of)==0 && kt.compare(ot)==0) ||
          (kf.compare(ot)==0 && kt.compare(of)==0)){ //in both W and B
        korder.push_back(j+1);
        break;
      } else {
        j=j+1;
      }
    }
    i=i+1;
  }
  return wrap(korder);
}

