#include <Rcpp.h>
#include <string.h>
#include <iostream>

using namespace Rcpp;

// [[Rcpp::export]]
RcppExport SEXP MW_all(SEXP MWr) {
  Rcpp::List MW(MWr);
  std::vector<std::string> fromlist = MW["from"];
  std::vector<std::string> tolist = MW["to"];
  
  int i = 0;
  while (i<fromlist.size()) {
    std::string t = tolist.at(i);
    for (int j=0; j<fromlist.size(); j++){
      std::string f = fromlist.at(j);
      if (f.compare(t)==0){
        int unique = 0;
        for (int k=0; k<fromlist.size(); k++){
          std::string tmpf = fromlist.at(k);
          std::string tmpt = tolist.at(k);
          if (tmpf.compare(fromlist.at(i))==0 && tmpt.compare(tolist.at(j))==0){
            unique = 1;
            break;
          }
        }
        if (unique==0){
          fromlist.push_back(fromlist.at(i));
          tolist.push_back(tolist.at(j));
        }
      }
    }
    i = i+1;
  }
  return Rcpp::List::create(Rcpp::Named("from") = fromlist,
                            Rcpp::Named("to") = tolist);
}

