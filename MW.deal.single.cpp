#include <Rcpp.h>
#include <string.h>
#include <iostream>

using namespace Rcpp;

// [[Rcpp::export]]
RcppExport SEXP MW_single(SEXP MWr, SEXP newMW) {
  Rcpp::List MW(MWr);
  Rcpp::List nMW(newMW);
  std::vector<std::string> fromlist = MW["from"];
  std::vector<std::string> tolist = MW["to"];
  std::vector<std::string> from = nMW["from"];
  std::vector<std::string> to = nMW["to"];
  
  std::string t = to.at(0);
  for (int j=0; j<fromlist.size(); j++){
    std::string f = fromlist.at(j);
    if (f.compare(t)==0){
      int unique = 0;
      for (int k=0; k<fromlist.size(); k++){
        std::string tmpf = fromlist.at(k);
        std::string tmpt = tolist.at(k);
        if (tmpf.compare(from.at(0))==0 && tmpt.compare(tolist.at(j))==0){
          unique = 1;
          break;
        }
      }
      if (unique==0){
        from.push_back(from.at(0));
        to.push_back(tolist.at(j));
      }
    }
  }
  return Rcpp::List::create(Rcpp::Named("from") = from,
                            Rcpp::Named("to") = to);
}
