#include <Rcpp.h>
#include <string.h>
#include <iostream>

using namespace Rcpp;

// [[Rcpp::export]]
RcppExport SEXP MB_single(SEXP MBr, SEXP MWr, SEXP newMB) {
  Rcpp::List MB(MBr);
  std::vector<std::string> Bfrom = MB["from"];
  std::vector<std::string> Bto = MB["to"];
  
  Rcpp::List MW(MWr);
  std::vector<std::string> Wfrom = MW["from"];
  std::vector<std::string> Wto = MW["to"];
  
  Rcpp::List nMB(newMB);
  std::vector<std::string> from = nMB["from"];
  std::vector<std::string> to = nMB["to"];
  
  std::string bt = to.at(0);
  for (int j=0; j<Wfrom.size(); j++){
    std::string wt = Wto.at(j);
    if (wt.compare(bt)==0){
      int unique = 0;
      for (int k=0; k<Bfrom.size(); k++){
        std::string tmpf = Bfrom.at(k);
        std::string tmpt = Bto.at(k);
        if (tmpf.compare(from.at(0))==0 && tmpt.compare(Wfrom.at(j))==0){
          unique = 1;
          break;
        }
      }
      if (unique==0){
        from.push_back(from.at(0));
        to.push_back(Wfrom.at(j));
      }
    }
  }
  return Rcpp::List::create(Rcpp::Named("from") = from,
                            Rcpp::Named("to") = to);
}
