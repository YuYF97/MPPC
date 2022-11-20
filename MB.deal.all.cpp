#include <Rcpp.h>
#include <string.h>
#include <iostream>

using namespace Rcpp;

// [[Rcpp::export]]
RcppExport SEXP MB_all(SEXP MBr, SEXP MWr) {
  Rcpp::List MB(MBr);
  std::vector<std::string> Bfrom = MB["from"];
  std::vector<std::string> Bto = MB["to"];
  
  Rcpp::List MW(MWr);
  std::vector<std::string> Wfrom = MW["from"];
  std::vector<std::string> Wto = MW["to"];
  
  int i = 0;
  while (i<Bfrom.size()) {
    std::string bt = Bto.at(i);
    for (int j=0; j<Wfrom.size(); j++){
      std::string wt = Wto.at(j);
      if (wt.compare(bt)==0){
        int unique = 0;
        for (int k=0; k<Bfrom.size(); k++){
          std::string tmpf = Bfrom.at(k);
          std::string tmpt = Bto.at(k);
          if (tmpf.compare(Bfrom.at(i))==0 && tmpt.compare(Wfrom.at(j))==0){
            unique = 1;
            break;
          }
        }
        if (unique==0){
          Bfrom.push_back(Bfrom.at(i));
          Bto.push_back(Wfrom.at(j));
        }
      }
    }
    i = i+1;
  }
  return Rcpp::List::create(Rcpp::Named("from") = Bfrom,
                            Rcpp::Named("to") = Bto);
}

