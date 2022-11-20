#include <Rcpp.h>
#include <string.h>
#include <iostream>

using namespace Rcpp;

// [[Rcpp::export]]
RcppExport SEXP innerconflict(List kw, std::vector<std::string> newkw) {
  std::vector<std::string> from = kw["from"];
  std::vector<std::string> to = kw["to"];
  
  std::string nf = newkw.at(0);
  std::string nt = newkw.at(1);
  
  int logi = 0;
  
  for(int i=0; i<from.size(); i++) {
    std::string f = from.at(i);
    std::string t = to.at(i);
    if ((nf.compare(f)==0 && nt.compare(t)==0) ||
        (nf.compare(t)==0 && nt.compare(f)==0)) {
      logi = 1;
      break;
    }
  }
  
  return wrap(logi);
}

