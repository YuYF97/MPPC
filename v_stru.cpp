#include <RcppArmadillo.h>
#include <Rcpp.h>
#include <string.h>
#include <iostream>
#include <map>

using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]

RcppExport SEXP v_stru(List order, List triple, List Sepset) {
  std::vector<std::string> ofrom = order["from"];
  std::vector<std::string> oto = order["to"];
  
  std::vector<std::string> from = triple["from"];
  std::vector<std::string> med = triple["med"];
  std::vector<std::string> to = triple["to"];
  
  std::vector<int> vstr;
  
  for (int i=0; i<from.size(); i++) {
    std::string f = from.at(i);
    std::string t = to.at(i);
    std::string m = med.at(i);
    int pos;
    for (int j=0; j<ofrom.size(); j++) {
      if ((f.compare(ofrom.at(j))==0 & t.compare(oto.at(j))==0) | 
          (f.compare(oto.at(j))==0 & t.compare(ofrom.at(j))==0)) {
        pos = j;
        break;
      }
    }
    
    List tmp = Sepset[pos];
    if (tmp.size()==0) {
      vstr.push_back(1);
      continue;
    } else {
      int num=0;
      for (int j=0; j<tmp.size(); j++) {
        std::vector<std::string> tt = tmp[j];
        for (int k=0; k<tt.size(); k++) {
          if (m.compare(tt.at(k))==0) {
            num = num+1;
            break;
          }
        }
      }
      if (num > (tmp.size()/2))
        vstr.push_back(0);
      else 
        vstr.push_back(1);
    }
  }
  
  
  return wrap(vstr);
}
