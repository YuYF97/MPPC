#include <RcppArmadillo.h>
#include <Rcpp.h>
#include <string.h>
#include <iostream>
#include <map>

using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]

RcppExport SEXP triple(List und_arc, List arc, List dir_arc) {
  std::vector<std::string> ufrom = und_arc["from"];
  std::vector<std::string> uto = und_arc["to"];
  std::vector<std::string> afrom = arc["from"];
  std::vector<std::string> ato = arc["to"];
  std::vector<std::string> dfrom = dir_arc["from"];
  std::vector<std::string> dto = dir_arc["to"];
  
  std::vector<std::string> from;
  std::vector<std::string> med;
  std::vector<std::string> to;
  
  int kk=0;
  while (kk<ufrom.size()) {
    std::string f = ufrom.at(kk);
    std::string t = uto.at(kk);
    for (int k=kk; k<ufrom.size(); k++) {
      if (f.compare(uto.at(k))==0 & t.compare(ufrom.at(k))==0) {
        ufrom.erase(ufrom.begin()+k);
        uto.erase(uto.begin()+k);
        break;
      }
    }
    kk = kk+1;
  }
  
  kk=0;
  while (kk<afrom.size()) {
    std::string f = afrom.at(kk);
    std::string t = ato.at(kk);
    for (int k=kk; k<afrom.size(); k++) {
      if (f.compare(ato.at(k))==0 & t.compare(afrom.at(k))==0) {
        afrom.erase(afrom.begin()+k);
        ato.erase(ato.begin()+k);
        break;
      }
    }
    kk = kk+1;
  }
  
  if (ufrom.size()>0) {
    for (int i=0; i<(ufrom.size()-1); i++) {
      std::string f = ufrom.at(i);
      std::string t = uto.at(i);
      
      for (int j=(i+1); j<ufrom.size(); j++) {
        if (f.compare(ufrom.at(j))==0) {
          med.push_back(f);
          from.push_back(t);
          to.push_back(uto.at(j));
        } else if (f.compare(uto.at(j))==0) {
          med.push_back(f);
          from.push_back(t);
          to.push_back(ufrom.at(j));
        } else if (t.compare(ufrom.at(j))==0) {
          med.push_back(t);
          from.push_back(f);
          to.push_back(uto.at(j));
        } else if (t.compare(uto.at(j))==0) {
          med.push_back(t);
          from.push_back(f);
          to.push_back(ufrom.at(j));
        }
      }
    }
  }
  
  if (ufrom.size()>0) {
    for (int i=0; i<ufrom.size(); i++) {
      std::string f = ufrom.at(i);
      std::string t = uto.at(i);
      
      for (int j=0; j<dfrom.size(); j++) {
        if (f.compare(dto.at(j))==0) {
          med.push_back(f);
          from.push_back(dfrom.at(j));
          to.push_back(t);
        } else if (t.compare(dto.at(j))==0) {
          med.push_back(t);
          from.push_back(dfrom.at(j));
          to.push_back(f);
        } 
      }
    }
  }
  
  for (int i=0; i<from.size(); i++) {
    std::string f = from.at(i);
    std::string t = to.at(i);
    for (int j=0; j<afrom.size(); j++) {
      if ((f.compare(afrom.at(j))==0 & t.compare(ato.at(j))==0) |
          (f.compare(ato.at(j))==0 & t.compare(afrom.at(j))==0)) {
        from.erase(from.begin()+i);
        med.erase(med.begin()+i);
        to.erase(to.begin()+i);
        i=i-1;
        break;
      }
    }
  }
  
  return DataFrame::create(Rcpp::Named("from")=from, 
                           Rcpp::Named("med")=med, 
                           Rcpp::Named("to")=to);
}
