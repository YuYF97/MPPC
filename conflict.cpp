#include <Rcpp.h>
#include <string.h>
#include <iostream>

using namespace Rcpp;

// [[Rcpp::export]]
RcppExport SEXP conflict(SEXP MWr, SEXP MBr) {
  Rcpp::List MB(MBr);
  std::vector<std::string> Bfrom = MB["from"];
  std::vector<std::string> Bto = MB["to"];
  
  Rcpp::List MW(MWr);
  std::vector<std::string> Wfrom = MW["from"];
  std::vector<std::string> Wto = MW["to"];
  
  for (int i=0; i<Bfrom.size(); i++) {
    std::string bf = Bfrom.at(i);
    std::string bt = Bto.at(i);
    if (bf.compare(bt)==0) {
      Bfrom.erase(Bfrom.begin()+i);
      Bto.erase(Bto.begin()+i);
      i=i-1;
    }
  }
  
  for (int i=0; i<Wfrom.size(); i++) {
    std::string wf = Wfrom.at(i);
    std::string wt = Wto.at(i);
    for (int j=(i+1); j<Wfrom.size(); j++) {
      std::string f = Wfrom.at(j);
      std::string t = Wto.at(j);
      if (wf.compare(t)==0 && wt.compare(f)==0) {
        Wfrom.erase(Wfrom.begin()+j);
        Wto.erase(Wto.begin()+j);
        Wfrom.erase(Wfrom.begin()+i);
        Wto.erase(Wto.begin()+i);
        i=i-1;
        break;
      }
    }
  }
  
  int i=0;
  while (i<Wfrom.size()){
    std::string wf = Wfrom.at(i);
    std::string wt = Wto.at(i);
    if (wf.compare(wt)==0) {
      Wfrom.erase(Wfrom.begin()+i);
      Wto.erase(Wto.begin()+i);
      continue;
    }
    int j=0;
    while (j<Bfrom.size()){
      std::string bf = Bfrom.at(j);
      std::string bt = Bto.at(j);
      if (wf.compare(bf)==0 && wt.compare(bt)==0){ //in both W and B
        Wfrom.erase(Wfrom.begin()+i);
        Wto.erase(Wto.begin()+i);
        Bfrom.erase(Bfrom.begin()+j);
        Bto.erase(Bto.begin()+j);
        i=i-1;
      } else if (wf.compare(bt)==0 && wt.compare(bf)==0){ //rev(B) in W
        Bfrom.erase(Bfrom.begin()+j);
        Bto.erase(Bto.begin()+j);
      } else {
        j=j+1;
      }
    }
    i=i+1;
  }
  
  
  return Rcpp::List::create(Rcpp::List::create(Rcpp::Named("from") = Wfrom,Rcpp::Named("to") = Wto),
                            Rcpp::List::create(Rcpp::Named("from") = Bfrom,Rcpp::Named("to") = Bto));
}

