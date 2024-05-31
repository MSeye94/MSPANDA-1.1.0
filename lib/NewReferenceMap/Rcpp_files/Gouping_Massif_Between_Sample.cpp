#include <Rcpp.h>
#include <iostream>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
List iterateFunction(DataFrame X, DataFrame X_new, Function f) {
    
    std::cout<<""<<std::endl;
    std::cout<<" Running cpp itération"<<std::endl;
  
    List resIterateVal;
    while(X.nrow()>=1){
       
       resIterateVal = f(X,X_new);
       
       X = resIterateVal[0];
       X_new = resIterateVal[1];
    }
    
    return resIterateVal;
}


