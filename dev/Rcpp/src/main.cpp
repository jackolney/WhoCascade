#include <iostream>
#include <vector>
#include "euler.h"
#include "parameters.h"
#include "initial.h"
#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
List Cascade(NumericVector pInput, NumericVector iInput) {

    params * p = new params(pInput);

    initial * i = new initial(iInput);

    Euler(i, p, 0, 10,  0.02);


    //Print out p
    // for(int i = 0; i < p.size(); ++i) {
    //     // std::cout << p.names() << std::endl;
    //     std::cout << p[i] << std::endl;
    // }

    // Create an array
    List TestArray;

    // Fill in a row
    double sample_row[10] = {};

    for(int i = 0; i < 10; ++i) {
        sample_row[i] = 42;
    }

    //SEXP Objects
    NumericVector Output(10);
    // double * pOutput = REAL(Output);


    for(int i = 0; i < 10; ++i) {
        Output[i] = sample_row[i];
    }

    TestArray["out"] = as<NumericVector>(Output);
    TestArray["outside"] = as<NumericVector>(Output);

    return TestArray;
}

/*** R
# Here I can put code to run at compile-time.
*/