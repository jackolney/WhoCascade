#include <iostream>
#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
List Euler() {

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
Euler()
*/