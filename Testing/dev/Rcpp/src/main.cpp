#include <iostream>
#include <vector>
#include "euler.h"
#include "parameters.h"
#include "initial.h"
#include "output.h"
#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
List Cascade() {

    int start = 0;
    int stop = 10;
    double step = 0.02;

    params * p = new params;
    initial * i = new initial;
    output * o = new output(stop / step);
    
    Euler(i, p, o, start, stop,  step);

    // Create a List
    List Out;

    //Create SEXP Object (NumericVectors)
    NumericVector N(500);
    NumericVector UnDx_500(500);
    NumericVector UnDx_350500(500);
    NumericVector UnDx_250350(500);
    NumericVector UnDx_200250(500);
    NumericVector UnDx_100200(500);
    NumericVector UnDx_50100(500);
    NumericVector UnDx_50(500);
    NumericVector Dx_500(500);
    NumericVector Dx_350500(500);
    NumericVector Dx_250350(500);
    NumericVector Dx_200250(500);
    NumericVector Dx_100200(500);
    NumericVector Dx_50100(500);
    NumericVector Dx_50(500);
    NumericVector Care_500(500);
    NumericVector Care_350500(500);
    NumericVector Care_250350(500);
    NumericVector Care_200250(500);
    NumericVector Care_100200(500);
    NumericVector Care_50100(500);
    NumericVector Care_50(500);
    NumericVector PreLtfu_500(500);
    NumericVector PreLtfu_350500(500);
    NumericVector PreLtfu_250350(500);
    NumericVector PreLtfu_200250(500);
    NumericVector PreLtfu_100200(500);
    NumericVector PreLtfu_50100(500);
    NumericVector PreLtfu_50(500);
    NumericVector Tx_Na_500(500);
    NumericVector Tx_Na_350500(500);
    NumericVector Tx_Na_250350(500);
    NumericVector Tx_Na_200250(500);
    NumericVector Tx_Na_100200(500);
    NumericVector Tx_Na_50100(500);
    NumericVector Tx_Na_50(500);
    NumericVector Tx_A_500(500);
    NumericVector Tx_A_350500(500);
    NumericVector Tx_A_250350(500);
    NumericVector Tx_A_200250(500);
    NumericVector Tx_A_100200(500);
    NumericVector Tx_A_50100(500);
    NumericVector Tx_A_50(500);
    NumericVector Ltfu_500(500);
    NumericVector Ltfu_350500(500);
    NumericVector Ltfu_250350(500);
    NumericVector Ltfu_200250(500);
    NumericVector Ltfu_100200(500);
    NumericVector Ltfu_50100(500);
    NumericVector Ltfu_50(500);
    NumericVector NewInf(500);
    NumericVector HivMortality(500);
    NumericVector NaturalMortality(500);
    NumericVector Dx_Cost(500);
    NumericVector Linkage_Cost(500);
    NumericVector Annual_Care_Cost(500);
    NumericVector Annual_ART_Cost(500);
    
    // Fill up vectors
    for(int i = 0; i < (stop / step); ++i) {
        N[i] = o->N[i];
        UnDx_500[i] = o->UnDx_500[i];
        UnDx_350500[i] = o->UnDx_350500[i];
        UnDx_250350[i] = o->UnDx_250350[i];
        UnDx_200250[i] = o->UnDx_200250[i];
        UnDx_100200[i] = o->UnDx_100200[i];
        UnDx_50100[i] = o->UnDx_50100[i];
        UnDx_50[i] = o->UnDx_50[i];
        Dx_500[i] = o->Dx_500[i];
        Dx_350500[i] = o->Dx_350500[i];
        Dx_250350[i] = o->Dx_250350[i];
        Dx_200250[i] = o->Dx_200250[i];
        Dx_100200[i] = o->Dx_100200[i];
        Dx_50100[i] = o->Dx_50100[i];
        Dx_50[i] = o->Dx_50[i];
        Care_500[i] = o->Care_500[i];
        Care_350500[i] = o->Care_350500[i];
        Care_250350[i] = o->Care_250350[i];
        Care_200250[i] = o->Care_200250[i];
        Care_100200[i] = o->Care_100200[i];
        Care_50100[i] = o->Care_50100[i];
        Care_50[i] = o->Care_50[i];
        PreLtfu_500[i] = o->PreLtfu_500[i];
        PreLtfu_350500[i] = o->PreLtfu_350500[i];
        PreLtfu_250350[i] = o->PreLtfu_250350[i];
        PreLtfu_200250[i] = o->PreLtfu_200250[i];
        PreLtfu_100200[i] = o->PreLtfu_100200[i];
        PreLtfu_50100[i] = o->PreLtfu_50100[i];
        PreLtfu_50[i] = o->PreLtfu_50[i];
        Tx_Na_500[i] = o->Tx_Na_500[i];
        Tx_Na_350500[i] = o->Tx_Na_350500[i];
        Tx_Na_250350[i] = o->Tx_Na_250350[i];
        Tx_Na_200250[i] = o->Tx_Na_200250[i];
        Tx_Na_100200[i] = o->Tx_Na_100200[i];
        Tx_Na_50100[i] = o->Tx_Na_50100[i];
        Tx_Na_50[i] = o->Tx_Na_50[i];
        Tx_A_500[i] = o->Tx_A_500[i];
        Tx_A_350500[i] = o->Tx_A_350500[i];
        Tx_A_250350[i] = o->Tx_A_250350[i];
        Tx_A_200250[i] = o->Tx_A_200250[i];
        Tx_A_100200[i] = o->Tx_A_100200[i];
        Tx_A_50100[i] = o->Tx_A_50100[i];
        Tx_A_50[i] = o->Tx_A_50[i];
        Ltfu_500[i] = o->Ltfu_500[i];
        Ltfu_350500[i] = o->Ltfu_350500[i];
        Ltfu_250350[i] = o->Ltfu_250350[i];
        Ltfu_200250[i] = o->Ltfu_200250[i];
        Ltfu_100200[i] = o->Ltfu_100200[i];
        Ltfu_50100[i] = o->Ltfu_50100[i];
        Ltfu_50[i] = o->Ltfu_50[i];
        NewInf[i] = o->NewInf[i];
        HivMortality[i] = o->HivMortality[i];
        NaturalMortality[i] = o->NaturalMortality[i];
        Dx_Cost[i] = o->Dx_Cost[i];
        Linkage_Cost[i] = o->Linkage_Cost[i];
        Annual_Care_Cost[i] = o->Annual_Care_Cost[i];
        Annual_ART_Cost[i] = o->Annual_ART_Cost[i];
    }

    // Attach NumericVectors to List
    Out["N"] = as<NumericVector>(N);
    Out["UnDx_500"] = as<NumericVector>(UnDx_500);
    Out["UnDx_350500"] = as<NumericVector>(UnDx_350500);
    Out["UnDx_250350"] = as<NumericVector>(UnDx_250350);
    Out["UnDx_200250"] = as<NumericVector>(UnDx_200250);
    Out["UnDx_100200"] = as<NumericVector>(UnDx_100200);
    Out["UnDx_50100"] = as<NumericVector>(UnDx_50100);
    Out["UnDx_50"] = as<NumericVector>(UnDx_50);
    Out["Dx_500"] = as<NumericVector>(Dx_500);
    Out["Dx_350500"] = as<NumericVector>(Dx_350500);
    Out["Dx_250350"] = as<NumericVector>(Dx_250350);
    Out["Dx_200250"] = as<NumericVector>(Dx_200250);
    Out["Dx_100200"] = as<NumericVector>(Dx_100200);
    Out["Dx_50100"] = as<NumericVector>(Dx_50100);
    Out["Dx_50"] = as<NumericVector>(Dx_50);
    Out["Care_500"] = as<NumericVector>(Care_500);
    Out["Care_350500"] = as<NumericVector>(Care_350500);
    Out["Care_250350"] = as<NumericVector>(Care_250350);
    Out["Care_200250"] = as<NumericVector>(Care_200250);
    Out["Care_100200"] = as<NumericVector>(Care_100200);
    Out["Care_50100"] = as<NumericVector>(Care_50100);
    Out["Care_50"] = as<NumericVector>(Care_50);
    Out["PreLtfu_500"] = as<NumericVector>(PreLtfu_500);
    Out["PreLtfu_350500"] = as<NumericVector>(PreLtfu_350500);
    Out["PreLtfu_250350"] = as<NumericVector>(PreLtfu_250350);
    Out["PreLtfu_200250"] = as<NumericVector>(PreLtfu_200250);
    Out["PreLtfu_100200"] = as<NumericVector>(PreLtfu_100200);
    Out["PreLtfu_50100"] = as<NumericVector>(PreLtfu_50100);
    Out["PreLtfu_50"] = as<NumericVector>(PreLtfu_50);
    Out["Tx_Na_500"] = as<NumericVector>(Tx_Na_500);
    Out["Tx_Na_350500"] = as<NumericVector>(Tx_Na_350500);
    Out["Tx_Na_250350"] = as<NumericVector>(Tx_Na_250350);
    Out["Tx_Na_200250"] = as<NumericVector>(Tx_Na_200250);
    Out["Tx_Na_100200"] = as<NumericVector>(Tx_Na_100200);
    Out["Tx_Na_50100"] = as<NumericVector>(Tx_Na_50100);
    Out["Tx_Na_50"] = as<NumericVector>(Tx_Na_50);
    Out["Tx_A_500"] = as<NumericVector>(Tx_A_500);
    Out["Tx_A_350500"] = as<NumericVector>(Tx_A_350500);
    Out["Tx_A_250350"] = as<NumericVector>(Tx_A_250350);
    Out["Tx_A_200250"] = as<NumericVector>(Tx_A_200250);
    Out["Tx_A_100200"] = as<NumericVector>(Tx_A_100200);
    Out["Tx_A_50100"] = as<NumericVector>(Tx_A_50100);
    Out["Tx_A_50"] = as<NumericVector>(Tx_A_50);
    Out["Ltfu_500"] = as<NumericVector>(Ltfu_500);
    Out["Ltfu_350500"] = as<NumericVector>(Ltfu_350500);
    Out["Ltfu_250350"] = as<NumericVector>(Ltfu_250350);
    Out["Ltfu_200250"] = as<NumericVector>(Ltfu_200250);
    Out["Ltfu_100200"] = as<NumericVector>(Ltfu_100200);
    Out["Ltfu_50100"] = as<NumericVector>(Ltfu_50100);
    Out["Ltfu_50"] = as<NumericVector>(Ltfu_50);
    Out["NewInf"] = as<NumericVector>(NewInf);
    Out["HivMortality"] = as<NumericVector>(HivMortality);
    Out["NaturalMortality"] = as<NumericVector>(NaturalMortality);
    Out["Dx_Cost"] = as<NumericVector>(Dx_Cost);
    Out["Linkage_Cost"] = as<NumericVector>(Linkage_Cost);
    Out["Annual_Care_Cost"] = as<NumericVector>(Annual_Care_Cost);
    Out["Annual_ART_Cost"] = as<NumericVector>(Annual_ART_Cost);

    delete p;
    delete i;
    delete o;

    return(Out);
}

/*** R
# Call the below function to run the model:
# Cascade()
*/