//
//  main.cpp
//  WhoCascade
//
//  Created by Jack Olney on 15/01/2016.
//  Copyright © 2016 Jack Olney. All rights reserved.
//

#include <iomanip>
#include <iostream>
#include <vector>
#include "euler.h"
#include "parameters.h"
#include "initial.h"
#include "output.h"

// R-stuff
#include <Rdefines.h>
#include <stdio.h>
#include <R.h>

using namespace std;

extern "C" {

    SEXP Cascade() {

    int start = 0;
    int stop = 10;
    double step = 0.02;

    params * p = new params;
    initial * i = new initial;
    output * o = new output(stop / step);

    Euler(i, p, o, start, stop,  step);

    // Test Output Vector
    SEXP sN, sUnDx_500, sUnDx_350500, sUnDx_250350, sUnDx_200250,
    sUnDx_100200, sUnDx_50100, sUnDx_50, sDx_500, sDx_350500, sDx_250350,
    sDx_200250, sDx_100200, sDx_50100, sDx_50, sCare_500, sCare_350500, sCare_250350,
    sCare_200250, sCare_100200, sCare_50100, sCare_50, sPreLtfu_500, sPreLtfu_350500,
    sPreLtfu_250350, sPreLtfu_200250, sPreLtfu_100200, sPreLtfu_50100, sPreLtfu_50, sTx_Na_500,
    sTx_Na_350500, sTx_Na_250350, sTx_Na_200250, sTx_Na_100200, sTx_Na_50100, sTx_Na_50, sTx_A_500,
    sTx_A_350500, sTx_A_250350, sTx_A_200250, sTx_A_100200, sTx_A_50100, sTx_A_50, sLtfu_500, sLtfu_350500,
    sLtfu_250350, sLtfu_200250, sLtfu_100200, sLtfu_50100, sLtfu_50, sNewInf, sHivMortality, sNaturalMortality,
    sDx_Cost, sLinkage_Cost, sAnnual_Care_Cost, sAnnual_ART_Cost;

    PROTECT(sN = allocVector(REALSXP, stop / step));
    PROTECT(sUnDx_500 = allocVector(REALSXP, stop / step));
    PROTECT(sUnDx_350500 = allocVector(REALSXP, stop / step));
    PROTECT(sUnDx_250350 = allocVector(REALSXP, stop / step));
    PROTECT(sUnDx_200250 = allocVector(REALSXP, stop / step));
    PROTECT(sUnDx_100200 = allocVector(REALSXP, stop / step));
    PROTECT(sUnDx_50100 = allocVector(REALSXP, stop / step));
    PROTECT(sUnDx_50 = allocVector(REALSXP, stop / step));
    PROTECT(sDx_500 = allocVector(REALSXP, stop / step));
    PROTECT(sDx_350500 = allocVector(REALSXP, stop / step));
    PROTECT(sDx_250350 = allocVector(REALSXP, stop / step));
    PROTECT(sDx_200250 = allocVector(REALSXP, stop / step));
    PROTECT(sDx_100200 = allocVector(REALSXP, stop / step));
    PROTECT(sDx_50100 = allocVector(REALSXP, stop / step));
    PROTECT(sDx_50 = allocVector(REALSXP, stop / step));
    PROTECT(sCare_500 = allocVector(REALSXP, stop / step));
    PROTECT(sCare_350500 = allocVector(REALSXP, stop / step));
    PROTECT(sCare_250350 = allocVector(REALSXP, stop / step));
    PROTECT(sCare_200250 = allocVector(REALSXP, stop / step));
    PROTECT(sCare_100200 = allocVector(REALSXP, stop / step));
    PROTECT(sCare_50100 = allocVector(REALSXP, stop / step));
    PROTECT(sCare_50 = allocVector(REALSXP, stop / step));
    PROTECT(sPreLtfu_500 = allocVector(REALSXP, stop / step));
    PROTECT(sPreLtfu_350500 = allocVector(REALSXP, stop / step));
    PROTECT(sPreLtfu_250350 = allocVector(REALSXP, stop / step));
    PROTECT(sPreLtfu_200250 = allocVector(REALSXP, stop / step));
    PROTECT(sPreLtfu_100200 = allocVector(REALSXP, stop / step));
    PROTECT(sPreLtfu_50100 = allocVector(REALSXP, stop / step));
    PROTECT(sPreLtfu_50 = allocVector(REALSXP, stop / step));
    PROTECT(sTx_Na_500 = allocVector(REALSXP, stop / step));
    PROTECT(sTx_Na_350500 = allocVector(REALSXP, stop / step));
    PROTECT(sTx_Na_250350 = allocVector(REALSXP, stop / step));
    PROTECT(sTx_Na_200250 = allocVector(REALSXP, stop / step));
    PROTECT(sTx_Na_100200 = allocVector(REALSXP, stop / step));
    PROTECT(sTx_Na_50100 = allocVector(REALSXP, stop / step));
    PROTECT(sTx_Na_50 = allocVector(REALSXP, stop / step));
    PROTECT(sTx_A_500 = allocVector(REALSXP, stop / step));
    PROTECT(sTx_A_350500 = allocVector(REALSXP, stop / step));
    PROTECT(sTx_A_250350 = allocVector(REALSXP, stop / step));
    PROTECT(sTx_A_200250 = allocVector(REALSXP, stop / step));
    PROTECT(sTx_A_100200 = allocVector(REALSXP, stop / step));
    PROTECT(sTx_A_50100 = allocVector(REALSXP, stop / step));
    PROTECT(sTx_A_50 = allocVector(REALSXP, stop / step));
    PROTECT(sLtfu_500 = allocVector(REALSXP, stop / step));
    PROTECT(sLtfu_350500 = allocVector(REALSXP, stop / step));
    PROTECT(sLtfu_250350 = allocVector(REALSXP, stop / step));
    PROTECT(sLtfu_200250 = allocVector(REALSXP, stop / step));
    PROTECT(sLtfu_100200 = allocVector(REALSXP, stop / step));
    PROTECT(sLtfu_50100 = allocVector(REALSXP, stop / step));
    PROTECT(sLtfu_50 = allocVector(REALSXP, stop / step));
    PROTECT(sNewInf = allocVector(REALSXP, stop / step));
    PROTECT(sHivMortality = allocVector(REALSXP, stop / step));
    PROTECT(sNaturalMortality = allocVector(REALSXP, stop / step));
    PROTECT(sDx_Cost = allocVector(REALSXP, stop / step));
    PROTECT(sLinkage_Cost = allocVector(REALSXP, stop / step));
    PROTECT(sAnnual_Care_Cost = allocVector(REALSXP, stop / step));
    PROTECT(sAnnual_ART_Cost = allocVector(REALSXP, stop / step));

    double * pN = REAL(sN);
    double * pUnDx_500 = REAL(sUnDx_500);
    double * pUnDx_350500 = REAL(sUnDx_350500);
    double * pUnDx_250350 = REAL(sUnDx_250350);
    double * pUnDx_200250 = REAL(sUnDx_200250);
    double * pUnDx_100200 = REAL(sUnDx_100200);
    double * pUnDx_50100 = REAL(sUnDx_50100);
    double * pUnDx_50 = REAL(sUnDx_50);
    double * pDx_500 = REAL(sDx_500);
    double * pDx_350500 = REAL(sDx_350500);
    double * pDx_250350 = REAL(sDx_250350);
    double * pDx_200250 = REAL(sDx_200250);
    double * pDx_100200 = REAL(sDx_100200);
    double * pDx_50100 = REAL(sDx_50100);
    double * pDx_50 = REAL(sDx_50);
    double * pCare_500 = REAL(sCare_500);
    double * pCare_350500 = REAL(sCare_350500);
    double * pCare_250350 = REAL(sCare_250350);
    double * pCare_200250 = REAL(sCare_200250);
    double * pCare_100200 = REAL(sCare_100200);
    double * pCare_50100 = REAL(sCare_50100);
    double * pCare_50 = REAL(sCare_50);
    double * pPreLtfu_500 = REAL(sPreLtfu_500);
    double * pPreLtfu_350500 = REAL(sPreLtfu_350500);
    double * pPreLtfu_250350 = REAL(sPreLtfu_250350);
    double * pPreLtfu_200250 = REAL(sPreLtfu_200250);
    double * pPreLtfu_100200 = REAL(sPreLtfu_100200);
    double * pPreLtfu_50100 = REAL(sPreLtfu_50100);
    double * pPreLtfu_50 = REAL(sPreLtfu_50);
    double * pTx_Na_500 = REAL(sTx_Na_500);
    double * pTx_Na_350500 = REAL(sTx_Na_350500);
    double * pTx_Na_250350 = REAL(sTx_Na_250350);
    double * pTx_Na_200250 = REAL(sTx_Na_200250);
    double * pTx_Na_100200 = REAL(sTx_Na_100200);
    double * pTx_Na_50100 = REAL(sTx_Na_50100);
    double * pTx_Na_50 = REAL(sTx_Na_50);
    double * pTx_A_500 = REAL(sTx_A_500);
    double * pTx_A_350500 = REAL(sTx_A_350500);
    double * pTx_A_250350 = REAL(sTx_A_250350);
    double * pTx_A_200250 = REAL(sTx_A_200250);
    double * pTx_A_100200 = REAL(sTx_A_100200);
    double * pTx_A_50100 = REAL(sTx_A_50100);
    double * pTx_A_50 = REAL(sTx_A_50);
    double * pLtfu_500 = REAL(sLtfu_500);
    double * pLtfu_350500 = REAL(sLtfu_350500);
    double * pLtfu_250350 = REAL(sLtfu_250350);
    double * pLtfu_200250 = REAL(sLtfu_200250);
    double * pLtfu_100200 = REAL(sLtfu_100200);
    double * pLtfu_50100 = REAL(sLtfu_50100);
    double * pLtfu_50 = REAL(sLtfu_50);
    double * pNewInf = REAL(sNewInf);
    double * pHivMortality = REAL(sHivMortality);
    double * pNaturalMortality = REAL(sNaturalMortality);
    double * pDx_Cost = REAL(sDx_Cost);
    double * pLinkage_Cost = REAL(sLinkage_Cost);
    double * pAnnual_Care_Cost = REAL(sAnnual_Care_Cost);
    double * pAnnual_ART_Cost = REAL(sAnnual_ART_Cost);

    SEXP OutputVector;
    PROTECT(OutputVector = allocVector(VECSXP,57));
    SET_VECTOR_ELT(OutputVector, 0, sN);
    SET_VECTOR_ELT(OutputVector, 1, sUnDx_500);
    SET_VECTOR_ELT(OutputVector, 2, sUnDx_350500);
    SET_VECTOR_ELT(OutputVector, 3, sUnDx_250350);
    SET_VECTOR_ELT(OutputVector, 4, sUnDx_200250);
    SET_VECTOR_ELT(OutputVector, 5, sUnDx_100200);
    SET_VECTOR_ELT(OutputVector, 6, sUnDx_50100);
    SET_VECTOR_ELT(OutputVector, 7, sUnDx_50);
    SET_VECTOR_ELT(OutputVector, 8, sDx_500);
    SET_VECTOR_ELT(OutputVector, 9, sDx_350500);
    SET_VECTOR_ELT(OutputVector, 10, sDx_250350);
    SET_VECTOR_ELT(OutputVector, 11, sDx_200250);
    SET_VECTOR_ELT(OutputVector, 12, sDx_100200);
    SET_VECTOR_ELT(OutputVector, 13, sDx_50100);
    SET_VECTOR_ELT(OutputVector, 14, sDx_50);
    SET_VECTOR_ELT(OutputVector, 15, sCare_500);
    SET_VECTOR_ELT(OutputVector, 16, sCare_350500);
    SET_VECTOR_ELT(OutputVector, 17, sCare_250350);
    SET_VECTOR_ELT(OutputVector, 18, sCare_200250);
    SET_VECTOR_ELT(OutputVector, 19, sCare_100200);
    SET_VECTOR_ELT(OutputVector, 20, sCare_50100);
    SET_VECTOR_ELT(OutputVector, 21, sCare_50);
    SET_VECTOR_ELT(OutputVector, 22, sPreLtfu_500);
    SET_VECTOR_ELT(OutputVector, 23, sPreLtfu_350500);
    SET_VECTOR_ELT(OutputVector, 24, sPreLtfu_250350);
    SET_VECTOR_ELT(OutputVector, 25, sPreLtfu_200250);
    SET_VECTOR_ELT(OutputVector, 26, sPreLtfu_100200);
    SET_VECTOR_ELT(OutputVector, 27, sPreLtfu_50100);
    SET_VECTOR_ELT(OutputVector, 28, sPreLtfu_50);
    SET_VECTOR_ELT(OutputVector, 29, sTx_Na_500);
    SET_VECTOR_ELT(OutputVector, 30, sTx_Na_350500);
    SET_VECTOR_ELT(OutputVector, 31, sTx_Na_250350);
    SET_VECTOR_ELT(OutputVector, 32, sTx_Na_200250);
    SET_VECTOR_ELT(OutputVector, 33, sTx_Na_100200);
    SET_VECTOR_ELT(OutputVector, 34, sTx_Na_50100);
    SET_VECTOR_ELT(OutputVector, 35, sTx_Na_50);
    SET_VECTOR_ELT(OutputVector, 36, sTx_A_500);
    SET_VECTOR_ELT(OutputVector, 37, sTx_A_350500);
    SET_VECTOR_ELT(OutputVector, 38, sTx_A_250350);
    SET_VECTOR_ELT(OutputVector, 39, sTx_A_200250);
    SET_VECTOR_ELT(OutputVector, 40, sTx_A_100200);
    SET_VECTOR_ELT(OutputVector, 41, sTx_A_50100);
    SET_VECTOR_ELT(OutputVector, 42, sTx_A_50);
    SET_VECTOR_ELT(OutputVector, 43, sLtfu_500);
    SET_VECTOR_ELT(OutputVector, 44, sLtfu_350500);
    SET_VECTOR_ELT(OutputVector, 45, sLtfu_250350);
    SET_VECTOR_ELT(OutputVector, 46, sLtfu_200250);
    SET_VECTOR_ELT(OutputVector, 47, sLtfu_100200);
    SET_VECTOR_ELT(OutputVector, 48, sLtfu_50100);
    SET_VECTOR_ELT(OutputVector, 49, sLtfu_50);
    SET_VECTOR_ELT(OutputVector, 50, sNewInf);
    SET_VECTOR_ELT(OutputVector, 51, sHivMortality);
    SET_VECTOR_ELT(OutputVector, 52, sNaturalMortality);
    SET_VECTOR_ELT(OutputVector, 53, sDx_Cost);
    SET_VECTOR_ELT(OutputVector, 54, sLinkage_Cost);
    SET_VECTOR_ELT(OutputVector, 55, sAnnual_Care_Cost);
    SET_VECTOR_ELT(OutputVector, 56, sAnnual_ART_Cost);

    SEXP VectorNames;
    PROTECT(VectorNames = allocVector(VECSXP,57));
    SET_VECTOR_ELT(VectorNames, 0, mkChar("sN"));
    SET_VECTOR_ELT(VectorNames, 1, mkChar("sUnDx_500"));
    SET_VECTOR_ELT(VectorNames, 2, mkChar("sUnDx_350500"));
    SET_VECTOR_ELT(VectorNames, 3, mkChar("sUnDx_250350"));
    SET_VECTOR_ELT(VectorNames, 4, mkChar("sUnDx_200250"));
    SET_VECTOR_ELT(VectorNames, 5, mkChar("sUnDx_100200"));
    SET_VECTOR_ELT(VectorNames, 6, mkChar("sUnDx_50100"));
    SET_VECTOR_ELT(VectorNames, 7, mkChar("sUnDx_50"));
    SET_VECTOR_ELT(VectorNames, 8, mkChar("sDx_500"));
    SET_VECTOR_ELT(VectorNames, 9, mkChar("sDx_350500"));
    SET_VECTOR_ELT(VectorNames, 10, mkChar("sDx_250350"));
    SET_VECTOR_ELT(VectorNames, 11, mkChar("sDx_200250"));
    SET_VECTOR_ELT(VectorNames, 12, mkChar("sDx_100200"));
    SET_VECTOR_ELT(VectorNames, 13, mkChar("sDx_50100"));
    SET_VECTOR_ELT(VectorNames, 14, mkChar("sDx_50"));
    SET_VECTOR_ELT(VectorNames, 15, mkChar("sCare_500"));
    SET_VECTOR_ELT(VectorNames, 16, mkChar("sCare_350500"));
    SET_VECTOR_ELT(VectorNames, 17, mkChar("sCare_250350"));
    SET_VECTOR_ELT(VectorNames, 18, mkChar("sCare_200250"));
    SET_VECTOR_ELT(VectorNames, 19, mkChar("sCare_100200"));
    SET_VECTOR_ELT(VectorNames, 20, mkChar("sCare_50100"));
    SET_VECTOR_ELT(VectorNames, 21, mkChar("sCare_50"));
    SET_VECTOR_ELT(VectorNames, 22, mkChar("sPreLtfu_500"));
    SET_VECTOR_ELT(VectorNames, 23, mkChar("sPreLtfu_350500"));
    SET_VECTOR_ELT(VectorNames, 24, mkChar("sPreLtfu_250350"));
    SET_VECTOR_ELT(VectorNames, 25, mkChar("sPreLtfu_200250"));
    SET_VECTOR_ELT(VectorNames, 26, mkChar("sPreLtfu_100200"));
    SET_VECTOR_ELT(VectorNames, 27, mkChar("sPreLtfu_50100"));
    SET_VECTOR_ELT(VectorNames, 28, mkChar("sPreLtfu_50"));
    SET_VECTOR_ELT(VectorNames, 29, mkChar("sTx_Na_500"));
    SET_VECTOR_ELT(VectorNames, 30, mkChar("sTx_Na_350500"));
    SET_VECTOR_ELT(VectorNames, 31, mkChar("sTx_Na_250350"));
    SET_VECTOR_ELT(VectorNames, 32, mkChar("sTx_Na_200250"));
    SET_VECTOR_ELT(VectorNames, 33, mkChar("sTx_Na_100200"));
    SET_VECTOR_ELT(VectorNames, 34, mkChar("sTx_Na_50100"));
    SET_VECTOR_ELT(VectorNames, 35, mkChar("sTx_Na_50"));
    SET_VECTOR_ELT(VectorNames, 36, mkChar("sTx_A_500"));
    SET_VECTOR_ELT(VectorNames, 37, mkChar("sTx_A_350500"));
    SET_VECTOR_ELT(VectorNames, 38, mkChar("sTx_A_250350"));
    SET_VECTOR_ELT(VectorNames, 39, mkChar("sTx_A_200250"));
    SET_VECTOR_ELT(VectorNames, 40, mkChar("sTx_A_100200"));
    SET_VECTOR_ELT(VectorNames, 41, mkChar("sTx_A_50100"));
    SET_VECTOR_ELT(VectorNames, 42, mkChar("sTx_A_50"));
    SET_VECTOR_ELT(VectorNames, 43, mkChar("sLtfu_500"));
    SET_VECTOR_ELT(VectorNames, 44, mkChar("sLtfu_350500"));
    SET_VECTOR_ELT(VectorNames, 45, mkChar("sLtfu_250350"));
    SET_VECTOR_ELT(VectorNames, 46, mkChar("sLtfu_200250"));
    SET_VECTOR_ELT(VectorNames, 47, mkChar("sLtfu_100200"));
    SET_VECTOR_ELT(VectorNames, 48, mkChar("sLtfu_50100"));
    SET_VECTOR_ELT(VectorNames, 49, mkChar("sLtfu_50"));
    SET_VECTOR_ELT(VectorNames, 50, mkChar("sNewInf"));
    SET_VECTOR_ELT(VectorNames, 51, mkChar("sHivMortality"));
    SET_VECTOR_ELT(VectorNames, 52, mkChar("sNaturalMortality"));
    SET_VECTOR_ELT(VectorNames, 53, mkChar("sDx_Cost"));
    SET_VECTOR_ELT(VectorNames, 54, mkChar("sLinkage_Cost"));
    SET_VECTOR_ELT(VectorNames, 55, mkChar("sAnnual_Care_Cost"));
    SET_VECTOR_ELT(VectorNames, 56, mkChar("sAnnual_ART_Cost"));
    namesgets(OutputVector,VectorNames);

    for (int i = 0; i < (stop / step); ++i) {
        pN[i] = o->N[i];
        pUnDx_500[i] = o->UnDx_500[i];
        pUnDx_350500[i] = o->UnDx_350500[i];
        pUnDx_250350[i] = o->UnDx_250350[i];
        pUnDx_200250[i] = o->UnDx_200250[i];
        pUnDx_100200[i] = o->UnDx_100200[i];
        pUnDx_50100[i] = o->UnDx_50100[i];
        pUnDx_50[i] = o->UnDx_50[i];
        pDx_500[i] = o->Dx_500[i];
        pDx_350500[i] = o->Dx_350500[i];
        pDx_250350[i] = o->Dx_250350[i];
        pDx_200250[i] = o->Dx_200250[i];
        pDx_100200[i] = o->Dx_100200[i];
        pDx_50100[i] = o->Dx_50100[i];
        pDx_50[i] = o->Dx_50[i];
        pCare_500[i] = o->Care_500[i];
        pCare_350500[i] = o->Care_350500[i];
        pCare_250350[i] = o->Care_250350[i];
        pCare_200250[i] = o->Care_200250[i];
        pCare_100200[i] = o->Care_100200[i];
        pCare_50100[i] = o->Care_50100[i];
        pCare_50[i] = o->Care_50[i];
        pPreLtfu_500[i] = o->PreLtfu_500[i];
        pPreLtfu_350500[i] = o->PreLtfu_350500[i];
        pPreLtfu_250350[i] = o->PreLtfu_250350[i];
        pPreLtfu_200250[i] = o->PreLtfu_200250[i];
        pPreLtfu_100200[i] = o->PreLtfu_100200[i];
        pPreLtfu_50100[i] = o->PreLtfu_50100[i];
        pPreLtfu_50[i] = o->PreLtfu_50[i];
        pTx_Na_500[i] = o->Tx_Na_500[i];
        pTx_Na_350500[i] = o->Tx_Na_350500[i];
        pTx_Na_250350[i] = o->Tx_Na_250350[i];
        pTx_Na_200250[i] = o->Tx_Na_200250[i];
        pTx_Na_100200[i] = o->Tx_Na_100200[i];
        pTx_Na_50100[i] = o->Tx_Na_50100[i];
        pTx_Na_50[i] = o->Tx_Na_50[i];
        pTx_A_500[i] = o->Tx_A_500[i];
        pTx_A_350500[i] = o->Tx_A_350500[i];
        pTx_A_250350[i] = o->Tx_A_250350[i];
        pTx_A_200250[i] = o->Tx_A_200250[i];
        pTx_A_100200[i] = o->Tx_A_100200[i];
        pTx_A_50100[i] = o->Tx_A_50100[i];
        pTx_A_50[i] = o->Tx_A_50[i];
        pLtfu_500[i] = o->Ltfu_500[i];
        pLtfu_350500[i] = o->Ltfu_350500[i];
        pLtfu_250350[i] = o->Ltfu_250350[i];
        pLtfu_200250[i] = o->Ltfu_200250[i];
        pLtfu_100200[i] = o->Ltfu_100200[i];
        pLtfu_50100[i] = o->Ltfu_50100[i];
        pLtfu_50[i] = o->Ltfu_50[i];
        pNewInf[i] = o->NewInf[i];
        pHivMortality[i] = o->HivMortality[i];
        pNaturalMortality[i] = o->NaturalMortality[i];
        pDx_Cost[i] = o->Dx_Cost[i];
        pLinkage_Cost[i] = o->Linkage_Cost[i];
        pAnnual_Care_Cost[i] = o->Annual_Care_Cost[i];
        pAnnual_ART_Cost[i] = o->Annual_ART_Cost[i];
    }

    delete p;
    delete i;
    delete o;

    UNPROTECT(59);
    return(OutputVector);
    }


}

int main() {

	// params * p = new params;

	// initial * i = new initial();

	// Euler(i, p, 0, 10,  0.02);

    // delete p;
    // delete i;

	return 0;
}


// Need an efficient structure that allows me to create a starting size, for all...

// IDEAL SITUATION
// Euler(start, stop, step)
// Then inside Euler, is the functions that are called to run the model? (a different function for each ODE?)
// Maintain 'pure' functional programming ethos?

// For this initial test, then just create two separate functions.

// Development Notes:
// Beta needs to be assigned at the beginning of each simulation. It is a pre-sim calculation. Based on "new infections" -> pass this to C++ and let it do the rest.
// Beta placeholder is: 0.02466

// R Wish List:
// 1) Compile and cout results (N) - Done.
// 2) Return a vector - Done.
// 3) Take an arguement - DO IT.
// 4) All together now

// Fun Stuff:
// Dynamic change of length of output vectors that matches (StopTime / Step) of model.

// EXPAND STOP TIME TRIGGER TO MAKE ARRAYS DYNAMIC
// Fix the output.h array size issue, can I use the parameter passed to the constructor for this?
