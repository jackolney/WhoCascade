#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

void derivs(int * neq, double *t, double *y, double *ydot, double *yout, int *ip);

struct myparms {
    double Nu_1;
    double Nu_2;
    double Nu_3;
    double Nu_4;
    double Nu_5;
    double Nu_6;
    double Rho;
    double Epsilon;
    double Kappa;
    double Gamma;
    double Theta;
    double Omega;
    double p;
    double s_1;
    double s_2;
    double s_3;
    double s_4;
    double s_5;
    double s_6;
    double s_7;
    double Sigma;
    double Delta_1;
    double Delta_2;
    double Delta_3;
    double Delta_4;
    double Delta_5;
    double Alpha_1;
    double Alpha_2;
    double Alpha_3;
    double Alpha_4;
    double Alpha_5;
    double Alpha_6;
    double Alpha_7;
    double Tau_1;
    double Tau_2;
    double Tau_3;
    double Tau_4;
    double Tau_5;
    double Tau_6;
    double Tau_7;
    double Mu;
    double ART_All;
    double ART_500;
    double ART_350;
    double ART_200;
    double Dx_unitCost;
    double Linkage_unitCost;
    double Annual_Care_unitCost;
    double Annual_ART_unitCost;
    double prop_preART_500;
    double prop_preART_350500;
    double prop_preART_250350;
    double prop_preART_200250;
    double prop_preART_100200;
    double prop_preART_50100;
    double prop_preART_50;
    double w1;
    double w2;
    double w3;
    double w4;
    double w5;
    double beta;
};

static struct myparms parms;

#define UnDx_500 0
#define UnDx_350500 1
#define UnDx_250350 2
#define UnDx_200250 3
#define UnDx_100200 4
#define UnDx_50100 5
#define UnDx_50 6
#define Dx_500 7
#define Dx_350500 8
#define Dx_250350 9
#define Dx_200250 10
#define Dx_100200 11
#define Dx_50100 12
#define Dx_50 13
#define Care_500 14
#define Care_350500 15
#define Care_250350 16
#define Care_200250 17
#define Care_100200 18
#define Care_50100 19
#define Care_50 20
#define PreLtfu_500 21
#define PreLtfu_350500 22
#define PreLtfu_250350 23
#define PreLtfu_200250 24
#define PreLtfu_100200 25
#define PreLtfu_50100 26
#define PreLtfu_50 27
#define Tx_Na_500 28
#define Tx_Na_350500 29
#define Tx_Na_250350 30
#define Tx_Na_200250 31
#define Tx_Na_100200 32
#define Tx_Na_50100 33
#define Tx_Na_50 34
#define Tx_A_500 35
#define Tx_A_350500 36
#define Tx_A_250350 37
#define Tx_A_200250 38
#define Tx_A_100200 39
#define Tx_A_50100 40
#define Tx_A_50 41
#define Ltfu_500 42
#define Ltfu_350500 43
#define Ltfu_250350 44
#define Ltfu_200250 45
#define Ltfu_100200 46
#define Ltfu_50100 47
#define Ltfu_50 48
#define NewInf 49
#define HivMortality 50
#define NaturalMortality 51
#define Dx_Cost 52
#define Linkage_Cost 53
#define Annual_Care_Cost 54
#define Annual_ART_Cost 55
#define N 56

SEXP r_initmod(SEXP rp) {
    if(LENGTH(rp) != 62) {
        Rf_error("Invalid Parameters.");
    }

    double * p = REAL(rp);

    parms.Nu_1 = p[0];
    parms.Nu_2 = p[1];
    parms.Nu_3 = p[2];
    parms.Nu_4 = p[3];
    parms.Nu_5 = p[4];
    parms.Nu_6 = p[5];
    parms.Rho = p[6];
    parms.Epsilon = p[7];
    parms.Kappa = p[8];
    parms.Gamma = p[9];
    parms.Theta = p[10];
    parms.Omega = p[11];
    parms.p = p[12];
    parms.s_1 = p[13];
    parms.s_2 = p[14];
    parms.s_3 = p[15];
    parms.s_4 = p[16];
    parms.s_5 = p[17];
    parms.s_6 = p[18];
    parms.s_7 = p[19];
    parms.Sigma = p[20];
    parms.Delta_1 = p[21];
    parms.Delta_2 = p[22];
    parms.Delta_3 = p[23];
    parms.Delta_4 = p[24];
    parms.Delta_5 = p[25];
    parms.Alpha_1 = p[26];
    parms.Alpha_2 = p[27];
    parms.Alpha_3 = p[28];
    parms.Alpha_4 = p[29];
    parms.Alpha_5 = p[30];
    parms.Alpha_6 = p[31];
    parms.Alpha_7 = p[32];
    parms.Tau_1 = p[33];
    parms.Tau_2 = p[34];
    parms.Tau_3 = p[35];
    parms.Tau_4 = p[36];
    parms.Tau_5 = p[37];
    parms.Tau_6 = p[38];
    parms.Tau_7 = p[39];
    parms.Mu = p[40];
    parms.ART_All = p[41];
    parms.ART_500 = p[42];
    parms.ART_350 = p[43];
    parms.ART_200 = p[44];
    parms.Dx_unitCost = p[45];
    parms.Linkage_unitCost = p[46];
    parms.Annual_Care_unitCost = p[47];
    parms.Annual_ART_unitCost = p[48];
    parms.prop_preART_500 = p[49];
    parms.prop_preART_350500 = p[50];
    parms.prop_preART_250350 = p[51];
    parms.prop_preART_200250 = p[52];
    parms.prop_preART_100200 = p[53];
    parms.prop_preART_50100 = p[54];
    parms.prop_preART_50 = p[55];
    parms.w1 = p[56];
    parms.w2 = p[57];
    parms.w3 = p[58];
    parms.w4 = p[59];
    parms.w5 = p[60];
    parms.beta = p[61];

    return R_NilValue;
}

SEXP r_derivs(SEXP y) {
    int neq = LENGTH(y);
    SEXP ydot = PROTECT(allocVector(REALSXP, neq));
    double * yout = NULL;
    int ip = 0;
    double t = 0.0;

    derivs(&neq, &t, REAL(y), REAL(ydot), yout, &ip);
    UNPROTECT(1);
    return ydot;
}

void initmod(void(* odeparms) (int *, double *)) {

    DL_FUNC get_deSolve_gparms = R_GetCCallable("deSolve", "get_deSolve_gparms");
    
    SEXP rp = get_deSolve_gparms();

    r_initmod(rp);
}

void derivs(int * neq, double *t, double *y, double *ydot, double *yout, int *ip) {

    ydot[0] = parms.prop_preART_500 * (parms.beta * (((y[UnDx_500] + y[Dx_500] + y[Care_500] + y[PreLtfu_500] + y[Tx_Na_500] + y[Ltfu_500]) * parms.w1) + ((y[UnDx_350500] + y[Dx_350500] + y[Care_350500] + y[PreLtfu_350500] + y[Tx_Na_350500] + y[Ltfu_350500]) * parms.w2) + ((y[UnDx_250350] + y[Dx_250350] + y[Care_250350] + y[PreLtfu_250350] + y[Tx_Na_250350] + y[Ltfu_250350] + y[UnDx_200250] + y[Dx_200250] + y[Care_200250] + y[PreLtfu_200250] + y[Tx_Na_200250] + y[Ltfu_200250]) * parms.w3) + ((y[UnDx_100200] + y[Dx_100200] + y[Care_100200] + y[PreLtfu_100200] + y[Tx_Na_100200] + y[Ltfu_100200] + y[UnDx_50100] + y[Dx_50100] + y[Care_50100] + y[PreLtfu_50100] + y[Tx_Na_50100] + y[Ltfu_50100] + y[UnDx_50] + y[Dx_50] + y[Care_50] + y[PreLtfu_50] + y[Tx_Na_50] + y[Ltfu_50]) * parms.w4) + ((y[Tx_A_500] + y[Tx_A_350500] + y[Tx_A_250350] + y[Tx_A_200250] + y[Tx_A_100200] + y[Tx_A_50100] + y[Tx_A_50]) * parms.w5))) - (parms.Nu_1 + parms.Rho + (parms.ART_All * parms.s_1 * parms.p * parms.Theta) + (parms.ART_All * parms.s_1 * (1-parms.p) * parms.Theta) + parms.Alpha_1 + parms.Mu) * y[UnDx_500];
    ydot[1] = parms.prop_preART_350500 * (parms.beta * (((y[UnDx_500] + y[Dx_500] + y[Care_500] + y[PreLtfu_500] + y[Tx_Na_500] + y[Ltfu_500]) * parms.w1) + ((y[UnDx_350500] + y[Dx_350500] + y[Care_350500] + y[PreLtfu_350500] + y[Tx_Na_350500] + y[Ltfu_350500]) * parms.w2) + ((y[UnDx_250350] + y[Dx_250350] + y[Care_250350] + y[PreLtfu_250350] + y[Tx_Na_250350] + y[Ltfu_250350] + y[UnDx_200250] + y[Dx_200250] + y[Care_200250] + y[PreLtfu_200250] + y[Tx_Na_200250] + y[Ltfu_200250]) * parms.w3) + ((y[UnDx_100200] + y[Dx_100200] + y[Care_100200] + y[PreLtfu_100200] + y[Tx_Na_100200] + y[Ltfu_100200] + y[UnDx_50100] + y[Dx_50100] + y[Care_50100] + y[PreLtfu_50100] + y[Tx_Na_50100] + y[Ltfu_50100] + y[UnDx_50] + y[Dx_50] + y[Care_50] + y[PreLtfu_50] + y[Tx_Na_50] + y[Ltfu_50]) * parms.w4) + ((y[Tx_A_500] + y[Tx_A_350500] + y[Tx_A_250350] + y[Tx_A_200250] + y[Tx_A_100200] + y[Tx_A_50100] + y[Tx_A_50]) * parms.w5))) + parms.Nu_1 * y[UnDx_500] - (parms.Nu_2 + parms.Rho + (parms.ART_500 * parms.s_2 * parms.p * parms.Theta) + (parms.ART_500 * parms.s_2 * (1-parms.p) * parms.Theta) + parms.Alpha_2 + parms.Mu) * y[UnDx_350500];
    ydot[2] = parms.prop_preART_250350 * (parms.beta * (((y[UnDx_500] + y[Dx_500] + y[Care_500] + y[PreLtfu_500] + y[Tx_Na_500] + y[Ltfu_500]) * parms.w1) + ((y[UnDx_350500] + y[Dx_350500] + y[Care_350500] + y[PreLtfu_350500] + y[Tx_Na_350500] + y[Ltfu_350500]) * parms.w2) + ((y[UnDx_250350] + y[Dx_250350] + y[Care_250350] + y[PreLtfu_250350] + y[Tx_Na_250350] + y[Ltfu_250350] + y[UnDx_200250] + y[Dx_200250] + y[Care_200250] + y[PreLtfu_200250] + y[Tx_Na_200250] + y[Ltfu_200250]) * parms.w3) + ((y[UnDx_100200] + y[Dx_100200] + y[Care_100200] + y[PreLtfu_100200] + y[Tx_Na_100200] + y[Ltfu_100200] + y[UnDx_50100] + y[Dx_50100] + y[Care_50100] + y[PreLtfu_50100] + y[Tx_Na_50100] + y[Ltfu_50100] + y[UnDx_50] + y[Dx_50] + y[Care_50] + y[PreLtfu_50] + y[Tx_Na_50] + y[Ltfu_50]) * parms.w4) + ((y[Tx_A_500] + y[Tx_A_350500] + y[Tx_A_250350] + y[Tx_A_200250] + y[Tx_A_100200] + y[Tx_A_50100] + y[Tx_A_50]) * parms.w5))) + parms.Nu_2 * y[UnDx_350500] - (parms.Nu_3 + parms.Rho + (parms.ART_350 * parms.s_3 * parms.p * parms.Theta) + (parms.ART_350 * parms.s_3 * (1-parms.p) * parms.Theta) + parms.Alpha_3 + parms.Mu) * y[UnDx_250350];
    ydot[3] = parms.prop_preART_200250 * (parms.beta * (((y[UnDx_500] + y[Dx_500] + y[Care_500] + y[PreLtfu_500] + y[Tx_Na_500] + y[Ltfu_500]) * parms.w1) + ((y[UnDx_350500] + y[Dx_350500] + y[Care_350500] + y[PreLtfu_350500] + y[Tx_Na_350500] + y[Ltfu_350500]) * parms.w2) + ((y[UnDx_250350] + y[Dx_250350] + y[Care_250350] + y[PreLtfu_250350] + y[Tx_Na_250350] + y[Ltfu_250350] + y[UnDx_200250] + y[Dx_200250] + y[Care_200250] + y[PreLtfu_200250] + y[Tx_Na_200250] + y[Ltfu_200250]) * parms.w3) + ((y[UnDx_100200] + y[Dx_100200] + y[Care_100200] + y[PreLtfu_100200] + y[Tx_Na_100200] + y[Ltfu_100200] + y[UnDx_50100] + y[Dx_50100] + y[Care_50100] + y[PreLtfu_50100] + y[Tx_Na_50100] + y[Ltfu_50100] + y[UnDx_50] + y[Dx_50] + y[Care_50] + y[PreLtfu_50] + y[Tx_Na_50] + y[Ltfu_50]) * parms.w4) + ((y[Tx_A_500] + y[Tx_A_350500] + y[Tx_A_250350] + y[Tx_A_200250] + y[Tx_A_100200] + y[Tx_A_50100] + y[Tx_A_50]) * parms.w5))) + parms.Nu_3 * y[UnDx_250350] - (parms.Nu_4 + parms.Rho + (parms.ART_350 * parms.s_4 * parms.p * parms.Theta) + (parms.ART_350 * parms.s_4 * (1-parms.p) * parms.Theta) + parms.Alpha_4 + parms.Mu) * y[UnDx_200250];
    ydot[4] = parms.prop_preART_100200 * (parms.beta * (((y[UnDx_500] + y[Dx_500] + y[Care_500] + y[PreLtfu_500] + y[Tx_Na_500] + y[Ltfu_500]) * parms.w1) + ((y[UnDx_350500] + y[Dx_350500] + y[Care_350500] + y[PreLtfu_350500] + y[Tx_Na_350500] + y[Ltfu_350500]) * parms.w2) + ((y[UnDx_250350] + y[Dx_250350] + y[Care_250350] + y[PreLtfu_250350] + y[Tx_Na_250350] + y[Ltfu_250350] + y[UnDx_200250] + y[Dx_200250] + y[Care_200250] + y[PreLtfu_200250] + y[Tx_Na_200250] + y[Ltfu_200250]) * parms.w3) + ((y[UnDx_100200] + y[Dx_100200] + y[Care_100200] + y[PreLtfu_100200] + y[Tx_Na_100200] + y[Ltfu_100200] + y[UnDx_50100] + y[Dx_50100] + y[Care_50100] + y[PreLtfu_50100] + y[Tx_Na_50100] + y[Ltfu_50100] + y[UnDx_50] + y[Dx_50] + y[Care_50] + y[PreLtfu_50] + y[Tx_Na_50] + y[Ltfu_50]) * parms.w4) + ((y[Tx_A_500] + y[Tx_A_350500] + y[Tx_A_250350] + y[Tx_A_200250] + y[Tx_A_100200] + y[Tx_A_50100] + y[Tx_A_50]) * parms.w5))) + parms.Nu_4 * y[UnDx_200250] - (parms.Nu_5 + parms.Rho + (parms.ART_200 * parms.s_5 * parms.p * parms.Theta) + (parms.ART_200 * parms.s_5 * (1-parms.p) * parms.Theta) + parms.Alpha_5 + parms.Mu) * y[UnDx_100200];
    ydot[5] = parms.prop_preART_50100 * (parms.beta * (((y[UnDx_500] + y[Dx_500] + y[Care_500] + y[PreLtfu_500] + y[Tx_Na_500] + y[Ltfu_500]) * parms.w1) + ((y[UnDx_350500] + y[Dx_350500] + y[Care_350500] + y[PreLtfu_350500] + y[Tx_Na_350500] + y[Ltfu_350500]) * parms.w2) + ((y[UnDx_250350] + y[Dx_250350] + y[Care_250350] + y[PreLtfu_250350] + y[Tx_Na_250350] + y[Ltfu_250350] + y[UnDx_200250] + y[Dx_200250] + y[Care_200250] + y[PreLtfu_200250] + y[Tx_Na_200250] + y[Ltfu_200250]) * parms.w3) + ((y[UnDx_100200] + y[Dx_100200] + y[Care_100200] + y[PreLtfu_100200] + y[Tx_Na_100200] + y[Ltfu_100200] + y[UnDx_50100] + y[Dx_50100] + y[Care_50100] + y[PreLtfu_50100] + y[Tx_Na_50100] + y[Ltfu_50100] + y[UnDx_50] + y[Dx_50] + y[Care_50] + y[PreLtfu_50] + y[Tx_Na_50] + y[Ltfu_50]) * parms.w4) + ((y[Tx_A_500] + y[Tx_A_350500] + y[Tx_A_250350] + y[Tx_A_200250] + y[Tx_A_100200] + y[Tx_A_50100] + y[Tx_A_50]) * parms.w5))) + parms.Nu_5 * y[UnDx_100200] - (parms.Nu_6 + parms.Rho + (parms.ART_200 * parms.s_6 * parms.p * parms.Theta) + (parms.ART_200 * parms.s_6 * (1-parms.p) * parms.Theta) + parms.Alpha_6 + parms.Mu) * y[UnDx_50100];
    ydot[6] = parms.prop_preART_50 * (parms.beta * (((y[UnDx_500] + y[Dx_500] + y[Care_500] + y[PreLtfu_500] + y[Tx_Na_500] + y[Ltfu_500]) * parms.w1) + ((y[UnDx_350500] + y[Dx_350500] + y[Care_350500] + y[PreLtfu_350500] + y[Tx_Na_350500] + y[Ltfu_350500]) * parms.w2) + ((y[UnDx_250350] + y[Dx_250350] + y[Care_250350] + y[PreLtfu_250350] + y[Tx_Na_250350] + y[Ltfu_250350] + y[UnDx_200250] + y[Dx_200250] + y[Care_200250] + y[PreLtfu_200250] + y[Tx_Na_200250] + y[Ltfu_200250]) * parms.w3) + ((y[UnDx_100200] + y[Dx_100200] + y[Care_100200] + y[PreLtfu_100200] + y[Tx_Na_100200] + y[Ltfu_100200] + y[UnDx_50100] + y[Dx_50100] + y[Care_50100] + y[PreLtfu_50100] + y[Tx_Na_50100] + y[Ltfu_50100] + y[UnDx_50] + y[Dx_50] + y[Care_50] + y[PreLtfu_50] + y[Tx_Na_50] + y[Ltfu_50]) * parms.w4) + ((y[Tx_A_500] + y[Tx_A_350500] + y[Tx_A_250350] + y[Tx_A_200250] + y[Tx_A_100200] + y[Tx_A_50100] + y[Tx_A_50]) * parms.w5))) + parms.Nu_6 * y[UnDx_50100] - (parms.Rho + (parms.ART_200 * parms.s_7 * parms.p * parms.Theta) + (parms.ART_200 * parms.s_7 * (1-parms.p) * parms.Theta) + parms.Alpha_7 + parms.Mu) * y[UnDx_50];

    ydot[7] = parms.Rho * y[UnDx_500] - (parms.Nu_1 + parms.Epsilon + (parms.ART_All * parms.s_1 * parms.p * parms.Theta) + (parms.ART_All * parms.s_1 * (1-parms.p) * parms.Theta) + parms.Alpha_1 + parms.Mu) * y[Dx_500];
    ydot[8] = parms.Rho * y[UnDx_350500] + parms.Nu_1 * y[Dx_500] - (parms.Nu_2 + parms.Epsilon + (parms.ART_500 * parms.s_2 * parms.p * parms.Theta) + (parms.ART_500 * parms.s_2 * (1-parms.p) * parms.Theta) + parms.Alpha_2 + parms.Mu) * y[Dx_350500];
    ydot[9] = parms.Rho * y[UnDx_250350] + parms.Nu_2 * y[Dx_350500] - (parms.Nu_3 + parms.Epsilon + (parms.ART_350 * parms.s_3 * parms.p * parms.Theta) + (parms.ART_350 * parms.s_3 * (1-parms.p) * parms.Theta) + parms.Alpha_3 + parms.Mu) * y[Dx_250350];
    ydot[10] = parms.Rho * y[UnDx_200250] + parms.Nu_3 * y[Dx_250350] - (parms.Nu_4 + parms.Epsilon + (parms.ART_350 * parms.s_4 * parms.p * parms.Theta) + (parms.ART_350 * parms.s_4 * (1-parms.p) * parms.Theta) + parms.Alpha_4 + parms.Mu) * y[Dx_200250];
    ydot[11] = parms.Rho * y[UnDx_100200] + parms.Nu_4 * y[Dx_200250] - (parms.Nu_5 + parms.Epsilon + (parms.ART_200 * parms.s_5 * parms.p * parms.Theta) + (parms.ART_200 * parms.s_5 * (1-parms.p) * parms.Theta) + parms.Alpha_5 + parms.Mu) * y[Dx_100200];
    ydot[12] = parms.Rho * y[UnDx_50100] + parms.Nu_5 * y[Dx_100200] - (parms.Nu_6 + parms.Epsilon + (parms.ART_200 * parms.s_6 * parms.p * parms.Theta) + (parms.ART_200 * parms.s_6 * (1-parms.p) * parms.Theta) + parms.Alpha_6 + parms.Mu) * y[Dx_50100];
    ydot[13] = parms.Rho * y[UnDx_50] + parms.Nu_6 * y[Dx_50100] - (parms.Epsilon + (parms.ART_200 * parms.s_7 * parms.p * parms.Theta) + (parms.ART_200 * parms.s_7 * (1-parms.p) * parms.Theta) + parms.Alpha_7 + parms.Mu) * y[Dx_50];

    ydot[14] = parms.Epsilon * y[Dx_500] - (parms.Nu_1 + parms.Kappa + (parms.ART_All * parms.p * parms.Gamma) + (parms.ART_All * parms.s_1 * parms.p * parms.Theta) + (parms.ART_500 * (1-parms.p) * parms.Gamma) + (parms.ART_500 * parms.s_1 * (1-parms.p) * parms.Theta) + parms.Alpha_1 + parms.Mu) * y[Care_500];
    ydot[15] = parms.Epsilon * y[Dx_350500] + parms.Nu_1 * y[Care_500] - (parms.Nu_2 + parms.Kappa + (parms.ART_500 * parms.p * parms.Gamma) + (parms.ART_500 * parms.s_2 * parms.p * parms.Theta) + (parms.ART_500 * (1-parms.p) * parms.Gamma) + (parms.ART_500 * parms.s_2 * (1-parms.p) * parms.Theta) + parms.Alpha_2 + parms.Mu) * y[Care_350500];
    ydot[16] = parms.Epsilon * y[Dx_250350] + parms.Nu_2 * y[Care_350500] - (parms.Nu_3 + parms.Kappa + (parms.ART_350 * parms.p * parms.Gamma) + (parms.ART_350 * parms.s_3 * parms.p * parms.Theta) + (parms.ART_350 * (1-parms.p) * parms.Gamma) + (parms.ART_350 * parms.s_3 * (1-parms.p) * parms.Theta) + parms.Alpha_3 + parms.Mu) * y[Care_250350];
    ydot[17] = parms.Epsilon * y[Dx_200250] + parms.Nu_3 * y[Care_250350] - (parms.Nu_4 + parms.Kappa + (parms.ART_350 * parms.p * parms.Gamma) + (parms.ART_350 * parms.s_4 * parms.p * parms.Theta) + (parms.ART_350 * (1-parms.p) * parms.Gamma) + (parms.ART_350 * parms.s_4 * (1-parms.p) * parms.Theta) + parms.Alpha_4 + parms.Mu) * y[Care_200250];
    ydot[18] = parms.Epsilon * y[Dx_100200] + parms.Nu_4 * y[Care_200250] - (parms.Nu_5 + parms.Kappa + (parms.ART_200 * parms.p * parms.Gamma) + (parms.ART_200 * parms.s_5 * parms.p * parms.Theta) + (parms.ART_200 * (1-parms.p) * parms.Gamma) + (parms.ART_200 * parms.s_5 * (1-parms.p) * parms.Theta) + parms.Alpha_5 + parms.Mu) * y[Care_100200];
    ydot[19] = parms.Epsilon * y[Dx_50100] + parms.Nu_5 * y[Care_100200] - (parms.Nu_6 + parms.Kappa + (parms.ART_200 * parms.p * parms.Gamma) + (parms.ART_200 * parms.s_6 * parms.p * parms.Theta) + (parms.ART_200 * (1-parms.p) * parms.Gamma) + (parms.ART_200 * parms.s_6 * (1-parms.p) * parms.Theta) + parms.Alpha_6 + parms.Mu) * y[Care_50100];
    ydot[20] = parms.Epsilon * y[Dx_50] + parms.Nu_6 * y[Care_50100] - (parms.Kappa + (parms.ART_200 * parms.p * parms.Gamma) + (parms.ART_200 * parms.s_7 * parms.p * parms.Theta) + (parms.ART_200 * (1-parms.p) * parms.Gamma) + (parms.ART_200 * parms.s_7 * (1-parms.p) * parms.Theta) + parms.Alpha_7 + parms.Mu) * y[Care_50];

    ydot[21] = parms.Kappa * y[Care_500] - (parms.Nu_1 + (parms.ART_All * parms.s_1 * parms.p * parms.Theta) + (parms.ART_All * parms.s_1 * (1-parms.p) * parms.Theta) + parms.Alpha_1 + parms.Mu) * y[PreLtfu_500];
    ydot[22] = parms.Kappa * y[Care_350500] + parms.Nu_1 * y[PreLtfu_500] - (parms.Nu_2 + (parms.ART_500 * parms.s_2 * parms.p * parms.Theta) + (parms.ART_500 * parms.s_2 * (1-parms.p) * parms.Theta) + parms.Alpha_2 + parms.Mu) * y[PreLtfu_350500];
    ydot[23] = parms.Kappa * y[Care_250350] + parms.Nu_2 * y[PreLtfu_350500] - (parms.Nu_3 + (parms.ART_350 * parms.s_3 * parms.p * parms.Theta) + (parms.ART_350 * parms.s_3 * (1-parms.p) * parms.Theta) + parms.Alpha_3 + parms.Mu) * y[PreLtfu_250350];
    ydot[24] = parms.Kappa * y[Care_200250] + parms.Nu_3 * y[PreLtfu_250350] - (parms.Nu_4 + (parms.ART_350 * parms.s_4 * parms.p * parms.Theta) + (parms.ART_350 * parms.s_4 * (1-parms.p) * parms.Theta) + parms.Alpha_4 + parms.Mu) * y[PreLtfu_200250];
    ydot[25] = parms.Kappa * y[Care_100200] + parms.Nu_4 * y[PreLtfu_200250] - (parms.Nu_5 + (parms.ART_200 * parms.s_5 * parms.p * parms.Theta) + (parms.ART_200 * parms.s_5 * (1-parms.p) * parms.Theta) + parms.Alpha_5 + parms.Mu) * y[PreLtfu_100200];
    ydot[26] = parms.Kappa * y[Care_50100] + parms.Nu_5 * y[PreLtfu_100200] - (parms.Nu_6 + (parms.ART_200 * parms.s_6 * parms.p * parms.Theta) + (parms.ART_200 * parms.s_6 * (1-parms.p) * parms.Theta) + parms.Alpha_6 + parms.Mu) * y[PreLtfu_50100];
    ydot[27] = parms.Kappa * y[Care_50] + parms.Nu_6 * y[PreLtfu_50100] - ((parms.ART_200 * parms.s_7 * parms.p * parms.Theta) + (parms.ART_200 * parms.s_7 * (1-parms.p) * parms.Theta) + parms.Alpha_7 + parms.Mu) * y[PreLtfu_50];

    ydot[28] = (parms.ART_500 * parms.s_1 * (1-parms.p) * parms.Theta) * y[PreLtfu_500] + ((parms.ART_All * (1-parms.p) * parms.Gamma) + (parms.ART_All * parms.s_1 * (1-parms.p) * parms.Theta)) * y[Care_500] + (parms.ART_All * parms.s_1 * (1-parms.p) * parms.Theta) * y[Dx_500] + (parms.ART_All * parms.s_1 * (1-parms.p) * parms.Theta) * y[UnDx_500] - (parms.Nu_1 + parms.Omega + parms.Sigma + parms.Alpha_1 + parms.Mu) * y[Tx_Na_500];
    ydot[29] = (parms.ART_500 * parms.s_2 * (1-parms.p) * parms.Theta) * y[PreLtfu_350500] + ((parms.ART_500 * (1-parms.p) * parms.Gamma) + (parms.ART_500 * parms.s_2 * (1-parms.p) * parms.Theta)) * y[Care_350500] + (parms.ART_500 * parms.s_2 * (1-parms.p) * parms.Theta) * y[Dx_350500] + (parms.ART_500 * parms.s_2 * (1-parms.p) * parms.Theta) * y[UnDx_350500] + parms.Nu_1 * y[Tx_Na_500] - (parms.Nu_2 + parms.Omega + parms.Sigma + parms.Alpha_2 + parms.Mu) * y[Tx_Na_350500];
    ydot[30] = (parms.ART_350 * parms.s_3 * (1-parms.p) * parms.Theta) * y[PreLtfu_250350] + ((parms.ART_350 * (1-parms.p) * parms.Gamma) + (parms.ART_350 * parms.s_3 * (1-parms.p) * parms.Theta)) * y[Care_250350] + (parms.ART_350 * parms.s_3 * (1-parms.p) * parms.Theta) * y[Dx_250350] + (parms.ART_350 * parms.s_3 * (1-parms.p) * parms.Theta) * y[UnDx_250350] + parms.Nu_2 * y[Tx_Na_350500] - (parms.Nu_3 + parms.Omega + parms.Sigma + parms.Alpha_3 + parms.Mu) * y[Tx_Na_250350];
    ydot[31] = (parms.ART_350 * parms.s_4 * (1-parms.p) * parms.Theta) * y[PreLtfu_200250] + ((parms.ART_350 * (1-parms.p) * parms.Gamma) + (parms.ART_350 * parms.s_4 * (1-parms.p) * parms.Theta)) * y[Care_200250] + (parms.ART_350 * parms.s_4 * (1-parms.p) * parms.Theta) * y[Dx_200250] + (parms.ART_350 * parms.s_4 * (1-parms.p) * parms.Theta) * y[UnDx_200250] + parms.Nu_3 * y[Tx_Na_250350] - (parms.Nu_4 + parms.Omega + parms.Sigma + parms.Alpha_4 + parms.Mu) * y[Tx_Na_200250];
    ydot[32] = (parms.ART_200 * parms.s_5 * (1-parms.p) * parms.Theta) * y[PreLtfu_100200] + ((parms.ART_200 * (1-parms.p) * parms.Gamma) + (parms.ART_200 * parms.s_5 * (1-parms.p) * parms.Theta)) * y[Care_100200] + (parms.ART_200 * parms.s_5 * (1-parms.p) * parms.Theta) * y[Dx_100200] + (parms.ART_200 * parms.s_5 * (1-parms.p) * parms.Theta) * y[UnDx_100200] + parms.Nu_4 * y[Tx_Na_200250] - (parms.Nu_5 + parms.Omega + parms.Sigma + parms.Alpha_5 + parms.Mu) * y[Tx_Na_100200];
    ydot[33] = (parms.ART_200 * parms.s_6 * (1-parms.p) * parms.Theta) * y[PreLtfu_50100] + ((parms.ART_200 * (1-parms.p) * parms.Gamma) + (parms.ART_200 * parms.s_6 * (1-parms.p) * parms.Theta)) * y[Care_50100] + (parms.ART_200 * parms.s_6 * (1-parms.p) * parms.Theta) * y[Dx_50100] + (parms.ART_200 * parms.s_6 * (1-parms.p) * parms.Theta) * y[UnDx_50100] + parms.Nu_5 * y[Tx_Na_100200] - (parms.Nu_6 + parms.Omega + parms.Sigma + parms.Alpha_6 + parms.Mu) * y[Tx_Na_50100];
    ydot[34] = (parms.ART_200 * parms.s_7 * (1-parms.p) * parms.Theta) * y[PreLtfu_50] + ((parms.ART_200 * (1-parms.p) * parms.Gamma) + (parms.ART_200 * parms.s_7 * (1-parms.p) * parms.Theta)) * y[Care_50] + (parms.ART_200 * parms.s_7 * (1-parms.p) * parms.Theta) * y[Dx_50] + (parms.ART_200 * parms.s_7 * (1-parms.p) * parms.Theta) * y[UnDx_50] + parms.Nu_6 * y[Tx_Na_50100] - (parms.Omega + parms.Sigma + parms.Alpha_7 + parms.Mu) * y[Tx_Na_50];

    ydot[35] = parms.Sigma * y[Tx_Na_500] + (parms.ART_All * parms.s_1 * parms.p * parms.Theta) * y[PreLtfu_500] + ((parms.ART_All * parms.p * parms.Gamma) + (parms.ART_All * parms.s_1 * parms.p * parms.Theta)) * y[Care_500] + (parms.ART_All * parms.s_1 * parms.p * parms.Theta) * y[Dx_500] + (parms.ART_All * parms.s_1 * parms.p * parms.Theta) * y[UnDx_500] - (parms.Omega + parms.Tau_1 + parms.Mu) * y[Tx_A_500];
    ydot[36] = parms.Sigma * y[Tx_Na_350500] + (parms.ART_500 * parms.s_2 * parms.p * parms.Theta) * y[PreLtfu_350500] + ((parms.ART_500 * parms.p * parms.Gamma) + (parms.ART_500 * parms.s_2 * parms.p * parms.Theta)) * y[Care_350500] + (parms.ART_500 * parms.s_2 * parms.p * parms.Theta) * y[Dx_350500] + (parms.ART_500 * parms.s_2 * parms.p * parms.Theta) * y[UnDx_350500] + parms.Delta_1 * y[Tx_A_250350] - (parms.Omega + parms.Tau_2 + parms.Mu) * y[Tx_A_350500];
    ydot[37] = parms.Sigma * y[Tx_Na_250350] + (parms.ART_350 * parms.s_3 * parms.p * parms.Theta) * y[PreLtfu_250350] + ((parms.ART_350 * parms.p * parms.Gamma) + (parms.ART_350 * parms.s_3 * parms.p * parms.Theta)) * y[Care_250350] + (parms.ART_350 * parms.s_3 * parms.p * parms.Theta) * y[Dx_250350] + (parms.ART_350 * parms.s_3 * parms.p * parms.Theta) * y[UnDx_250350] + parms.Delta_2 * y[Tx_A_200250] - (parms.Delta_1 + parms.Omega + parms.Tau_3 + parms.Mu) * y[Tx_A_250350];
    ydot[38] = parms.Sigma * y[Tx_Na_200250] + (parms.ART_350 * parms.s_4 * parms.p * parms.Theta) * y[PreLtfu_200250] + ((parms.ART_350 * parms.p * parms.Gamma) + (parms.ART_350 * parms.s_4 * parms.p * parms.Theta)) * y[Care_200250] + (parms.ART_350 * parms.s_4 * parms.p * parms.Theta) * y[Dx_200250] + (parms.ART_350 * parms.s_4 * parms.p * parms.Theta) * y[UnDx_200250] + parms.Delta_3 * y[Tx_A_100200] - (parms.Delta_2 + parms.Omega + parms.Tau_4 + parms.Mu) * y[Tx_A_200250];
    ydot[39] = parms.Sigma * y[Tx_Na_100200] + (parms.ART_200 * parms.s_5 * parms.p * parms.Theta) * y[PreLtfu_100200] + ((parms.ART_200 * parms.p * parms.Gamma) + (parms.ART_200 * parms.s_5 * parms.p * parms.Theta)) * y[Care_100200] + (parms.ART_200 * parms.s_5 * parms.p * parms.Theta) * y[Dx_100200] + (parms.ART_200 * parms.s_5 * parms.p * parms.Theta) * y[UnDx_100200] + parms.Delta_4 * y[Tx_A_50100] - (parms.Delta_3 + parms.Omega + parms.Tau_5 + parms.Mu) * y[Tx_A_100200];
    ydot[40] = parms.Sigma * y[Tx_Na_50100] + (parms.ART_200 * parms.s_6 * parms.p * parms.Theta) * y[PreLtfu_50100] + ((parms.ART_200 * parms.p * parms.Gamma) + (parms.ART_200 * parms.s_6 * parms.p * parms.Theta)) * y[Care_50100] + (parms.ART_200 * parms.s_6 * parms.p * parms.Theta) * y[Dx_50100] + (parms.ART_200 * parms.s_6 * parms.p * parms.Theta) * y[UnDx_50100] + parms.Delta_5 * y[Tx_A_50] - (parms.Delta_4 + parms.Omega + parms.Tau_6 + parms.Mu) * y[Tx_A_50100];
    ydot[41] = parms.Sigma * y[Tx_Na_50] + (parms.ART_200 * parms.s_7 * parms.p * parms.Theta) * y[PreLtfu_50] + ((parms.ART_200 * parms.p * parms.Gamma) + (parms.ART_200 * parms.s_7 * parms.p * parms.Theta)) * y[Care_50] + (parms.ART_200 * parms.s_7 * parms.p * parms.Theta) * y[Dx_50] + (parms.ART_200 * parms.s_7 * parms.p * parms.Theta) * y[UnDx_50] - (parms.Delta_5 + parms.Omega + parms.Tau_7 + parms.Mu) * y[Tx_A_50];

    ydot[42] = parms.Omega * (y[Tx_Na_500] + y[Tx_A_500]) - (parms.Nu_1 + parms.Alpha_1 + parms.Mu) * y[Ltfu_500];
    ydot[43] = parms.Omega * (y[Tx_Na_350500] + y[Tx_A_350500]) + parms.Nu_1 * y[Ltfu_500] - (parms.Nu_2 + parms.Alpha_2 + parms.Mu) * y[Ltfu_350500];
    ydot[44] = parms.Omega * (y[Tx_Na_250350] + y[Tx_A_250350]) + parms.Nu_2 * y[Ltfu_350500] - (parms.Nu_3 + parms.Alpha_3 + parms.Mu) * y[Ltfu_250350];
    ydot[45] = parms.Omega * (y[Tx_Na_200250] + y[Tx_A_200250]) + parms.Nu_3 * y[Ltfu_250350] - (parms.Nu_4 + parms.Alpha_4 + parms.Mu) * y[Ltfu_200250];
    ydot[46] = parms.Omega * (y[Tx_Na_100200] + y[Tx_A_100200]) + parms.Nu_4 * y[Ltfu_200250] - (parms.Nu_5 + parms.Alpha_5 + parms.Mu) * y[Ltfu_100200];
    ydot[47] = parms.Omega * (y[Tx_Na_50100] + y[Tx_A_50100]) + parms.Nu_5 * y[Ltfu_100200] - (parms.Nu_6 + parms.Alpha_6 + parms.Mu) * y[Ltfu_50100];
    ydot[48] = parms.Omega * (y[Tx_Na_50] + y[Tx_A_50]) + parms.Nu_6 * y[Ltfu_50100] - (parms.Alpha_7 + parms.Mu) * y[Ltfu_50];

    ydot[49] = parms.beta * (((y[UnDx_500] + y[Dx_500] + y[Care_500] + y[PreLtfu_500] + y[Tx_Na_500] + y[Ltfu_500]) * parms.w1) + ((y[UnDx_350500] + y[Dx_350500] + y[Care_350500] + y[PreLtfu_350500] + y[Tx_Na_350500] + y[Ltfu_350500]) * parms.w2) + ((y[UnDx_250350] + y[Dx_250350] + y[Care_250350] + y[PreLtfu_250350] + y[Tx_Na_250350] + y[Ltfu_250350] + y[UnDx_200250] + y[Dx_200250] + y[Care_200250] + y[PreLtfu_200250] + y[Tx_Na_200250] + y[Ltfu_200250]) * parms.w3) + ((y[UnDx_100200] + y[Dx_100200] + y[Care_100200] + y[PreLtfu_100200] + y[Tx_Na_100200] + y[Ltfu_100200] + y[UnDx_50100] + y[Dx_50100] + y[Care_50100] + y[PreLtfu_50100] + y[Tx_Na_50100] + y[Ltfu_50100] + y[UnDx_50] + y[Dx_50] + y[Care_50] + y[PreLtfu_50] + y[Tx_Na_50] + y[Ltfu_50]) * parms.w4) + ((y[Tx_A_500] + y[Tx_A_350500] + y[Tx_A_250350] + y[Tx_A_200250] + y[Tx_A_100200] + y[Tx_A_50100] + y[Tx_A_50]) * parms.w5));
    ydot[50] = parms.Alpha_1 * (y[UnDx_500] + y[Dx_500] + y[Care_500] + y[PreLtfu_500] + y[Tx_Na_500]) + parms.Alpha_2 * (y[UnDx_350500] + y[Dx_350500] + y[Care_350500] + y[PreLtfu_350500] + y[Tx_Na_350500]) + parms.Alpha_3 * (y[UnDx_250350] + y[Dx_250350] + y[Care_250350] + y[PreLtfu_250350] + y[Tx_Na_250350]) + parms.Alpha_4 * (y[UnDx_200250] + y[Dx_200250] + y[Care_200250] + y[PreLtfu_200250] + y[Tx_Na_200250]) + parms.Alpha_5 * (y[UnDx_100200] + y[Dx_100200] + y[Care_100200] + y[PreLtfu_100200] + y[Tx_Na_100200]) + parms.Alpha_6 * (y[UnDx_50100] + y[Dx_50100] + y[Care_50100] + y[PreLtfu_50100] + y[Tx_Na_50100]) + parms.Alpha_7 * (y[UnDx_50] + y[Dx_50] + y[Care_50] + y[PreLtfu_50] + y[Tx_Na_50]) + parms.Tau_1 * y[Tx_A_500] + parms.Tau_2 * y[Tx_A_350500] + parms.Tau_3 * y[Tx_A_250350] + parms.Tau_4 * y[Tx_A_200250] + parms.Tau_5 * y[Tx_A_100200] + parms.Tau_6 * y[Tx_A_50100] + parms.Tau_7 * y[Tx_A_50];
    ydot[51] = parms.Mu * (y[UnDx_500] + y[UnDx_350500] + y[UnDx_250350] + y[UnDx_200250] + y[UnDx_100200] + y[UnDx_50100] + y[UnDx_50] + y[Dx_500] + y[Dx_350500] + y[Dx_250350] + y[Dx_200250] + y[Dx_100200] + y[Dx_50100] + y[Dx_50] + y[Care_500] + y[Care_350500] + y[Care_250350] + y[Care_200250] + y[Care_100200] + y[Care_50100] + y[Care_50] + y[PreLtfu_500] + y[PreLtfu_350500] + y[PreLtfu_250350] + y[PreLtfu_200250] + y[PreLtfu_100200] + y[PreLtfu_50100] + y[PreLtfu_50] + y[Tx_Na_500] + y[Tx_Na_350500] + y[Tx_Na_250350] + y[Tx_Na_200250] + y[Tx_Na_100200] + y[Tx_Na_50100] + y[Tx_Na_50] + y[Tx_A_500] + y[Tx_A_350500] + y[Tx_A_250350] + y[Tx_A_200250] + y[Tx_A_100200] + y[Tx_A_50100] + y[Tx_A_50] + y[Ltfu_500] + y[Ltfu_350500] + y[Ltfu_250350] + y[Ltfu_200250] + y[Ltfu_100200] + y[Ltfu_50100] + y[Ltfu_50]);

    ydot[52] = (parms.Rho * (y[UnDx_500] + y[UnDx_350500] + y[UnDx_250350] + y[UnDx_200250] + y[UnDx_100200] + y[UnDx_50100] + y[UnDx_50])) * parms.Dx_unitCost;
    ydot[53] = (parms.Epsilon * (y[Dx_500] + y[Dx_350500] + y[Dx_250350] + y[Dx_200250] + y[Dx_100200] + y[Dx_50100] + y[Dx_50])) * parms.Linkage_unitCost;
    ydot[54] = (y[Care_500] + y[Care_350500] + y[Care_250350] + y[Care_200250] + y[Care_100200] + y[Care_50100] + y[Care_50]) * parms.Annual_Care_unitCost;
    ydot[55] = (y[Tx_Na_500] + y[Tx_Na_350500] + y[Tx_Na_250350] + y[Tx_Na_200250] + y[Tx_Na_100200] + y[Tx_Na_50100] + y[Tx_Na_50] + y[Tx_A_500] + y[Tx_A_350500] + y[Tx_A_250350] + y[Tx_A_200250] + y[Tx_A_100200] + y[Tx_A_50100] + y[Tx_A_50]) * parms.Annual_ART_unitCost;

    ydot[56] = y[UnDx_500] + y[UnDx_350500] + y[UnDx_250350] + y[UnDx_200250] + y[UnDx_100200] + y[UnDx_50100] + y[UnDx_50] + y[Dx_500] + y[Dx_350500] + y[Dx_250350] + y[Dx_200250] + y[Dx_100200] + y[Dx_50100] + y[Dx_50] + y[Care_500] + y[Care_350500] + y[Care_250350] + y[Care_200250] + y[Care_100200] + y[Care_50100] + y[Care_50] + y[PreLtfu_500] + y[PreLtfu_350500] + y[PreLtfu_250350] + y[PreLtfu_200250] + y[PreLtfu_100200] + y[PreLtfu_50100] + y[PreLtfu_50] + y[Tx_Na_500] + y[Tx_Na_350500] + y[Tx_Na_250350] + y[Tx_Na_200250] + y[Tx_Na_100200] + y[Tx_Na_50100] + y[Tx_Na_50] + y[Tx_A_500] + y[Tx_A_350500] + y[Tx_A_250350] + y[Tx_A_200250] + y[Tx_A_100200] + y[Tx_A_50100] + y[Tx_A_50] + y[Ltfu_500] + y[Ltfu_350500] + y[Ltfu_250350] + y[Ltfu_200250] + y[Ltfu_100200] + y[Ltfu_50100] + y[Ltfu_50];

}
