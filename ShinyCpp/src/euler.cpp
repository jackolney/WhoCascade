//
//  euler.cpp
//  WhoCascade
//
//  Created by Jack Olney on 17/01/2016.
//  Copyright © 2016 Jack Olney. All rights reserved.
//

#include <iostream>
#include <iomanip>
#include "euler.h"
#include "parameters.h"
#include "initial.h"

using namespace std;

/* Approximates y(t) in y'(t)=f(t,y) with y(start)=y0 and t=start..stop and the step size 'step'. */

void Euler(initial * i, params * p, double start, double stop, double step)
{
	for (double t = start; t < stop; t += step) {
		
		cout << fixed << setprecision(3) << t << " N = " << i->N << endl;
		
		double beta = 0.02466;
		
		// Think of this just like the excel implementation of the model, each row is a different cell f(x)
		
		/* UNDIAGNOSED */
		i->UnDx_500 += step * (0.5251 * (beta * (((i->UnDx_500 + i->Dx_500 + i->Care_500 + i->PreLtfu_500 + i->Tx_Na_500 + i->Ltfu_500) * 1.35)
			+ ((i->UnDx_350500 + i->Dx_350500 + i->Care_350500 + i->PreLtfu_350500 + i->Tx_Na_350500 + i->Ltfu_350500) * 1)
			+ ((i->UnDx_250350 + i->Dx_250350 + i->Care_250350 + i->PreLtfu_250350 + i->Tx_Na_250350 + i->Ltfu_250350 + i->UnDx_200250 + i->Dx_200250 + i->Care_200250 + i->PreLtfu_200250 + i->Tx_Na_200250 + i->Ltfu_200250) * 1.64)
			+ ((i->UnDx_100200 + i->Dx_100200 + i->Care_100200 + i->PreLtfu_100200 + i->Tx_Na_100200 + i->Ltfu_100200 + i->UnDx_50100 + i->Dx_50100 + i->Care_50100 + i->PreLtfu_50100 + i->Tx_Na_50100 + i->Ltfu_50100 + i->UnDx_50 + i->Dx_50 + i->Care_50 + i->PreLtfu_50 + i->Tx_Na_50 + i->Ltfu_50) * 5.17)
			+ ((i->Tx_A_500 + i->Tx_A_350500 + i->Tx_A_250350 + i->Tx_A_200250 + i->Tx_A_100200 + i->Tx_A_50100 + i->Tx_A_50) * 0.1)))
			- (p->nu_1 + p->rho + (p->art_all * p->s_1 * p->p * p->theta) + (p->art_all * p->s_1 * (1-p->p) * p->theta) + p->alpha_1 + p->mu) * i->UnDx_500);
		
		i->UnDx_350500 += step * (0.2315 * (beta * (((i->UnDx_500 + i->Dx_500 + i->Care_500 + i->PreLtfu_500 + i->Tx_Na_500 + i->Ltfu_500) * 1.35)
			+ ((i->UnDx_350500 + i->Dx_350500 + i->Care_350500 + i->PreLtfu_350500 + i->Tx_Na_350500 + i->Ltfu_350500) * 1)
			+ ((i->UnDx_250350 + i->Dx_250350 + i->Care_250350 + i->PreLtfu_250350 + i->Tx_Na_250350 + i->Ltfu_250350 + i->UnDx_200250 + i->Dx_200250 + i->Care_200250 + i->PreLtfu_200250 + i->Tx_Na_200250 + i->Ltfu_200250) * 1.64)
			+ ((i->UnDx_100200 + i->Dx_100200 + i->Care_100200 + i->PreLtfu_100200 + i->Tx_Na_100200 + i->Ltfu_100200 + i->UnDx_50100 + i->Dx_50100 + i->Care_50100 + i->PreLtfu_50100 + i->Tx_Na_50100 + i->Ltfu_50100 + i->UnDx_50 + i->Dx_50 + i->Care_50 + i->PreLtfu_50 + i->Tx_Na_50 + i->Ltfu_50) * 5.17)
			+ ((i->Tx_A_500 + i->Tx_A_350500 + i->Tx_A_250350 + i->Tx_A_200250 + i->Tx_A_100200 + i->Tx_A_50100 + i->Tx_A_50) * 0.1)))
			+ p->nu_1 * i->UnDx_500 - (p->nu_2 + p->rho + (p->art_500 * p->s_2 * p->p * p->theta) + (p->art_500 * p->s_2 * (1-p->p) * p->theta) + p->alpha_2 + p->mu) * i->UnDx_350500);
		
		i->UnDx_250350 += step * (0.1787 * (beta * (((i->UnDx_500 + i->Dx_500 + i->Care_500 + i->PreLtfu_500 + i->Tx_Na_500 + i->Ltfu_500) * 1.35)
			+ ((i->UnDx_350500 + i->Dx_350500 + i->Care_350500 + i->PreLtfu_350500 + i->Tx_Na_350500 + i->Ltfu_350500) * 1)
			+ ((i->UnDx_250350 + i->Dx_250350 + i->Care_250350 + i->PreLtfu_250350 + i->Tx_Na_250350 + i->Ltfu_250350 + i->UnDx_200250 + i->Dx_200250 + i->Care_200250 + i->PreLtfu_200250 + i->Tx_Na_200250 + i->Ltfu_200250) * 1.64)
			+ ((i->UnDx_100200 + i->Dx_100200 + i->Care_100200 + i->PreLtfu_100200 + i->Tx_Na_100200 + i->Ltfu_100200 + i->UnDx_50100 + i->Dx_50100 + i->Care_50100 + i->PreLtfu_50100 + i->Tx_Na_50100 + i->Ltfu_50100 + i->UnDx_50 + i->Dx_50 + i->Care_50 + i->PreLtfu_50 + i->Tx_Na_50 + i->Ltfu_50) * 5.17)
			+ ((i->Tx_A_500 + i->Tx_A_350500 + i->Tx_A_250350 + i->Tx_A_200250 + i->Tx_A_100200 + i->Tx_A_50100 + i->Tx_A_50) * 0.1)))
			+ p->nu_2 * i->UnDx_350500 - (p->nu_3 + p->rho + (p->art_350 * p->s_3 * p->p * p->theta) + (p->art_350 * p->s_3 * (1-p->p) * p->theta) + p->alpha_3 + p->mu) * i->UnDx_250350);
		
		i->UnDx_200250 += step * (0.0615 * (beta * (((i->UnDx_500 + i->Dx_500 + i->Care_500 + i->PreLtfu_500 + i->Tx_Na_500 + i->Ltfu_500) * 1.35)
			+ ((i->UnDx_350500 + i->Dx_350500 + i->Care_350500 + i->PreLtfu_350500 + i->Tx_Na_350500 + i->Ltfu_350500) * 1)
			+ ((i->UnDx_250350 + i->Dx_250350 + i->Care_250350 + i->PreLtfu_250350 + i->Tx_Na_250350 + i->Ltfu_250350 + i->UnDx_200250 + i->Dx_200250 + i->Care_200250 + i->PreLtfu_200250 + i->Tx_Na_200250 + i->Ltfu_200250) * 1.64)
			+ ((i->UnDx_100200 + i->Dx_100200 + i->Care_100200 + i->PreLtfu_100200 + i->Tx_Na_100200 + i->Ltfu_100200 + i->UnDx_50100 + i->Dx_50100 + i->Care_50100 + i->PreLtfu_50100 + i->Tx_Na_50100 + i->Ltfu_50100 + i->UnDx_50 + i->Dx_50 + i->Care_50 + i->PreLtfu_50 + i->Tx_Na_50 + i->Ltfu_50) * 5.17)
			+ ((i->Tx_A_500 + i->Tx_A_350500 + i->Tx_A_250350 + i->Tx_A_200250 + i->Tx_A_100200 + i->Tx_A_50100 + i->Tx_A_50) * 0.1)))
			+ p->nu_3 * i->UnDx_250350 - (p->nu_4 + p->rho + (p->art_350 * p->s_4 * p->p * p->theta) + (p->art_350 * p->s_4 * (1-p->p) * p->theta) + p->alpha_4 + p->mu) * i->UnDx_200250);
		
		i->UnDx_100200 += step * (0.0011 * (beta * (((i->UnDx_500 + i->Dx_500 + i->Care_500 + i->PreLtfu_500 + i->Tx_Na_500 + i->Ltfu_500) * 1.35)
			+ ((i->UnDx_350500 + i->Dx_350500 + i->Care_350500 + i->PreLtfu_350500 + i->Tx_Na_350500 + i->Ltfu_350500) * 1)
			+ ((i->UnDx_250350 + i->Dx_250350 + i->Care_250350 + i->PreLtfu_250350 + i->Tx_Na_250350 + i->Ltfu_250350 + i->UnDx_200250 + i->Dx_200250 + i->Care_200250 + i->PreLtfu_200250 + i->Tx_Na_200250 + i->Ltfu_200250) * 1.64)
			+ ((i->UnDx_100200 + i->Dx_100200 + i->Care_100200 + i->PreLtfu_100200 + i->Tx_Na_100200 + i->Ltfu_100200 + i->UnDx_50100 + i->Dx_50100 + i->Care_50100 + i->PreLtfu_50100 + i->Tx_Na_50100 + i->Ltfu_50100 + i->UnDx_50 + i->Dx_50 + i->Care_50 + i->PreLtfu_50 + i->Tx_Na_50 + i->Ltfu_50) * 5.17)
			+ ((i->Tx_A_500 + i->Tx_A_350500 + i->Tx_A_250350 + i->Tx_A_200250 + i->Tx_A_100200 + i->Tx_A_50100 + i->Tx_A_50) * 0.1)))
			+ p->nu_4 * i->UnDx_200250 - (p->nu_5 + p->rho + (p->art_200 * p->s_5 * p->p * p->theta) + (p->art_200 * p->s_5 * (1-p->p) * p->theta) + p->alpha_5 + p->mu) * i->UnDx_100200);
		
		i->UnDx_50100 += step * (0.0008 * (beta * (((i->UnDx_500 + i->Dx_500 + i->Care_500 + i->PreLtfu_500 + i->Tx_Na_500 + i->Ltfu_500) * 1.35)
			+ ((i->UnDx_350500 + i->Dx_350500 + i->Care_350500 + i->PreLtfu_350500 + i->Tx_Na_350500 + i->Ltfu_350500) * 1)
			+ ((i->UnDx_250350 + i->Dx_250350 + i->Care_250350 + i->PreLtfu_250350 + i->Tx_Na_250350 + i->Ltfu_250350 + i->UnDx_200250 + i->Dx_200250 + i->Care_200250 + i->PreLtfu_200250 + i->Tx_Na_200250 + i->Ltfu_200250) * 1.64)
			+ ((i->UnDx_100200 + i->Dx_100200 + i->Care_100200 + i->PreLtfu_100200 + i->Tx_Na_100200 + i->Ltfu_100200 + i->UnDx_50100 + i->Dx_50100 + i->Care_50100 + i->PreLtfu_50100 + i->Tx_Na_50100 + i->Ltfu_50100 + i->UnDx_50 + i->Dx_50 + i->Care_50 + i->PreLtfu_50 + i->Tx_Na_50 + i->Ltfu_50) * 5.17)
			+ ((i->Tx_A_500 + i->Tx_A_350500 + i->Tx_A_250350 + i->Tx_A_200250 + i->Tx_A_100200 + i->Tx_A_50100 + i->Tx_A_50) * 0.1)))
			+ p->nu_5 * i->UnDx_100200 - (p->nu_6 + p->rho + (p->art_200 * p->s_6 * p->p * p->theta) + (p->art_200 * p->s_6 * (1-p->p) * p->theta) + p->alpha_6 + p->mu) * i->UnDx_50100);
		
		i->UnDx_50 += step * (0.0014 * (beta * (((i->UnDx_500 + i->Dx_500 + i->Care_500 + i->PreLtfu_500 + i->Tx_Na_500 + i->Ltfu_500) * 1.35)
			+ ((i->UnDx_350500 + i->Dx_350500 + i->Care_350500 + i->PreLtfu_350500 + i->Tx_Na_350500 + i->Ltfu_350500) * 1)
			+ ((i->UnDx_250350 + i->Dx_250350 + i->Care_250350 + i->PreLtfu_250350 + i->Tx_Na_250350 + i->Ltfu_250350 + i->UnDx_200250 + i->Dx_200250 + i->Care_200250 + i->PreLtfu_200250 + i->Tx_Na_200250 + i->Ltfu_200250) * 1.64)
			+ ((i->UnDx_100200 + i->Dx_100200 + i->Care_100200 + i->PreLtfu_100200 + i->Tx_Na_100200 + i->Ltfu_100200 + i->UnDx_50100 + i->Dx_50100 + i->Care_50100 + i->PreLtfu_50100 + i->Tx_Na_50100 + i->Ltfu_50100 + i->UnDx_50 + i->Dx_50 + i->Care_50 + i->PreLtfu_50 + i->Tx_Na_50 + i->Ltfu_50) * 5.17)
			+ ((i->Tx_A_500 + i->Tx_A_350500 + i->Tx_A_250350 + i->Tx_A_200250 + i->Tx_A_100200 + i->Tx_A_50100 + i->Tx_A_50) * 0.1)))
			+ p->nu_6 * i->UnDx_50100 - (p->rho + (p->art_200 * p->s_7 * p->p * p->theta) + (p->art_200 * p->s_7 * (1-p->p) * p->theta) + p->alpha_7 + p->mu) * i->UnDx_50);
		
		/* DIAGNOSED */
		i->Dx_500 += step * (+ p->rho * i->UnDx_500 - (p->nu_1 + p->epsilon + (p->art_all * p->s_1 * p->p * p->theta) + (p->art_all * p->s_1 * (1-p->p) * p->theta) + p->alpha_1 + p->mu) * i->Dx_500);
		i->Dx_350500 += step * (+ p->rho * i->UnDx_350500 + p->nu_1 * i->Dx_500 - (p->nu_2 + p->epsilon + (p->art_500 * p->s_2 * p->p * p->theta) + (p->art_500 * p->s_2 * (1-p->p) * p->theta) + p->alpha_2 + p->mu) * i->Dx_350500);
		i->Dx_250350 += step * (+ p->rho * i->UnDx_250350 + p->nu_2 * i->Dx_350500 - (p->nu_3 + p->epsilon + (p->art_350 * p->s_3 * p->p * p->theta) + (p->art_350 * p->s_3 * (1-p->p) * p->theta) + p->alpha_3 + p->mu) * i->Dx_250350);
		i->Dx_200250 += step * (+ p->rho * i->UnDx_200250 + p->nu_3 * i->Dx_250350 - (p->nu_4 + p->epsilon + (p->art_350 * p->s_4 * p->p * p->theta) + (p->art_350 * p->s_4 * (1-p->p) * p->theta) + p->alpha_4 + p->mu) * i->Dx_200250);
		i->Dx_100200 += step * (+ p->rho * i->UnDx_100200 + p->nu_4 * i->Dx_200250 - (p->nu_5 + p->epsilon + (p->art_200 * p->s_5 * p->p * p->theta) + (p->art_200 * p->s_5 * (1-p->p) * p->theta) + p->alpha_5 + p->mu) * i->Dx_100200);
		i->Dx_50100 += step * (+ p->rho * i->UnDx_50100 + p->nu_5 * i->Dx_100200 - (p->nu_6 + p->epsilon + (p->art_200 * p->s_6 * p->p * p->theta) + (p->art_200 * p->s_6 * (1-p->p) * p->theta) + p->alpha_6 + p->mu) * i->Dx_50100);
		i->Dx_50 += step * (+ p->rho * i->UnDx_50 + p->nu_6 * i->Dx_50100 - (p->epsilon + (p->art_200 * p->s_7 * p->p * p->theta) + (p->art_200 * p->s_7 * (1-p->p) * p->theta) + p->alpha_7 + p->mu) * i->Dx_50);
		
		/* CARE */
		i->Care_500 += step * (+ p->epsilon * i->Dx_500 - (p->nu_1 + p->kappa + (p->art_all * p->p * p->gamma) + (p->art_all * p->s_1 * p->p * p->theta) + (p->art_500 * (1-p->p) * p->gamma) + (p->art_500 * p->s_1 * (1-p->p) * p->theta) + p->alpha_1 + p->mu) * i->Care_500);
		i->Care_350500 += step * (+ p->epsilon * i->Dx_350500 + p->nu_1 * i->Care_500 - (p->nu_2 + p->kappa + (p->art_500 * p->p * p->gamma) + (p->art_500 * p->s_2 * p->p * p->theta) + (p->art_500 * (1-p->p) * p->gamma) + (p->art_500 * p->s_2 * (1-p->p) * p->theta) + p->alpha_2 + p->mu) * i->Care_350500);
		i->Care_250350 += step * (+ p->epsilon * i->Dx_250350 + p->nu_2 * i->Care_350500 - (p->nu_3 + p->kappa + (p->art_350 * p->p * p->gamma) + (p->art_350 * p->s_3 * p->p * p->theta) + (p->art_350 * (1-p->p) * p->gamma) + (p->art_350 * p->s_3 * (1-p->p) * p->theta) + p->alpha_3 + p->mu) * i->Care_250350);
		i->Care_200250 += step * (+ p->epsilon * i->Dx_200250 + p->nu_3 * i->Care_250350 - (p->nu_4 + p->kappa + (p->art_350 * p->p * p->gamma) + (p->art_350 * p->s_4 * p->p * p->theta) + (p->art_350 * (1-p->p) * p->gamma) + (p->art_350 * p->s_4 * (1-p->p) * p->theta) + p->alpha_4 + p->mu) * i->Care_200250);
		i->Care_100200 += step * (+ p->epsilon * i->Dx_100200 + p->nu_4 * i->Care_200250 - (p->nu_5 + p->kappa + (p->art_200 * p->p * p->gamma) + (p->art_200 * p->s_5 * p->p * p->theta) + (p->art_200 * (1-p->p) * p->gamma) + (p->art_200 * p->s_5 * (1-p->p) * p->theta) + p->alpha_5 + p->mu) * i->Care_100200);
		i->Care_50100 += step * (+ p->epsilon * i->Dx_50100 + p->nu_5 * i->Care_100200 - (p->nu_6 + p->kappa + (p->art_200 * p->p * p->gamma) + (p->art_200 * p->s_6 * p->p * p->theta) + (p->art_200 * (1-p->p) * p->gamma) + (p->art_200 * p->s_6 * (1-p->p) * p->theta) + p->alpha_6 + p->mu) * i->Care_50100);
		i->Care_50 += step * (+ p->epsilon * i->Dx_50 + p->nu_6 * i->Care_50100 - (p->kappa + (p->art_200 * p->p * p->gamma) + (p->art_200 * p->s_7 * p->p * p->theta) + (p->art_200 * (1-p->p) * p->gamma) + (p->art_200 * p->s_7 * (1-p->p) * p->theta) + p->alpha_7 + p->mu) * i->Care_50);
		
		/* PRE-LTFU */
		i->PreLtfu_500 += step * (+ p->kappa * i->Care_500 - (p->nu_1 + (p->art_all * p->s_1 * p->p * p->theta) + (p->art_all * p->s_1 * (1-p->p) * p->theta) + p->alpha_1 + p->mu) * i->PreLtfu_500);
		i->PreLtfu_350500 += step * (+ p->kappa * i->Care_350500 + p->nu_1 * i->PreLtfu_500 - (p->nu_2 + (p->art_500 * p->s_2 * p->p * p->theta) + (p->art_500 * p->s_2 * (1-p->p) * p->theta) + p->alpha_2 + p->mu) * i->PreLtfu_350500);
		i->PreLtfu_250350 += step * (+ p->kappa * i->Care_250350 + p->nu_2 * i->PreLtfu_350500 - (p->nu_3 + (p->art_350 * p->s_3 * p->p * p->theta) + (p->art_350 * p->s_3 * (1-p->p) * p->theta) + p->alpha_3 + p->mu) * i->PreLtfu_250350);
		i->PreLtfu_200250 += step * (+ p->kappa * i->Care_200250 + p->nu_3 * i->PreLtfu_250350 - (p->nu_4 + (p->art_350 * p->s_4 * p->p * p->theta) + (p->art_350 * p->s_4 * (1-p->p) * p->theta) + p->alpha_4 + p->mu) * i->PreLtfu_200250);
		i->PreLtfu_100200 += step * (+ p->kappa * i->Care_100200 + p->nu_4 * i->PreLtfu_200250 - (p->nu_5 + (p->art_200 * p->s_5 * p->p * p->theta) + (p->art_200 * p->s_5 * (1-p->p) * p->theta) + p->alpha_5 + p->mu) * i->PreLtfu_100200);
		i->PreLtfu_50100 += step * (+ p->kappa * i->Care_50100 + p->nu_5 * i->PreLtfu_100200 - (p->nu_6 + (p->art_200 * p->s_6 * p->p * p->theta) + (p->art_200 * p->s_6 * (1-p->p) * p->theta) + p->alpha_6 + p->mu) * i->PreLtfu_50100);
		i->PreLtfu_50 += step * (+ p->kappa * i->Care_50 + p->nu_6 * i->PreLtfu_50100 - ((p->art_200 * p->s_7 * p->p * p->theta) + (p->art_200 * p->s_7 * (1-p->p) * p->theta) + p->alpha_7 + p->mu) * i->PreLtfu_50);
		
		/* ART (non-adherers) */
		i->Tx_Na_500 += step * (+ (p->art_500 * p->s_1 * (1-p->p) * p->theta) * i->PreLtfu_500
			+ ((p->art_all * (1-p->p) * p->gamma) + (p->art_all * p->s_1 * (1-p->p) * p->theta)) * i->Care_500
			+ (p->art_all * p->s_1 * (1-p->p) * p->theta) * i->Dx_500 + (p->art_all * p->s_1 * (1-p->p) * p->theta) * i->UnDx_500
			- (p->nu_1 + p->omega + p->sigma + p->alpha_1 + p->mu) * i->Tx_Na_500);
		
		i->Tx_Na_350500 += step * (+ (p->art_500 * p->s_2 * (1-p->p) * p->theta) * i->PreLtfu_350500
			+ ((p->art_500 * (1-p->p) * p->gamma) + (p->art_500 * p->s_2 * (1-p->p) * p->theta)) * i->Care_350500
			+ (p->art_500 * p->s_2 * (1-p->p) * p->theta) * i->Dx_350500 + (p->art_500 * p->s_2 * (1-p->p) * p->theta) * i->UnDx_350500
			+ p->nu_1 * i->Tx_Na_500 - (p->nu_2 + p->omega + p->sigma + p->alpha_2 + p->mu) * i->Tx_Na_350500);
		
		i->Tx_Na_250350 += step * (+ (p->art_350 * p->s_3 * (1-p->p) * p->theta) * i->PreLtfu_250350
			+ ((p->art_350 * (1-p->p) * p->gamma) + (p->art_350 * p->s_3 * (1-p->p) * p->theta)) * i->Care_250350
			+ (p->art_350 * p->s_3 * (1-p->p) * p->theta) * i->Dx_250350 + (p->art_350 * p->s_3 * (1-p->p) * p->theta) * i->UnDx_250350
			+ p->nu_2 * i->Tx_Na_350500 - (p->nu_3 + p->omega + p->sigma + p->alpha_3 + p->mu) * i->Tx_Na_250350);
		
		i->Tx_Na_200250 += step * (+ (p->art_350 * p->s_4 * (1-p->p) * p->theta) * i->PreLtfu_200250
			+ ((p->art_350 * (1-p->p) * p->gamma) + (p->art_350 * p->s_4 * (1-p->p) * p->theta)) * i->Care_200250
			+ (p->art_350 * p->s_4 * (1-p->p) * p->theta) * i->Dx_200250 + (p->art_350 * p->s_4 * (1-p->p) * p->theta) * i->UnDx_200250
			+ p->nu_3 * i->Tx_Na_250350 - (p->nu_4 + p->omega + p->sigma + p->alpha_4 + p->mu) * i->Tx_Na_200250);
		
		i->Tx_Na_100200 += step * (+ (p->art_200 * p->s_5 * (1-p->p) * p->theta) * i->PreLtfu_100200
			+ ((p->art_200 * (1-p->p) * p->gamma) + (p->art_200 * p->s_5 * (1-p->p) * p->theta)) * i->Care_100200
			+ (p->art_200 * p->s_5 * (1-p->p) * p->theta) * i->Dx_100200 + (p->art_200 * p->s_5 * (1-p->p) * p->theta) * i->UnDx_100200
			+ p->nu_4 * i->Tx_Na_200250 - (p->nu_5 + p->omega + p->sigma + p->alpha_5 + p->mu) * i->Tx_Na_100200);
		
		i->Tx_Na_50100 += step * (+ (p->art_200 * p->s_6 * (1-p->p) * p->theta) * i->PreLtfu_50100
			+ ((p->art_200 * (1-p->p) * p->gamma) + (p->art_200 * p->s_6 * (1-p->p) * p->theta)) * i->Care_50100
			+ (p->art_200 * p->s_6 * (1-p->p) * p->theta) * i->Dx_50100 + (p->art_200 * p->s_6 * (1-p->p) * p->theta) * i->UnDx_50100
			+ p->nu_5 * i->Tx_Na_100200 - (p->nu_6 + p->omega + p->sigma + p->alpha_6 + p->mu) * i->Tx_Na_50100);
		
		i->Tx_Na_50 += step * (+ (p->art_200 * p->s_7 * (1-p->p) * p->theta) * i->PreLtfu_50
			+ ((p->art_200 * (1-p->p) * p->gamma) + (p->art_200 * p->s_7 * (1-p->p) * p->theta)) * i->Care_50
			+ (p->art_200 * p->s_7 * (1-p->p) * p->theta) * i->Dx_50 + (p->art_200 * p->s_7 * (1-p->p) * p->theta) * i->UnDx_50
			+ p->nu_6 * i->Tx_Na_50100 - (p->omega + p->sigma + p->alpha_7 + p->mu) * i->Tx_Na_50);
		
	
		/* ART (adherers) */
		i->Tx_A_500 += step * (+ p->sigma * i->Tx_Na_500 + (p->art_all * p->s_1 * p->p * p->theta) * i->PreLtfu_500
			+ ((p->art_all * p->p * p->gamma) + (p->art_all * p->s_1 * p->p * p->theta)) * i->Care_500
			+ (p->art_all * p->s_1 * p->p * p->theta) * i->Dx_500 + (p->art_all * p->s_1 * p->p * p->theta) * i->UnDx_500
			- (p->omega + p->tau_1 + p->mu) * i->Tx_A_500);
		
		i->Tx_A_350500 += step * (+ p->sigma * i->Tx_Na_350500 + (p->art_500 * p->s_2 * p->p * p->theta) * i->PreLtfu_350500
			+ ((p->art_500 * p->p * p->gamma) + (p->art_500 * p->s_2 * p->p * p->theta)) * i->Care_350500
			+ (p->art_500 * p->s_2 * p->p * p->theta) * i->Dx_350500 + (p->art_500 * p->s_2 * p->p * p->theta) * i->UnDx_350500
			+ p->delta_1 * i->Tx_A_250350 - (p->omega + p->tau_2 + p->mu) * i->Tx_A_350500);
		
		i->Tx_A_250350 += step * (+ p->sigma * i->Tx_Na_250350 + (p->art_350 * p->s_3 * p->p * p->theta) * i->PreLtfu_250350
			+ ((p->art_350 * p->p * p->gamma) + (p->art_350 * p->s_3 * p->p * p->theta)) * i->Care_250350
			+ (p->art_350 * p->s_3 * p->p * p->theta) * i->Dx_250350 + (p->art_350 * p->s_3 * p->p * p->theta) * i->UnDx_250350
			+ p->delta_2 * i->Tx_A_200250 - (p->delta_1 + p->omega + p->tau_3 + p->mu) * i->Tx_A_250350);
		
		i->Tx_A_200250 += step * (+ p->sigma * i->Tx_Na_200250 + (p->art_350 * p->s_4 * p->p * p->theta) * i->PreLtfu_200250
			+ ((p->art_350 * p->p * p->gamma) + (p->art_350 * p->s_4 * p->p * p->theta)) * i->Care_200250
			+ (p->art_350 * p->s_4 * p->p * p->theta) * i->Dx_200250 + (p->art_350 * p->s_4 * p->p * p->theta) * i->UnDx_200250
			+ p->delta_3 * i->Tx_A_100200 - (p->delta_2 + p->omega + p->tau_4 + p->mu) * i->Tx_A_200250);
		
		i->Tx_A_100200 += step * (+ p->sigma * i->Tx_Na_100200 + (p->art_200 * p->s_5 * p->p * p->theta) * i->PreLtfu_100200
			+ ((p->art_200 * p->p * p->gamma) + (p->art_200 * p->s_5 * p->p * p->theta)) * i->Care_100200
			+ (p->art_200 * p->s_5 * p->p * p->theta) * i->Dx_100200 + (p->art_200 * p->s_5 * p->p * p->theta) * i->UnDx_100200
			+ p->delta_4 * i->Tx_A_50100 - (p->delta_3 + p->omega + p->tau_5 + p->mu) * i->Tx_A_100200);
		
		i->Tx_A_50100 += step * (+ p->sigma * i->Tx_Na_50100 + (p->art_200 * p->s_6 * p->p * p->theta) * i->PreLtfu_50100
			+ ((p->art_200 * p->p * p->gamma) + (p->art_200 * p->s_6 * p->p * p->theta)) * i->Care_50100
			+ (p->art_200 * p->s_6 * p->p * p->theta) * i->Dx_50100 + (p->art_200 * p->s_6 * p->p * p->theta) * i->UnDx_50100
			+ p->delta_5 * i->Tx_A_50 - (p->delta_4 + p->omega + p->tau_6 + p->mu) * i->Tx_A_50100);
		
		i->Tx_A_50 += step * (+ p->sigma * i->Tx_Na_50 + (p->art_200 * p->s_7 * p->p * p->theta) * i->PreLtfu_50
			+ ((p->art_200 * p->p * p->gamma) + (p->art_200 * p->s_7 * p->p * p->theta)) * i->Care_50
			+ (p->art_200 * p->s_7 * p->p * p->theta) * i->Dx_50 + (p->art_200 * p->s_7 * p->p * p->theta) * i->UnDx_50
			- (p->delta_5 + p->omega + p->tau_7 + p->mu) * i->Tx_A_50);
		
		/* LTFU */
		i->Ltfu_500 += step * (+ p->omega * (i->Tx_Na_500 + i->Tx_A_500) - (p->nu_1 + p->alpha_1 + p->mu) * i->Ltfu_500);
		i->Ltfu_350500 += step * (+ p->omega * (i->Tx_Na_350500 + i->Tx_A_350500) + p->nu_1 * i->Ltfu_500 - (p->nu_2 + p->alpha_2 + p->mu) * i->Ltfu_350500);
		i->Ltfu_250350 += step * (+ p->omega * (i->Tx_Na_250350 + i->Tx_A_250350) + p->nu_2 * i->Ltfu_350500 - (p->nu_3 + p->alpha_3 + p->mu) * i->Ltfu_250350);
		i->Ltfu_200250 += step * (+ p->omega * (i->Tx_Na_200250 + i->Tx_A_200250) + p->nu_3 * i->Ltfu_250350 - (p->nu_4 + p->alpha_4 + p->mu) * i->Ltfu_200250);
		i->Ltfu_100200 += step * (+ p->omega * (i->Tx_Na_100200 + i->Tx_A_100200) + p->nu_4 * i->Ltfu_200250 - (p->nu_5 + p->alpha_5 + p->mu) * i->Ltfu_100200);
		i->Ltfu_50100 += step * (+ p->omega * (i->Tx_Na_50100 + i->Tx_A_50100) + p->nu_5 * i->Ltfu_100200 - (p->nu_6 + p->alpha_6 + p->mu) * i->Ltfu_50100);
		i->Ltfu_50 += step * (+ p->omega * (i->Tx_Na_50 + i->Tx_A_50) + p->nu_6 * i->Ltfu_50100 - (p->alpha_7 + p->mu) * i->Ltfu_50);
		
		/* Other Functions */
		i->NewInf += step * (beta * (((i->UnDx_500 + i->Dx_500 + i->Care_500 + i->PreLtfu_500 + i->Tx_Na_500 + i->Ltfu_500) * 1.35)
			+ ((i->UnDx_350500 + i->Dx_350500 + i->Care_350500 + i->PreLtfu_350500 + i->Tx_Na_350500 + i->Ltfu_350500) * 1)
			+ ((i->UnDx_250350 + i->Dx_250350 + i->Care_250350 + i->PreLtfu_250350 + i->Tx_Na_250350 + i->Ltfu_250350 + i->UnDx_200250 + i->Dx_200250 + i->Care_200250 + i->PreLtfu_200250 + i->Tx_Na_200250 + i->Ltfu_200250) * 1.64)
			+ ((i->UnDx_100200 + i->Dx_100200 + i->Care_100200 + i->PreLtfu_100200 + i->Tx_Na_100200 + i->Ltfu_100200 + i->UnDx_50100 + i->Dx_50100 + i->Care_50100 + i->PreLtfu_50100 + i->Tx_Na_50100 + i->Ltfu_50100 + i->UnDx_50 + i->Dx_50 + i->Care_50 + i->PreLtfu_50 + i->Tx_Na_50 + i->Ltfu_50) * 5.17)
			+ ((i->Tx_A_500 + i->Tx_A_350500 + i->Tx_A_250350 + i->Tx_A_200250 + i->Tx_A_100200 + i->Tx_A_50100 + i->Tx_A_50) * 0.1)));
		
		i->HivMortality += step * (p->alpha_1 * (i->UnDx_500 + i->Dx_500 + i->Care_500 + i->PreLtfu_500 + i->Tx_Na_500)
			+ p->alpha_2 * (i->UnDx_350500 + i->Dx_350500 + i->Care_350500 + i->PreLtfu_350500 + i->Tx_Na_350500)
			+ p->alpha_3 * (i->UnDx_250350 + i->Dx_250350 + i->Care_250350 + i->PreLtfu_250350 + i->Tx_Na_250350)
			+ p->alpha_4 * (i->UnDx_200250 + i->Dx_200250 + i->Care_200250 + i->PreLtfu_200250 + i->Tx_Na_200250)
			+ p->alpha_5 * (i->UnDx_100200 + i->Dx_100200 + i->Care_100200 + i->PreLtfu_100200 + i->Tx_Na_100200)
			+ p->alpha_6 * (i->UnDx_50100 + i->Dx_50100 + i->Care_50100 + i->PreLtfu_50100 + i->Tx_Na_50100)
			+ p->alpha_7 * (i->UnDx_50 + i->Dx_50 + i->Care_50 + i->PreLtfu_50 + i->Tx_Na_50)
			+ p->tau_1 * i->Tx_A_500 + p->tau_2 * i->Tx_A_350500 + p->tau_3 * i->Tx_A_250350 + p->tau_4 * i->Tx_A_200250 + p->tau_5 * i->Tx_A_100200 + p->tau_6 * i->Tx_A_50100 + p->tau_7 * i->Tx_A_50);
		
		i->NaturalMortality += step * (p->mu * (i->UnDx_500 + i->UnDx_350500 + i->UnDx_250350 + i->UnDx_200250 + i->UnDx_100200 + i->UnDx_50100 + i->UnDx_50
			+ i->Dx_500 + i->Dx_350500 + i->Dx_250350 + i->Dx_200250 + i->Dx_100200 + i->Dx_50100 + i->Dx_50
			+ i->Care_500 + i->Care_350500 + i->Care_250350 + i->Care_200250 + i->Care_100200 + i->Care_50100 + i->Care_50
			+ i->PreLtfu_500 + i->PreLtfu_350500 + i->PreLtfu_250350 + i->PreLtfu_200250 + i->PreLtfu_100200 + i->PreLtfu_50100 + i->PreLtfu_50
			+ i->Tx_Na_500 + i->Tx_Na_350500 + i->Tx_Na_250350 + i->Tx_Na_200250 + i->Tx_Na_100200 + i->Tx_Na_50100 + i->Tx_Na_50
			+ i->Tx_A_500 + i->Tx_A_350500 + i->Tx_A_250350 + i->Tx_A_200250 + i->Tx_A_100200 + i->Tx_A_50100 + i->Tx_A_50
			+ i->Ltfu_500 + i->Ltfu_350500 + i->Ltfu_250350 + i->Ltfu_200250 + i->Ltfu_100200 + i->Ltfu_50100 + i->Ltfu_50));

		i->Dx_Cost += step * ((p->rho * (i->UnDx_500 + i->UnDx_350500 + i->UnDx_250350 + i->UnDx_200250 + i->UnDx_100200 + i->UnDx_50100 + i->UnDx_50)) * p->dx_unitcost);
		i->Linkage_Cost += step * ((p->epsilon * (i->Dx_500 + i->Dx_350500 + i->Dx_250350 + i->Dx_200250 + i->Dx_100200 + i->Dx_50100 + i->Dx_50)) * p->linkage_unitcost);
		i->Annual_Care_Cost += step * ((i->Care_500 + i->Care_350500 + i->Care_250350 + i->Care_200250 + i->Care_100200 + i->Care_50100 + i->Care_50) * p->annual_care_unitcost);
		i->Annual_ART_Cost += step * ((i->Tx_Na_500 + i->Tx_Na_350500 + i->Tx_Na_250350 + i->Tx_Na_200250 + i->Tx_Na_100200 + i->Tx_Na_50100 + i->Tx_Na_50 + i->Tx_A_500 + i->Tx_A_350500 + i->Tx_A_250350 + i->Tx_A_200250 + i->Tx_A_100200 + i->Tx_A_50100 + i->Tx_A_50) * p->annual_art_unitcost);
		
		/* Non-differentials */
		i->N = (i->UnDx_500 + i->UnDx_350500 + i->UnDx_250350 + i->UnDx_200250 + i->UnDx_100200 + i->UnDx_50100 + i->UnDx_50
			+ i->Dx_500 + i->Dx_350500 + i->Dx_250350 + i->Dx_200250 + i->Dx_100200 + i->Dx_50100 + i->Dx_50
			+ i->Care_500 + i->Care_350500 + i->Care_250350 + i->Care_200250 + i->Care_100200 + i->Care_50100 + i->Care_50
			+ i->PreLtfu_500 + i->PreLtfu_350500 + i->PreLtfu_250350 + i->PreLtfu_200250 + i->PreLtfu_100200 + i->PreLtfu_50100 + i->PreLtfu_50
			+ i->Tx_Na_500 + i->Tx_Na_350500 + i->Tx_Na_250350 + i->Tx_Na_200250 + i->Tx_Na_100200 + i->Tx_Na_50100 + i->Tx_Na_50
			+ i->Tx_A_500 + i->Tx_A_350500 + i->Tx_A_250350 + i->Tx_A_200250 + i->Tx_A_100200 + i->Tx_A_50100 + i->Tx_A_50
			+ i->Ltfu_500 + i->Ltfu_350500 + i->Ltfu_250350 + i->Ltfu_200250 + i->Ltfu_100200 + i->Ltfu_50100 + i->Ltfu_50);
		
	}
	cout << "Finished." << endl;
}