//
//  parameters.h
//  WhoCascade
//
//  Created by Jack Olney on 17/01/2016.
//  Copyright © 2016 Jack Olney. All rights reserved.
//

#ifndef parameters_h
#define parameters_h

#include <stdio.h>

class params {
public:
	params(); // constructor
	~params(); // destructor
	
	/* stuff */
	double nu_1;
	double nu_2;
	double nu_3;
	double nu_4;
	double nu_5;
	double nu_6;
	double rho;
	double epsilon;
	double kappa;
	double gamma;
	double theta;
	double omega;
	double p;
	double s_1;
	double s_2;
	double s_3;
	double s_4;
	double s_5;
	double s_6;
	double s_7;
	double sigma;
	double delta_1;
	double delta_2;
	double delta_3;
	double delta_4;
	double delta_5;
	double alpha_1;
	double alpha_2;
	double alpha_3;
	double alpha_4;
	double alpha_5;
	double alpha_6;
	double alpha_7;
	double tau_1;
	double tau_2;
	double tau_3;
	double tau_4;
	double tau_5;
	double tau_6;
	double tau_7;
	double mu;
	double art_all;
	double art_500;
	double art_350;
	double art_200;
	double dx_unitcost;
	double linkage_unitcost;
	double annual_care_unitcost;
	double annual_art_unitcost;
};

#endif /* parameters_h */
