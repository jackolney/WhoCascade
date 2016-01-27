//
//  parameters.cpp
//  WhoCascade
//
//  Created by Jack Olney on 17/01/2016.
//  Copyright © 2016 Jack Olney. All rights reserved.
//

#include "parameters.h"

params::params() :
nu_1(0.193634),
nu_2(0.321304),
nu_3(0.328285),
nu_4(0.497247),
nu_5(0.559090),
nu_6(0.846406),
rho(0.205),
epsilon(16.949),
kappa(1.079),
gamma(2.556),
theta(1.511),
omega(0.033),
p(0.95),
s_1(0.1),
s_2(0.4),
s_3(0.7),
s_4(1),
s_5(1.3),
s_6(1.6),
s_7(1.9),
sigma(0),
delta_1(0.5178),
delta_2(0.8862),
delta_3(0.8832),
delta_4(1.1581),
delta_5(2.5663),
alpha_1(0.004110),
alpha_2(0.011670),
alpha_3(0.009385),
alpha_4(0.016394),
alpha_5(0.027656),
alpha_6(0.047877),
alpha_7(1.081964),
tau_1(0.003905),
tau_2(0.011087),
tau_3(0.008916),
tau_4(0.015574),
tau_5(0.026273),
tau_6(0.045482),
tau_7(1.02785),
mu(0.0374),
art_all(1), // ART initiation threshold triggers
art_500(1),
art_350(1),
art_200(1),
dx_unitcost(10),
linkage_unitcost(40),
annual_care_unitcost(40),
annual_art_unitcost(367)
{}

params::~params() {}
