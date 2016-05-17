//
//  output.cpp
//  WhoCascade
//
//  Created by Jack Olney on 25/01/2016.
//  Copyright © 2016 Jack Olney. All rights reserved.
//

#include "output.h"

output::output(int stop)
{
    for (int i = 0; i < stop; ++i) {
        N[i] = 0;
        UnDx_500[i] = 0;
        UnDx_350500[i] = 0;
        UnDx_250350[i] = 0;
        UnDx_200250[i] = 0;
        UnDx_100200[i] = 0;
        UnDx_50100[i] = 0;
        UnDx_50[i] = 0;
        Dx_500[i] = 0;
        Dx_350500[i] = 0;
        Dx_250350[i] = 0;
        Dx_200250[i] = 0;
        Dx_100200[i] = 0;
        Dx_50100[i] = 0;
        Dx_50[i] = 0;
        Care_500[i] = 0;
        Care_350500[i] = 0;
        Care_250350[i] = 0;
        Care_200250[i] = 0;
        Care_100200[i] = 0;
        Care_50100[i] = 0;
        Care_50[i] = 0;
        PreLtfu_500[i] = 0;
        PreLtfu_350500[i] = 0;
        PreLtfu_250350[i] = 0;
        PreLtfu_200250[i] = 0;
        PreLtfu_100200[i] = 0;
        PreLtfu_50100[i] = 0;
        PreLtfu_50[i] = 0;
        Tx_Na_500[i] = 0;
        Tx_Na_350500[i] = 0;
        Tx_Na_250350[i] = 0;
        Tx_Na_200250[i] = 0;
        Tx_Na_100200[i] = 0;
        Tx_Na_50100[i] = 0;
        Tx_Na_50[i] = 0;
        Tx_A_500[i] = 0;
        Tx_A_350500[i] = 0;
        Tx_A_250350[i] = 0;
        Tx_A_200250[i] = 0;
        Tx_A_100200[i] = 0;
        Tx_A_50100[i] = 0;
        Tx_A_50[i] = 0;
        Ltfu_500[i] = 0;
        Ltfu_350500[i] = 0;
        Ltfu_250350[i] = 0;
        Ltfu_200250[i] = 0;
        Ltfu_100200[i] = 0;
        Ltfu_50100[i] = 0;
        Ltfu_50[i] = 0;
        NewInf[i] = 0;
        HivMortality[i] = 0;
        NaturalMortality[i] = 0;
        Dx_Cost[i] = 0;
        Linkage_Cost[i] = 0;
        Annual_Care_Cost[i] = 0;
        Annual_ART_Cost[i] = 0;
    }
}

output::~output()
{}
