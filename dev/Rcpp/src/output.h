//
//  output.h
//  WhoCascade
//
//  Created by Jack Olney on 25/01/2016.
//  Copyright © 2016 Jack Olney. All rights reserved.
//

#ifndef output_h
#define output_h

class output {
public:
    output(int stop);
    ~output();

    /* Fill with some shit */
    double N[500];
    double UnDx_500[500];
    double UnDx_350500[500];
    double UnDx_250350[500];
    double UnDx_200250[500];
    double UnDx_100200[500];
    double UnDx_50100[500];
    double UnDx_50[500];
    double Dx_500[500];
    double Dx_350500[500];
    double Dx_250350[500];
    double Dx_200250[500];
    double Dx_100200[500];
    double Dx_50100[500];
    double Dx_50[500];
    double Care_500[500];
    double Care_350500[500];
    double Care_250350[500];
    double Care_200250[500];
    double Care_100200[500];
    double Care_50100[500];
    double Care_50[500];
    double PreLtfu_500[500];
    double PreLtfu_350500[500];
    double PreLtfu_250350[500];
    double PreLtfu_200250[500];
    double PreLtfu_100200[500];
    double PreLtfu_50100[500];
    double PreLtfu_50[500];
    double Tx_Na_500[500];
    double Tx_Na_350500[500];
    double Tx_Na_250350[500];
    double Tx_Na_200250[500];
    double Tx_Na_100200[500];
    double Tx_Na_50100[500];
    double Tx_Na_50[500];
    double Tx_A_500[500];
    double Tx_A_350500[500];
    double Tx_A_250350[500];
    double Tx_A_200250[500];
    double Tx_A_100200[500];
    double Tx_A_50100[500];
    double Tx_A_50[500];
    double Ltfu_500[500];
    double Ltfu_350500[500];
    double Ltfu_250350[500];
    double Ltfu_200250[500];
    double Ltfu_100200[500];
    double Ltfu_50100[500];
    double Ltfu_50[500];
    double NewInf[500];
    double HivMortality[500];
    double NaturalMortality[500];
    double Dx_Cost[500];
    double Linkage_Cost[500];
    double Annual_Care_Cost[500];
    double Annual_ART_Cost[500];
};

#endif /* output_h */
