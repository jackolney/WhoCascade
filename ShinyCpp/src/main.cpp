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

// R-stuff
#include <Rdefines.h>
#include <stdio.h>
#include <R.h>

using namespace std;

extern "C" {

    SEXP Cascade() {

    params * p = new params;
    
    initial * i = new initial();
    
    Euler(i, p, 0, 10,  0.02);

    // Test Output Vector
    SEXP OutputVector;
    PROTECT(OutputVector = allocVector(REALSXP, 10 / 0.02));
    double * pOutputVector = REAL(OutputVector);

    SEXP VectorVector;
    PROTECT(VectorVector = allocVector(VECSXP,1));
    SET_VECTOR_ELT(VectorVector,0,OutputVector);
    // double * pVectorVector = coerceVector(VectorVector,VECSXP);

    cout << "This = " << VECTOR_ELT(VectorVector,0) << endl;
    cout << "Now This = " << VectorVector << endl;



    SEXP VectorNames;
    PROTECT(VectorNames = allocVector(VECSXP,1));
    SET_VECTOR_ELT(VectorNames,0,mkChar("Test"));
    namesgets(VectorVector,VectorNames);

    for(int i = 0; i<500; i++) {
        pOutputVector[i] = 15;
        // Where VectorStruct is part of a data structure (containing Vectors),
        // That Euler() fills in as it goes along.
        // Pass a vector of vectors TO the Euler();
        // Fills out then WE RETURN THAT!
    }

    // cout << "Test = " << pVectorVector[i] << endl;

    // warning("this is warning."); // prints a warning to R console.

    delete p;
    delete i;

    UNPROTECT(3);
    return(VectorVector);
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

// Need to implement a two-state model

// A = 100
// B = 0

// Need an efficient structure that allows me to create a starting size, for all...
// Classes
// Class of 'ODE_FUNCTION' then create instance of them for use in the model with specific functions attached.
// How will these interface with Euler() then?


// IDEAL SITUATION
// Euler(start, stop, step)
// Then inside Euler, is the functions that are called to run the model? (a different function for each ODE?)
// Maintain 'pure' functional programming ethos?

// For this initial test, then just create two separate functions.


// Steps:
// 1) Can we put everything into functions? (keeps it neat) - Done.
// 2) Sanity check to R output / Berkeley Madonna output - Done.
// 3) Needs to take some inputs... - Done.
// 4) TAKE OUT OF Xcode
// 5) Respond by returning a whole heap of vectors to R.
// 6) EXPAND
// 7) R .Call()

// Development Notes:
// Beta needs to be assigned at the beginning of each simulation. It is a pre-sim calculation. Based on "new infections" -> pass this to C++ and let it do the rest.
// Beta placeholder is: 0.02466

// R Wish List:
// 1) Compile and cout results (N) - Done.
// 2) Return a vector - Ongoing.
// 3) Take an arguement
// 4) All together now.

// Fun Stuff:
// Dynamic change of length of output vectors that matches (StopTime / Step) of model.