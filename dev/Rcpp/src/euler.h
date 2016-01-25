//
//  euler.h
//  WhoCascade
//
//  Created by Jack Olney on 17/01/2016.
//  Copyright © 2016 Jack Olney. All rights reserved.
//

#ifndef euler_h
#define euler_h

#include <stdio.h>
#include "parameters.h"
#include "initial.h"

void Euler(initial * i, params * p, double start, double stop, double step);

#endif /* euler_h */
