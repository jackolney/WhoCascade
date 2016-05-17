README.txt

01/02/16

This alteration to the C model include an if () for altering the value of a variable half way through the calculation.

In this case, we alter beta when *t > 2.

Additionally, can we assign parameters dynamically.
e.g. I do this with beta right now.

printf("%f", parms.beta);
printf("\n");
parms.beta = <something>;
printf("%f", parms.beta);
printf("\n");
