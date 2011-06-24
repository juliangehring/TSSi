#include <math.h>
#include <stdlib.h>

void assessStepsGrad(double *lambda2, int *len2, double *dass) {

  int i;
  double fs;
  *dass = 0;
  
  for(i=1; i<*len2; i++) {
    fs = lambda2[i]-lambda2[i-1];
    *dass += (fs > 0)-(fs < 0);
  }

}
