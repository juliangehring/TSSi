#include <math.h>
#include <stdlib.h>

void assessStepsGrad(double *ratio2, int *len2, double *dass) {

  int i;
  double fs;
  *dass = 0;
  
  for(i=1; i<*len2; i++) {
    fs = ratio2[i]-ratio2[i-1];
    *dass += (fs > 0)-(fs < 0);
  }

}
