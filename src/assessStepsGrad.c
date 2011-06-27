#include <math.h>
#include <stdlib.h>

void assessStepsGrad(double *ratio2, int *len2, double *dass) {

  int i;
  double fs;
  *dass = 0;
  
  for(i=1; i<*len2; i++) {
    fs = round((ratio2[i]-ratio2[i-1])*1e4)/1e4;
    *dass += (fs > 0)-(fs < 0);
  }

}
