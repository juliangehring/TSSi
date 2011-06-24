#include <math.h>
#include <stdlib.h>

void assessSteps(double *ratio2, int *len2, double *ass) {

  int i;
  *ass = 0;
  
  for(i=1; i<*len2; i++) {
    *ass += fabs(ratio2[i]-ratio2[i-1]);
  }

}
