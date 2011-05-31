#include <math.h>
#include <stdlib.h>

void assessSteps(double *lambda2, int *len2, double *ass) {

  int i;
  *ass = 0;
  
  for(i=1; i<*len2; i++) {
    *ass += fabs((lambda2[i]-lambda2[i-1])/sqrt(lambda2[i]+lambda2[i-1]));
    //R: ass <- sum(abs(diff(lambda2)/sqrt(lambda2[-len]+lambda2[-1])))
  }

}
