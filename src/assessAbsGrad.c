#include <math.h>
#include <stdlib.h>

void assessAbsGrad(double *lambda, double *basal, int *nin, double *dass) {

  int i;
  //int n = *nin;

  for(i=0; i<*nin; i++) {
    dass[i] = fabs(1/sqrt(lambda[i]+*basal)-0.5*(lambda[i]-*basal)/pow(lambda[i]+*basal, 1.5));
  }

}


/*
dyn.load("cAssessAbs.so")
.C("cAssessAbs", as.double(11:15), as.double(1:5), as.integer(5), double(1), double(5))
*/
