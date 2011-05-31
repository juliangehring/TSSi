#include <math.h>
#include <stdlib.h>

void assessAbs(double *lambda, double *basal, int *nin, double *ass) {

  int i;
  //int n = *nin;
  *ass = 0;

  for(i=0; i<*nin; i++) {
    *ass += fabs((lambda[i]-*basal)/sqrt(lambda[i]+*basal));
  }

}


/*
dyn.load("cAssessAbs.so")
.C("cAssessAbs", as.double(11:15), as.double(1:5), as.integer(5), double(1), double(5))
*/
