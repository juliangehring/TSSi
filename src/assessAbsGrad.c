#include <math.h>
#include <stdlib.h>

void assessAbsGrad(double *lambda, double *basal, int *nin, double *dass) {

  int i;
  double fr;
  *dass = 0;

  for(i=0; i<*nin; i++) {
    fr = lambda[i]-*basal;
    *dass += (fr > 0)-(fr < 0);
  }

}
