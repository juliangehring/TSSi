#include <math.h>
#include <stdlib.h>

void assessAbsGrad(double *ratio, double *basal, int *nin, double *dass) {

  int i;
  double fr;
  *dass = 0;

  for(i=0; i<*nin; i++) {
    fr = round((ratio[i]-*basal)*1e4)/1e4;
    *dass += (fr > 0)-(fr < 0);
  }

}
