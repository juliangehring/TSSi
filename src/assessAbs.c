#include <math.h>
#include <stdlib.h>

void assessAbs(double *ratio, double *basal, int *nin, double *ass) {

  int i;
  *ass = 0;

  for(i=0; i<*nin; i++) {
    *ass += fabs(ratio[i]-*basal);
  }

}
