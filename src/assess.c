#include <math.h>
#include <stdlib.h>

void assessAbs(double *ratio, double *basal, int *nin, double *ass) {

  int i;
  *ass = 0;

  for(i=0; i<*nin; i++) {
    *ass += fabs(ratio[i]-*basal);
  }

}


void assessAbsGrad(double *ratio, double *basal, int *nin, double *dass) {

  int i;
  double fr;
  *dass = 0;

  for(i=0; i<*nin; i++) {
    fr = ratio[i]-*basal;
    *dass += (fr > 0)-(fr < 0);
  }

}


void assessSteps(double *ratio2, int *len2, double *ass) {

  int i;
  *ass = 0;
  
  for(i=1; i<*len2; i++) {
    *ass += fabs(ratio2[i]-ratio2[i-1]);
  }

}


void assessStepsGrad(double *ratio2, int *len2, double *dass) {

  int i;
  double fs;
  *dass = 0;
  
  for(i=1; i<*len2; i++) {
    fs = ratio2[i]-ratio2[i-1];
    *dass += (fs > 0)-(fs < 0);
  }

}
