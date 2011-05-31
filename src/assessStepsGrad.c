#include <math.h>
#include <stdlib.h>

void assessStepsGrad(double *lambda2, int *len2, double *dass) {

  int i;

  double dass0, dass2, sxl, syl, dlx, dyl;
  for(i=1; i<(*len2-1); i++) {
    sxl = lambda2[i] + lambda2[i-1]; // l + x
    syl = lambda2[i+1] + lambda2[i]; // y + l
    dlx = lambda2[i] - lambda2[i-1]; // l - x
    dyl = lambda2[i+1] - lambda2[i]; // y - l
    
    dass0 = 1/sqrt(sxl) - 0.5*dlx/pow(sxl, 1.5);
    dass2 = -1/sqrt(syl) - 0.5*dyl/pow(syl, 1.5);
    //R: 1/(x+lambda)^(1/2)-1/2*(lambda-x)/(x+lambda)^(3/2)
    //R: -1/(y+lambda)^(1/2)-1/2*(y-lambda)/(y+lambda)^(3/2)
    
    dass[i-1] = ((dlx > 0)-(dlx < 0))*dass0 + ((dyl > 0)-(dyl < 0))*dass2;
    //R: sign(lambda-x)*dassLast + sign(y-lambda)*dassNext
  }

}
