#include <R.h>
#include <R_ext/Rdynload.h>
#include "assess.h"


static const
R_CMethodDef cMethods[] = {
  {"assessAbs", (DL_FUNC) &assessAbs, 4},
  {"assessAbsGrad", (DL_FUNC) &assessAbsGrad, 4},
  {"assessSteps", (DL_FUNC) &assessSteps, 3},
  {"assessStepsGrad", (DL_FUNC) &assessStepsGrad, 3},
  {NULL, NULL, 0}
};


void R_init_TSSi(DllInfo *info) {
  R_registerRoutines(info, cMethods, NULL, NULL, NULL);
}
