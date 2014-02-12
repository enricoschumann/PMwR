#include <R.h>
#include <Rinternals.h>

SEXP mSum2(SEXP y, SEXP n) {
  int i;
  int N = asInteger(n); /* order */
  int ny = LENGTH(y);
  SEXP ans = allocVector(REALSXP, ny);
  double *ansp = REAL(ans);
  double *yp = REAL(y);
  for (i = 0; i < N; i++) 
    ansp[N - 1] += yp[i];
  for (i = N; i < ny; i++)
    ansp[i] = ansp[i - 1] + yp[i] - yp[i - N];
  return ans;
}
