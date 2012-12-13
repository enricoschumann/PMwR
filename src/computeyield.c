#include<Rmath.h>
void computeyield(double *c, double *t, double *y,
                  double *tol, double *h,
                  int *n, int *maxit) {

  int i;                     /* loop counter          */
  int it = 0;                /* max.it counter        */
  double y_old = y[0] + 1.0;
  double tmp1, tmp2, tmp3, tmp4;
  
  while (fabs(y[0] - y_old) > *tol) {
    if (++it > *maxit) break;
    y_old = y[0];
    tmp3 = 0; tmp4 = 0;
    for (i = 0; i < *n; ++i) {
      tmp1 = c[i];
      tmp2 = t[i];
      tmp3 += tmp1 / pow(1.0 + y[0],      tmp2);
      tmp4 += tmp1 / pow(1.0 + y[0] + *h, tmp2);
    }
    y[0] -= (*h) * tmp3/(tmp4 - tmp3);
  }
}
