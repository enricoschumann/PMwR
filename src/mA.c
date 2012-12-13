void mA(double *y, int *N, int *ny, double *res) {
  int i;
  for (i = 0; i < *N; i++) 
    res[*N - 1] += y[i];
  res[*N - 1] /= *N;
  for (i = *N; i < *ny; i++)
    res[i] = res[i - 1] + (y[i] - y[i - *N]) / *N;
}
