double av(double d) {
  if (d < 0)
    return -d;
  else 
    return d;  
}
void mSum(double *y, int *N, int *ny, double *res) {
  int i;
  for (i = 0; i < *N; i++) 
    res[*N - 1] += y[i];
  for (i = *N; i < *ny; i++)
    res[i] = res[i - 1] + y[i] - y[i - *N];
}
void mAbsSum(double* y, int* N, int* ny, double* res) {
  int i;
  for (i = 0; i < *N; i++) 
    res[*N - 1] += av(y[i]);
  for (i = *N; i < *ny; i++)
    res[i] = res[i - 1] + av(y[i]) - av(y[i - *N]);
}
