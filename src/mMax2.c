void mMax2(double *y, int *N, int *ny, double *res) {
  int i, j;
  int pos = -1L;
  double max = y[*N - 1L] + 1.0;
  for (i = *N - 1L; i < *ny; i++) {
    if (y[i] > max) {
      max = y[i];
      pos = i;
    } else if (pos < i - *N + 1L) {
      max = y[i];
      pos = i;   
      for (j = i - *N + 1L; j < i; j++) {
        if (y[j] > max) {
          max = y[j];
          pos = j;
        }    
      }
    }
    res[i] = max;
  }     
}
