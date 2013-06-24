void mMin2(double *y, int *N, int *ny, double *res) {
  int i, j;
  int pos = -1L;
  double min = y[*N - 1L] - 1.0;
  for (i = *N - 1L; i < *ny; i++) {
    if (y[i] < min) {
      min = y[i];
      pos = i;
    } else if (pos < i - *N + 1L) {
      min = y[i];
      pos = i;   
      for (j = i - *N + 1L; j < i; j++) {
        if (y[j] < min) {
          min = y[j];
          pos = j;
        }    
      }
    }
    res[i] = min;
  }     
}
