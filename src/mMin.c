void mMin(double *y, int *N, int *ny, double *res) {
  int i, j;
  double temp;
  for (i = *N - 1L; i < *ny; i++) {
    temp = y[i];
    for (j = i - *N + 1L; j < i; j++) {
      if (y[j] < temp)
        temp = y[j];
    }
    res[i] = temp;
  }     
}
