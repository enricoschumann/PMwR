void mMax(double *y, int *N, int *ny, double *res) {
  int i, j;
  double temp;
  for (i = *N - 1; i < *ny; i++) {
    temp = y[i];
    for (j = i - *N + 1L; j < i; j++) {
      if (y[j] > temp)
        temp = y[j];
    }
    res[i] = temp;
  }     
}
