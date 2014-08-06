// Calculates the cumsum of a vector.  When it encounters an NA, it does not propogate it

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <Rmath.h>

void cumsumNAdouble(double *X,   // The vector to be cumulatively summed
                    int *n,     // the length of X 
                    double *cumsum) // The output vector

{

  int i;
  double cum = 0;

  for (i = 0; i < *n; i++) {

    if (!ISNA(X[i])) {
      cum += X[i];
      cumsum[i] = cum;
    }
    else
      cumsum[i] = NA_REAL;
    
  }

} 

void cumsumNAint(int *X,   // The vector to be cumulatively summed
                 int *n,   // the length of X 
                 int *cumsum) // The output vector

{

  int i, cum = 0;

  for (i = 0; i < *n; i++) {

    if (X[i] != NA_INTEGER) {
      cum += X[i];
      cumsum[i] = cum;
    }
    else
      cumsum[i] = NA_INTEGER;

  }

} 
