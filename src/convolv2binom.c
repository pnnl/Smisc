// Calculate the mass function or the distribution function of 
// the sum of 2 independent binomial random variables (that have different
// probabilities) using a convolution.

// Landon Sego,  3/1/2008


#include <R.h>
#include <Rmath.h>

void d2binom(int *z,
             int *nX,
             double *pX,
             int *nY,
             double *pY,
             double *fZ)
{
  int i;
  double p = 0;

  for (i = 0; i <= *z; i++)
    p += dbinom(i, *nX, *pX, 0) * dbinom(*z - i, *nY, *pY, 0);
   
  *fZ = p;

}


void p2binom(int *z,
             int *nX,
             double *pX,
             int *nY,
             double *pY,
             double *FZ)
{
  int i, j;
  double p = 0;

  if (*z < (*nX + *nY) / 2) {

    for (i = 0; i <= *z; i++) 
      for (j = 0; j <= i; j++) 
        p += dbinom(j, *nX, *pX, 0) * dbinom(i - j, *nY, *pY, 0); 

    *FZ = p;

  }

  else {

    for (i = *z + 1; i <= *nX + *nY; i++) 
      for (j = 0; j <= i; j++) 
        p += dbinom(j, *nX, *pX, 0) * dbinom(i - j, *nY, *pY, 0); 

    *FZ = 1 - p;

  }
   

} // p2binom
