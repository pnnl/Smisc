// This code fits calculates the max up to the current index
// Landon Sego, 2008-09-20

#include <R.h>

void max_le_i_INT(int *y, int *ny, int *yOut) 
{

  int i, currentMax = y[0] - 1;

  for (i = 0; i < *ny; i++) {

    if (y[i] > currentMax) 
      currentMax = y[i];
    
    yOut[i] = currentMax;

  }

} // void max_le_i_INT

void max_le_i_DOUBLE(double *y, int *ny, double *yOut) 
{

  int i;
  double currentMax = y[0] - 1;

  for (i = 0; i < *ny; i++) {

    if (y[i] > currentMax) 
      currentMax = y[i];
    
    yOut[i] = currentMax;

  }

} // max_le_i_DOUBLE

