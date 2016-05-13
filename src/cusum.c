// Calculates a CUSUM of the form
// C[i] = max(0, C[i-1] + X[i] - k)
// Signal occurs when C[i] > h

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <Rmath.h>

void cusum(double *X,
           double *k,
           double *h,
           double *initial,
           int *reset,
           int *upper,
           int *n,
           double *cusum,
					 //           double *stagger,
           int *resetCounter)

{

  int i, resetCnt = 1;

  // Initialize the CUSUM
  double previous = *initial;

  if (*upper) {

    for (i=0; i < *n; i++) {

      // If reset
      if (*reset) {
        if (previous > *h) {
          previous = 0;
          resetCnt++;
	      }
      }

      // Record the staggered CUSUM to make it 
      // easier to calculate the random hazard 
			//      stagger[i] = previous;
  
      // Assign the value of the resetCounter
      resetCounter[i] = resetCnt;
  
      // Calculate the CUSUM
      cusum[i] = previous + X[i] - *k;
  
      // Bound below at 0
      if (cusum[i] < 0)
        cusum[i] = 0;
  
      // Set the previous value for the next iteration
      previous = cusum[i];
  
    } // for

  } // if (*upper)

  // Lower cusum to be coded later
  // else {

  // }

} // void cusum
