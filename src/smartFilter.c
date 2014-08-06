// This code fits calculates a moving dot-product across an array (vector) of data
// Landon Sego, 2008-04-15

/* do this first to get the right options for math.h */
#include <R_ext/Arith.h>
#include <R.h>
#include <Rmath.h>

void smartFilter(double *y,   // The data array
                 int *nNAy,   // Indicator array corresponding to the data array:  1 is not missing, 0 is missing
                 int *n,      // The length of y
                 double *x1,  // The weights to multiply against the data array
                 int *bwidth, // The bandwidth of the window. The length of window is (2 * *bw + 1). Must be >= 1
                 int *mwin,   // The minimum number of data points allowed for the dot-product to be calculated to the window (must be <= window length)
                 int *winCen, // The indexes of y that indicate the center of the windows
                 int *numWin, // The number of windows to fit
                 int *balance,// Indicator of whether the window must be balanced when missing data are present in order to calculate the
                              // dot product.  See conditions 2 and 3 below.
                 double *yOut)// The output of the moving dot product
{
    int win_i,           // Index of the elements in *winCen
        i,               // Index for the corresponding elements of the data array
        j,               // Index the elements in the moving window contained in 'win'
        k,               // A convenience index, a function of i, j, and bw
        bw = *bwidth,    // The bandwidth of the window
        nw = 2 * bw + 1, // The length of the window contained in 'win'
        nn = *n,         // The total length of y, the data array
        indMissing,      // Bernoulli indicator of whether there are any missing data in the window
        winC[nw],        // Bernoulli array of the elements of the window that are missing (1=present, 0=missing)
        first_x1,        // Window index of first non-missing data point in the window
        last_x1,         // Window index of last non-missing data point in the window
        getfirstx1,      // A flag to aid in finding the first_x1
        countI;          // An integer count of the number of non-missing data points in the window

    double win[nw],      // Array containing the moving window of data
           yOut_i,       // ith value of the yOut (the moving average)
           sx1new;       // Sum of weights which don't correspond to NA's in the window


    // Sanity checks
    if (*mwin < 1)
      error("The minimum number of non-missing data points in the window must be >= 1\n");
    if (*mwin > nw)
      error("The minimum number of non-missing data points in the window\n  must be <= length of the weight vector\n");
    if (nn < *mwin)
      error("The length of the data vector must be >= minimum number of\n  non-missing data points in the window\n");

    // Loop over the center of each window that will be grabbed from y
    for (win_i = 0; win_i < *numWin; win_i++) {

      // Identify the index for the center of this window
      i = winCen[win_i];

      // Create the win array that contains the window of interest
      // If not near the edge of the series
      if (!(i - bw < 0 || i + bw >= nn)) {
        for (j = 0; j < nw; j++) {
          k = i - bw + j;
          win[j] = y[k];
          winC[j] = nNAy[k];
        }
      }

      // else near the edge of the series:  pad the edges with NA's as necessary
      else {
        for (j = 0; j < nw; j++) {
          k = i - bw + j;
          if (!(k < 0 || k >= nn)) {
            win[j] = y[k];
            winC[j] = nNAy[k];
          }
          else {
            win[j] = NA_REAL;
            winC[j] = 0;
          }
        }
      }

      // Initialize the variable which indicates if any of the
      // data in the window are missing
      indMissing = 0;
      yOut_i = 0;

      // Loop over the window to calculate the weighted average
      for (j = 0; j < nw; j++) {
        if (winC[j]) 
          yOut_i += x1[j] * win[j];
        else {
          indMissing = 1;
          break;
        }
      } // for (j = 0; ... Finish looping over the window


      // Assign the value of the dot product if no elements were missing in the window
      if (!indMissing) 
        yOut[win_i] = yOut_i;

      // If there are missing values in window re-scale the weights and calculate the 
      // dot product again
      else {

        // Initialize the needed variables
        countI = 0; yOut_i = 0; 
        sx1new = 0; first_x1 = bw + 1; last_x1 = bw - 1; getfirstx1 = 1;

        // Create the array that indicates the location of missing values,
        // count the number of values present in the window,
        // sum the x1 values in the window.  
        // Identify the min and max x1 values
        for (j = 0; j < nw; j++) {
          if (winC[j]) {
            sx1new += x1[j];
            yOut_i += x1[j] * win[j];
            countI += 1;
            last_x1 = j;
            if (getfirstx1) {
              first_x1 = j;
              getfirstx1 = 0;
            }
          }
        }

        // Verify that window is of sufficient size and that there is at least one data point on each side
        // of the window, where the center point qualifies as a 'side'. It ensures there won't be a window that
        // looks like this:  NA NA NA NA NA NA NA NA NA 7 2 3 l 4 7

        // The following 3 conditions are being checked:

        // 1. Num of nonNA is at least as big as the min window size
        if (countI >= *mwin) {
  
          // 2. First nonNA occurs on or before center point of window
          // 3. Last nonNA occurs on or after center point of window
          if (*balance) {
            if ((first_x1 <= bw) & (last_x1 >= bw)) 
              yOut[win_i] = yOut_i / sx1new;
            else
              yOut[win_i] = NA_REAL;
	  }
          else
            yOut[win_i] = yOut_i / sx1new;
	}

        // else the window did not have sufficient data points to calculate the dot product
        else 
          yOut[win_i] = NA_REAL;

      } // else if (indMissing) : if there were missing values in the window

    } // for (win_i -- (initial loop)

} // end smartFilter

