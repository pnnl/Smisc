// Calculation of the distribution of the sum of independent binomials
// 
// Based on the exact algorithm discussed by
//   
//   Butler, Ken and Stephens, Michael. (1993) The Distribution of a Sum of
//   Binomial Random Variables. Technical Report No. 467, Department of
//   Statistics, Stanford University.
//   
//   http://www.dtic.mil/dtic/tr/fulltext/u2/a266969.pdf
// 
// Landon Sego 2011-12-05

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <Rmath.h>

//////////////////////////////////////////////////
// Calculate the mass probabilities of a binomial for x from 0 to i_j
//////////////////////////////////////////////////

void prob_x(int i_j, int i_n, double d_p, double *d_vector) {

  int i;
  double d_n = i_n;

  // These errors are checked in the R function dkbinom() & pkbinom()

  //  if (i_j < 0)
  //    error("j should be > 0");

  //  if (i_n <= 0)
  //    error("Not coded for n <= 0");

  //  if ((d_p < 0) | (d_p > 1))
  //    error("p should be in [0,1]");


  d_vector[0] = R_pow_di(1 - d_p, i_n);

//  Rprintf("d_vector[0] = %f\n", d_vector[0]);

  for (i = 1; i <= i_j; i++) {

    // once i exceeds n, result should be 0
    if (i > i_n) 
       d_vector[i] = 0;
   
    // when 0 < p < 1;
    else if ((d_p > 0) & (d_p < 1)) 
      d_vector[i] = ((d_n - i + 1) / i) * (d_p / (1 - d_p)) * d_vector[i - 1];

    // when p = 0;
    else if (d_p <= 0) 
      d_vector[i] = 0;

    // when p = 1
    else {
      if (i < i_n)
        d_vector[i] = 0;
      else
        d_vector[i] = 1;
    }

//    Rprintf("i = %i, i_n = %i, d_p = %f, d_vector[i-1] = %f, d_vector[i] = %f\n",
//             i, i_n, d_p, d_vector[i-1], d_vector[i]);

  }

} // prob_x

//////////////////////////////////////////////////
// Method for convolving two arrays (vectors) into a third one
//////////////////////////////////////////////////

void convolve(double *d_vA, double *d_vB, double *d_vC, int i_len) {

  //vA and vB get convolved to make vC, where vB is reversed
  
  int i, j;
  double d_incSum;

  for (j = 0; j <= i_len; j++) {

     d_incSum = 0;
  
     for (i = 0; i <= j; i++)
       d_incSum += d_vA[i] * d_vB[j - i];

     d_vC[j] = d_incSum;

  }

} // convolve

//////////////////////////////////////////////////
// Copying the contents of one array to another
//////////////////////////////////////////////////

void c_to_a(double *d_vA, double *d_vC, int i_len) {

  int i;

  for (i = 0; i <= i_len; i++)
    d_vA[i] = d_vC[i];

} // c_to_a


//////////////////////////////////////////////////
// Print results
//////////////////////////////////////////////////

void printAll(int k, double *d_vA, double *d_vB, double *d_vC, int i_len) {

   Rprintf("\nk = %i\n\n", k);
   
   int i;

   for (i = 0; i <= i_len; i++) 
     Rprintf("  x = %i, A[i] = %f, B[i] = %f, C[i] = %f\n",
                     i,   d_vA[i],   d_vB[i],   d_vC[i]);

   Rprintf("\n");

} // printAll

//////////////////////////////////////////////////
// This is the 'master' method called from R
//////////////////////////////////////////////////

void dkbinom(int *i_j, 
             int *i_nVec, 
             double *d_pVec, 
             int *i_numVar,
             int *i_cumulative,
             int *i_verbose,
             double *d_vecA, 
             double *d_vecB, 
             double *d_vecC,
             double *cumOut) {

   int k;

   // Initialize vecA and vecB
   prob_x(*i_j, i_nVec[0], d_pVec[0], d_vecA);
   prob_x(*i_j, i_nVec[1], d_pVec[1], d_vecB);

   // Create vecC by convolving A with B
   convolve(d_vecA, d_vecB, d_vecC, *i_j);

   if (*i_verbose)
     printAll(1, d_vecA, d_vecB, d_vecC, *i_j);

   // For each additional variable:
   if (*i_numVar > 2) {

     for (k = 2; k < *i_numVar; k++) {

       // Map the C array to A
       c_to_a(d_vecA, d_vecC, *i_j);
       
       // Calculate a new B vector with values of the kth variable
       prob_x(*i_j, i_nVec[k], d_pVec[k], d_vecB);

       // Convolve them to create a new C vector
       convolve(d_vecA, d_vecB, d_vecC, *i_j);    

       if (*i_verbose)
         printAll(k, d_vecA, d_vecB, d_vecC, *i_j);   

     }

   }

   // Calculate the cumulative probability if requested
   if (*i_cumulative) {

      double cum = 0;
      int i;
  
      for (i = 0; i <= *i_j; i++) 
        cum += d_vecC[i];
      
      *cumOut = cum;

   }
   

} // dkbinom
