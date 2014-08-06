// Calculate psi_n(t) from equation 13.4 in Gilbert (1987, p 165)
// in order to calculate the UMVUE of log-normal data

// Landon Sego,  2008-10-13


#include <R.h>
#include <Rmath.h>

void psi_n_t(int *n,
             double *t,
             double *tol,
             int *verbose,
             double *out)
{
  int i = 2, finish = 10000;            // The loop will begin by calculating the 2nd partial sum
  double psi_i = -99,                   // ith summand of psi
         psi_i1 = (*n - 1) * *t / *n,   // (i+1)th summand of psi, initialized high to avoid converenge on first iteration
         psi = 1 + psi_i1,              // ith partial sum of psi (this is the 1st partial sum)
         conv = 99999;                  // Measure of convergence


  while (i < finish) {

    psi_i1 *= (*n - 1) * (*n - 1) *  *t / (i * *n * (*n + 2 * i - 3));
 
    // Compute the ith partial sum of psi
    psi += psi_i1;  

    // Compute the convergence
    if (psi_i1 > psi_i)
      conv = psi_i1 - psi_i;
    else
      conv = psi_i - psi_i1;

    // increment the counter
    i++;

    // Write the results of each iteration
    if (*verbose)
      Rprintf("i = %i \tpsi_i1 = %f \tpsi = %f\t conv = %f\n", i, psi_i1, psi, conv);

    // Upon reaching convergence, break the loop
    if (conv < *tol)
      break;

    // Record the second to the last summand
    psi_i = psi_i1;

  }

  if (i >= finish)
    warning("Convergence not acheived\n");

  *out = psi;

} // psi_n_t
