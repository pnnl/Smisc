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
  int i = 3, finish = 10000;            // The loop will begin by calculating the 3nd partial sum
  double psi_i = (*n - 1) * *t / *n,    // (i+1)th summand of psi, (this is the 2nd summand in psi)
         psi = 1 + psi_i;               // ith partial sum of psi (this is the 2nd partial sum)

  if (*verbose) {
    Rprintf("i = 1 \tpsi_i = 1.0000000000000000 \tpsi = 1.0000000000000000\n");
    Rprintf("i = 2 \tpsi_i = %.16f \tpsi = %.16f\n", psi_i, psi);
	}

  while (i < finish) {

    psi_i *= (*n - 1) * (*n - 1) *  *t / ((i - 1) * *n * (*n + 2 * i - 5));
 
    // Compute the ith partial sum of psi
    psi += psi_i;  

    // Write the results of each iteration
    if (*verbose)
      Rprintf("i = %i \tpsi_i = %.16f \tpsi = %.16f\n", i, psi_i, psi);

    // Determine if we have convergence
    if (psi_i > 0) {
      if (psi_i < *tol)
        break;
		}
    else if (psi_i < 0) {
      if (-1 * psi_i < *tol)
        break;
		}
    else 
      break;

    // increment the counter
    i++;

  }

  if (i >= finish)
    warning("Convergence not acheived\n");

  *out = psi;

} // psi_n_t
