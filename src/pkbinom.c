// Calculation of the cumulative distribution of the sum of independent binomials
// A naive approach
//
// Landon Sego 2014-12-04

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <Rmath.h>

//////////////////////////////////////////////////
// Calculate the mass probabilities of a binomial for x from 0 to i_j
//////////////////////////////////////////////////

void pkbinom(int *i_n,     // Vector of size 5, 
             double *d_p,  // Vector of size 5
             int *i_t,     // Single integer
             double *d_out) {

  // Counting indexes
  int i, j, k, l, m; 

  // Arrays of binomial probabilities
  double d_p0[*i_t + 1], d_p1[*i_t + 1], d_p2[*i_t + 1], d_p3[*i_t + 1], d_p4[*i_t + 1];

  // Agregating probabilty
  double d_cdf = 0, d_i, d_j, d_k, d_l;

  // Calculate the binomial probabilities
  for (i = 0; i <= *i_t; i++) {
    d_p0[i] = dbinom(i, i_n[0], d_p[0], 0);
    d_p1[i] = dbinom(i, i_n[1], d_p[1], 0);
    d_p2[i] = dbinom(i, i_n[2], d_p[2], 0);
    d_p3[i] = dbinom(i, i_n[3], d_p[3], 0);
    d_p4[i] = dbinom(i, i_n[4], d_p[4], 0);
	}

  // Now aggregate the probabilities
  for (i = 0; i <= imin2(i_n[0], *i_t); i++) {

    d_i = d_p0[i];

    for (j = 0; j <= imin2(i_n[1], *i_t - i); j++) {

      d_j = d_i * d_p1[j];

      for (k = 0; k <= imin2(i_n[2], *i_t - i - j); k++) {

        d_k = d_j * d_p2[k];

        for (l = 0; l <= imin2(i_n[3], *i_t - i - j - k); l++) {

          d_l = d_k * d_p3[l];

          for (m = 0; m <= imin2(i_n[4], *i_t - i - j - k -l); m++) {

						d_cdf += d_l * d_p4[m];

					}
				}
			}
		}
	}

 *d_out = d_cdf;

} // pkbinom
