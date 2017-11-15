#include <stdlib.h> 
#include <R_ext/Rdynload.h>

/* .C calls */
extern void cumsumNAdouble(double *X,
													 int *n,
													 double *cumsum);

extern void cumsumNAint(int *X,
												int *n,
												int *cusum);

extern void cusum(double *X,
									double *k,
									double *h,
									double *initial,
									int *reset,
									int *upper,
									int *n,
									double *cusum,
									int *resetCounter);

extern void dkbinom(int *i_j, 
                    int *i_nVec, 
                    double *d_pVec, 
                    int *i_numVar,
                    int *i_cumulative,
                    int *i_verbose,
                    double *d_vecA, 
                    double *d_vecB, 
                    double *d_vecC,
										double *cumOut);

extern void max_le_i_DOUBLE(double *y,
														int *ny,
														double *yOut);

extern void max_le_i_INT(int *y,
												 int *ny,
												 int *yOut);

extern void pkbinom(int *i_n,
                    double *d_p,
                    int *i_t,
                    double *d_out);

extern void psi_n_t(int *n,
                    double *t,
                    double *tol,
                    int *verbose,
                    double *out);

extern void smartFilter(double *y,
                        int *nNAy,
                        int *n,
                        double *x1,
                        int *bwidth,
                        int *mwin,
                        int *winCen,
                        int *numWin,
                        int *balance,
                        double *yOut);

static const R_CMethodDef CEntries[] = {
    {"cumsumNAdouble",  (DL_FUNC) &cumsumNAdouble,   3},
    {"cumsumNAint",     (DL_FUNC) &cumsumNAint,      3},
    {"cusum_c",         (DL_FUNC) &cusum,            9},
    {"dkbinom_c",       (DL_FUNC) &dkbinom,         10},
    {"max_le_i_DOUBLE", (DL_FUNC) &max_le_i_DOUBLE,  3},
    {"max_le_i_INT",    (DL_FUNC) &max_le_i_INT,     3},
    {"pkbinom_c",       (DL_FUNC) &pkbinom,          4},
    {"psi_n_t_c",       (DL_FUNC) &psi_n_t,          5},
    {"smartFilter_c",   (DL_FUNC) &smartFilter,     10},
    {NULL, NULL, 0}
};

void R_init_Smisc(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
