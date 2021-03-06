// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// sum_cpp
double sum_cpp(NumericVector x);
RcppExport SEXP _onezero_sum_cpp(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(sum_cpp(x));
    return rcpp_result_gen;
END_RCPP
}
// weighted_mean
double weighted_mean(NumericVector x, NumericVector w);
RcppExport SEXP _onezero_weighted_mean(SEXP xSEXP, SEXP wSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type w(wSEXP);
    rcpp_result_gen = Rcpp::wrap(weighted_mean(x, w));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_onezero_sum_cpp", (DL_FUNC) &_onezero_sum_cpp, 1},
    {"_onezero_weighted_mean", (DL_FUNC) &_onezero_weighted_mean, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_onezero(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
