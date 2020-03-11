#include <Rcpp.h>
using namespace Rcpp;

//' Weighted Mean.
//'
//' @param x A numeric vector.
//' @param w A numeric vector of weights.
//'
//' @export
// [[Rcpp::export]]
double weighted_mean(NumericVector x, NumericVector w) {

    int n = x.size();
    double total_w = 0.0;
    double total_xw = 0.0;

    for (int i = 0; i < n; i++) {
        // If the ith element of x or w is NA then return NA
        // if(NumericVector::is_na(x[i]) || NumericVector::is_na(w[i])) {

            // if (na_rm) {
            //     continue;
            // } else {
            // return NumericVector::get_na();
            // }
            // }

        total_w += w[i];
        total_xw += x[i] * w[i];

    }

    return total_xw / total_w;

}

