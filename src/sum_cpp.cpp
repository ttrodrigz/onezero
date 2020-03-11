#include <Rcpp.h>
using namespace Rcpp;

//' Sum.
//'
//' @param x A numeric vector.
//'
//' @export
// [[Rcpp::export]]
double sum_cpp(NumericVector x) {

    int n = x.size();
    double total_x = 0.0;

    for (int i = 0; i < n; i++) {
        // If the ith element of x or w is NA then return NA
        // if(NumericVector::is_na(x[i]) || NumericVector::is_na(w[i])) {

            // if (na_rm) {
            //     continue;
            // } else {
            // return NumericVector::get_na();
            // }
            // }

        total_x += x[i];

    }

    return total_x;

}

