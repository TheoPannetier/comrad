#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
std::vector<double> get_n_eff_cpp(NumericVector z, double comp_width) {

  std::vector<double> n_eff(z.size());

  for(int i = 0; i < z.size(); ++i) {
    for(int j = 0; j < z.size(); ++j) {
      n_eff[i] += exp(- pow(z[i] - z[j], 2.0) / (2 * pow(comp_width, 2.0)));
    }
  }

  return n_eff;
}
