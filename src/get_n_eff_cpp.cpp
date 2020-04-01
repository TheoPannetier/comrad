#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
std::vector<double> get_n_eff_cpp(std::vector<double> z, double comp_width) {

  std::vector<double> n_eff;

  int i_max = z.size();
  int j_max = z.size();

  for(int i = 0; i < i_max; ++i) {
    double n_eff_i = 0;
    for(int j = 0; j < j_max; ++j) {
      n_eff_i += exp(- pow(z[i] - z[j], 2.0) / (2 * pow(comp_width, 2.0)));
    }
    n_eff.push_back(n_eff_i);
  }

  return n_eff;
}
