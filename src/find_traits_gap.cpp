// [[Rcpp::plugins(openmp)]]
// [[Rcpp::plugins(cpp14)]]

#include <Rcpp.h>
using namespace Rcpp;

//' Sort a vector by ascending value
//'
//' @param x a numeric vector
//'
//' @export
//' @name sort_by_ref

// [[Rcpp::export]]
void sort_by_ref(NumericVector x) {
  std::sort(x.begin(), x.end());
}


//' Find gaps in trait values
//'
//' Runs through an ordered vector of trait values, returns the positions of gaps
//' `>= trait_dist_sp` between consecutive values.
//'
//' @param traits a numeric vector, trait values **in ascending order**.
//' @inheritParams default_params_doc
//'
//' @author Théo Pannetier
//' @export
//' @name find_trait_gaps

// [[Rcpp::export]]
std::vector<int> find_trait_gaps(Rcpp::NumericVector traits, const double& trait_dist_sp) {
  Rcpp::NumericVector traits_diff = Rcpp::clone(traits);
  std::adjacent_difference(traits_diff.begin(), traits_diff.end(), traits_diff.begin());
  std::vector<int> gap_positions;
  int i = 1;
  std::for_each(
    traits_diff.begin() + 1, traits_diff.end(),
    [&](const double trait_diff)
    {
      if (trait_diff >= trait_dist_sp) gap_positions.push_back(i);
      i++;
    });
  return gap_positions;
}
