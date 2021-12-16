#include <Rcpp.h>
using namespace Rcpp;

//' Apply random mutations to a community
//'
//' The trait value of each individual in the input community is modified by a
//' mutation sampled in a normal distribution
//' of mean `0` and standard deviation `mutation_sd`.
//'
//' @param traits_comm trait values of individuals in the community
//' @param mutation_sd numeric `>= 0`, the standard deviation of the normal
//' distribution from which mutations are drawn.
//'
//' @author Theo Pannetier
//' @export

// [[Rcpp::export]]
NumericVector apply_mutations(NumericVector traits_comm, double mutation_sd) {
  NumericVector mutations = Rcpp::rnorm(0, mutation_sd);
  std::transform(traits_comm.begin(), traits_comm.end(), mutations.begin(), traits_comm.begin(), std::plus<>{});
  return traits_comm;
}
