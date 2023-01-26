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
//' @name apply_mutations

#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
void apply_mutations(NumericVector traits_comm, double mutation_sd) {
  const NumericVector mutations = Rcpp::rnorm(traits_comm.size(), 0, mutation_sd);
  std::transform(traits_comm.begin(), traits_comm.end(), mutations.begin(), traits_comm.begin(), std::plus<>{});
}
