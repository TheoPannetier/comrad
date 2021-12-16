#include <Rcpp.h>
using namespace Rcpp;

//' Apply random mutations to a community
//'
//' The trait value of each individual in the input community is modified, with
//' probability `prob_mutation`, by a mutation sampled in a normal distribution
//' of mean `0` and standard deviation `mutation_sd`.
//'
//' @inheritParams default_params_doc
//'
//' @author Theo Pannetier
//' @export

// [[Rcpp::export]]
NumericVector apply_mutations_cpp(NumericVector traits_comm, double mutation_sd) {
  for (int i = 0; i < traits_comm.size(); ++i)
  {
    traits_comm[i] += R::rnorm(0, mutation_sd);
  }
  return traits_comm;
}
