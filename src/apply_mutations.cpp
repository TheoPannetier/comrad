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

#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector apply_mutations_cpp(NumericVector traits_comm, double mutation_sd) {
  for (int i = 0; i < traits_comm.size(); ++i)
  {
    traits_comm[i] += R::rnorm(0, mutation_sd);
  }
  return traits_comm;
}

// [[Rcpp::export]]
NumericVector apply_mutations_cpp2(NumericVector traits_comm, double mutation_sd) {
  NumericVector mutations = Rcpp::rnorm(traits_comm.size(), 0, mutation_sd);
  assert(traits_comm().size == mutations.size());
  for (int i = 0; i < traits_comm.size(); ++i)
  {
    traits_comm[i] += mutations[i];
  }
  return traits_comm;
}

// [[Rcpp::export]]
NumericVector apply_mutations_cpp3(NumericVector traits_comm, double mutation_sd) {
  for (auto trait_ind : traits_comm)
  {
    trait_ind += R::rnorm(0, mutation_sd);
  }
  return traits_comm;
}

// [[Rcpp::export]]
void apply_mutations_cpp4(NumericVector traits_comm, double mutation_sd) {
  const NumericVector mutations = Rcpp::rnorm(traits_comm.size(), 0, mutation_sd);
  std::transform(traits_comm.begin(), traits_comm.end(), mutations.begin(), traits_comm.begin(), std::plus<>{});
  //return traits_comm;
}
