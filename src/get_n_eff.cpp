#include <Rcpp.h>

using namespace Rcpp;

//' Compute the effective population size
//'
//' Computes \code{n_eff}, the effective population size experienced by an
//' individual.
//' @param z numeric vector, the trait values of all individuals in the
//' community.
//' @param competition_sd numeric `>= 0`. Width of the competition kernel.
//' @details `n_eff` sums the competitive effects an individual receives from
//' every individual in the community, including the individual itself. It is
//' called effective population size because it is the size of the population
//' that is relevant for competition.
//' @name get_n_eff
//' @author Thijs Janzen, Théo Pannetier
//' @export

// [[Rcpp::export]]
std::vector<float> get_n_eff(const std::vector<float>& z, float competition_sd) {

  std::vector<float> n_eff(z.size(), 0.f);

  // faster to compute these out of the for loops
  int z_length =  z.size();
  float denom = 1.0f / (2.f * (competition_sd * competition_sd));

  for(int i = 0; i < z_length; ++i) {
    float z_i = z[i];
    for(int j = i; j < z_length; ++j) {
      float comp_coeff = expf(-((z_i - z[j]) * (z_i - z[j]) ) * denom);
      n_eff[i] += comp_coeff;
      if(i != j) n_eff[j] += comp_coeff;
    }
  }

  return n_eff;
}


//' Compute the effective population size at a range of values
//'
//' Given the trait values of a population \code{z_pop}, calculate the effective
//' population sizes \code{n_eff} experienced at a set of positions \code{z_seq}
//' on the trait axis.
//' @param z_seq numeric vector, the set of trait values for which we want to know
//' \code{n_eff}, given \code{z_pop}
//' @param z_pop numeric vector, the trait values of all individuals in the population.
//' @param competition_sd numeric `>= 0`. Width of the competition kernel.
//' @details `n_eff` sums the competitive effects an individual receives from
//' every individual in the community, including the individual itself. It is
//' called effective population size because it is the size of the population
//' that is relevant for competition.
//' @name get_n_eff_seq
//' @author Théo Pannetier
//' @export

// [[Rcpp::export]]
 std::vector<float> get_n_eff_seq(const std::vector<float>& z_seq, const std::vector<float>& z_pop, float competition_sd) {

   float denom = 1.0f / (2.f * (competition_sd * competition_sd));

   std::vector<float> n_eff(z_seq.size(), 0.f);

   int range_size =  z_seq.size();
   for(int i = 0; i < range_size; ++i) {
     n_eff[i] = 0;
     for(auto& z_ind : z_pop) {
       n_eff[i] += expf(-((z_seq[i] - z_ind) * (z_seq[i] - z_ind) ) * denom);
     }
   }

   return n_eff;
 }

