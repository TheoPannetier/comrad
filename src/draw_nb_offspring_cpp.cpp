#include <Rcpp.h>
#include <random>
using namespace Rcpp;

//' Draw a number of offspring given a fitness value
//'
//' The number of offspring is drawn in a Poisson distribution in
//' `rpois(lambda = fitness)`. Vectorized.
//'
//' @param fitness numeric `>= 0`, a vector of fitness values.
//'
//' @seealso get_fitness
//' @author Theo Pannetier
//' @export
//' @name draw_nb_offspring_cpp

// [[Rcpp::export]]
std::vector<int> draw_nb_offspring_cpp(std::vector<float> fitness) {
  int nb_inds = fitness.size();

  std::vector<int> nb_offspring(nb_inds);

  std::default_random_engine generator;

  for (int i = 0; i < nb_inds; ++i) {
    std::poisson_distribution<int> dist(fitness[i]);
    nb_offspring[i] = dist(generator);
  }
  return nb_offspring;
}
