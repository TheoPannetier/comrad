#include <Rcpp.h>
#include <random>
using namespace Rcpp;

//' Draw a number of offspring given a fitness value
//'
//' The number of offspring is drawn in a Poisson distribution in
//' `rpois(lambda = fitness)`. Vectorized.
//'
//' @param fitness a vector of positive floats, the fitness value(s).
//' @param seed. Integer, the seed passed for random number generation.
//'
//' @seealso get_fitness
//' @author Theo Pannetier
//' @export
//' @name draw_nb_offspring_cpp

// [[Rcpp::export]]
std::vector<int> draw_nb_offspring_cpp(std::vector<float> fitness, int seed) {
  int nb_inds = fitness.size();

  std::vector<int> nb_offspring(nb_inds);

  std::default_random_engine generator(seed);

  for (int i = 0; i < nb_inds; ++i) {
    std::poisson_distribution<int> dist(fitness[i]);
    nb_offspring[i] = dist(generator);
  }
  return nb_offspring;
}
