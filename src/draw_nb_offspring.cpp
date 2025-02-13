// [[Rcpp::plugins(openmp)]]
// [[Rcpp::plugins(cpp14)]]

#include <Rcpp.h>
using namespace Rcpp;

//' Draw a number of offspring given a fitness value
//'
//' The number of offspring is drawn in a Poisson distribution in
//' `rpois(lambda = fitness)`. Vectorized.
//'
//' @param fitness a vector of positive floats, the fitness value(s).
//'
//' @seealso get_fitness
//' @author Theo Pannetier
//' @export
//' @name draw_nb_offspring
//'

// [[Rcpp::export]]
std::vector<int> draw_nb_offspring(std::vector<float> fitness) {

  int nb_inds = fitness.size();
  std::vector<int> nb_offspring(nb_inds);
  std::transform(fitness.begin(), fitness.end(), nb_offspring.begin(),
                 [](float fit) {
                   return R::rpois(fit);
                 });
  return nb_offspring;
}
