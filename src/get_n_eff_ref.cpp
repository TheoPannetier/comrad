// [[Rcpp::plugins(openmp)]]
// [[Rcpp::plugins(cpp14)]]
// [[Rcpp::depends(RcppXsimd)]]

#include <Rcpp.h>
#include <functional>
#include <map>
#include "xsimd/xsimd.hpp"

namespace {

  using simd_type = xsimd::simd_type<float>;
  using simd_vector = std::vector<float, xsimd::aligned_allocator<float, XSIMD_DEFAULT_ALIGNMENT>>;

  // original
  simd_vector get_n_eff_ref(const simd_vector& z_seq, const simd_vector& z_pop, float denom)
  {
    simd_vector n_eff(z_seq.size(), 0.f);
    int z_seq_length = static_cast<int>(z_seq.size());
    int z_pop_length = static_cast<int>(z_pop.size());
    for (int i = 0; i < z_seq_length; ++i) {
      float z_i = z_seq[i];
      for (int j = 0; j < z_pop_length; ++j) {
        float comp_coeff = expf(-((z_i - z_pop[j]) * (z_i - z_pop[j])) * denom);
        n_eff[i] += comp_coeff;
      }
    }
    return n_eff;
  }


  struct reduction_op
  {
    const float zi_;
    const float denom_;

    reduction_op(float zi, float denom) noexcept
      : zi_(zi), denom_(denom)
    {}

    float operator()(float sum, float zj) const noexcept
    {
      return sum + expf(-((zi_ - zj) * (zi_ - zj)) * denom_);
    }
  };

  simd_vector get_n_eff_ref_algo(const simd_vector& z_seq, const simd_vector& z_pop, float denom)
  {
    simd_vector n_eff(z_seq.size(), 0.f);
    for (auto i = 0; i < z_seq.size(); ++i) {
      n_eff[i] = std::accumulate(z_pop.begin(), z_pop.end(), 0.0f, reduction_op(z_seq[i], denom));
    }
    return n_eff;
  }

  simd_vector get_n_eff_ref_omp(const simd_vector& z_seq, const simd_vector& z_pop, float denom)
  {
    simd_vector n_eff(z_seq.size(), 0.f);
    const int N = static_cast<int>(z_seq.size());
#   pragma omp parallel for
    for (int i = 0; i < N; ++i) {
      n_eff[i] = std::accumulate(z_pop.begin(), z_pop.end(), 0.0f, reduction_op(z_seq[i], denom));
    }
    return n_eff;
  }

  const std::map<std::string, std::function<simd_vector(const simd_vector&, const simd_vector&, float)>> brute_force_map_ref = {
    {"none", &get_n_eff_ref},
    {"algo", &get_n_eff_ref_algo},
    {"omp", &get_n_eff_ref_omp}
  };

}

using namespace Rcpp;

//' Compute the effective population size
//'
//' Computes \code{n_eff}, the effective population size experienced by an
//' individual.
//' @param z numeric vector, the trait values of all individuals in the
//' community.
//' @param competition_sd numeric `>= 0`. Width of the competition kernel.
//' @param brute_force_opt a string specifying which brute force option to use
//' to speed up the calculation of competition coefficients. Defaults to "none".
//' Other options are omp", for multithreading with OpenMP, "simd" for single
//' instruction, multiple data (SIMD) via the C++ library
//' [`xsimd`](https://github.com/xtensor-stack/xsimd); and "simd_omp" for both.
//' @details `n_eff` sums the competitive effects an individual receives from
//' every individual in the community, including the individual itself. It is
//' called effective population size because it is the size of the population
//' that is relevant for competition.
//' @name get_n_eff
//' @author Hanno Hildenbrandt
//' @export
// [[Rcpp::export]]
DoubleVector get_n_eff_ref(const DoubleVector& z_ref, const DoubleVector& z_pop, float competition_sd, const std::string& brute_force_opt = "none")
{
  auto it = brute_force_map_ref.find(brute_force_opt);
  if (it == brute_force_map_ref.end()) {
    throw std::runtime_error("invalid argument 'brute_force_opt'");
  }
  // Convert input in float
  simd_vector sz_ref(z_ref.size());
  std::transform(z_ref.begin(), z_ref.end(), sz_ref.begin(), [](double x) {
    return static_cast<float>(x);
  });
  simd_vector sz_pop(z_ref.size());
  std::transform(z_pop.begin(), z_pop.end(), sz_pop.begin(), [](double x) {
    return static_cast<float>(x);
  });

  const float denom = 1.0f / (2.f * (competition_sd * competition_sd));
  // Compute n_eff with selected function
  auto n_eff = it->second(sz_ref, sz_pop, denom);

  // Convert output in double
  auto res = DoubleVector(n_eff.size());
  std::transform(n_eff.cbegin(), n_eff.cend(), res.begin(), [](float x) {
    return static_cast<double>(x);
  });
  return res;
}
