// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// apply_mutations_cpp
NumericVector apply_mutations_cpp(NumericVector traits_comm, double mutation_sd);
RcppExport SEXP _comrad_apply_mutations_cpp(SEXP traits_commSEXP, SEXP mutation_sdSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type traits_comm(traits_commSEXP);
    Rcpp::traits::input_parameter< double >::type mutation_sd(mutation_sdSEXP);
    rcpp_result_gen = Rcpp::wrap(apply_mutations_cpp(traits_comm, mutation_sd));
    return rcpp_result_gen;
END_RCPP
}
// draw_nb_offspring_cpp
std::vector<int> draw_nb_offspring_cpp(std::vector<float> fitness);
RcppExport SEXP _comrad_draw_nb_offspring_cpp(SEXP fitnessSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector<float> >::type fitness(fitnessSEXP);
    rcpp_result_gen = Rcpp::wrap(draw_nb_offspring_cpp(fitness));
    return rcpp_result_gen;
END_RCPP
}
// sort_by_ref
void sort_by_ref(NumericVector x);
RcppExport SEXP _comrad_sort_by_ref(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    sort_by_ref(x);
    return R_NilValue;
END_RCPP
}
// find_trait_gaps
std::vector<int> find_trait_gaps(Rcpp::NumericVector traits, const double& trait_dist_sp);
RcppExport SEXP _comrad_find_trait_gaps(SEXP traitsSEXP, SEXP trait_dist_spSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type traits(traitsSEXP);
    Rcpp::traits::input_parameter< const double& >::type trait_dist_sp(trait_dist_spSEXP);
    rcpp_result_gen = Rcpp::wrap(find_trait_gaps(traits, trait_dist_sp));
    return rcpp_result_gen;
END_RCPP
}
// get_n_eff_cpp
DoubleVector get_n_eff_cpp(const DoubleVector& z, float competition_sd, const std::string& brute_force_opt);
RcppExport SEXP _comrad_get_n_eff_cpp(SEXP zSEXP, SEXP competition_sdSEXP, SEXP brute_force_optSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const DoubleVector& >::type z(zSEXP);
    Rcpp::traits::input_parameter< float >::type competition_sd(competition_sdSEXP);
    Rcpp::traits::input_parameter< const std::string& >::type brute_force_opt(brute_force_optSEXP);
    rcpp_result_gen = Rcpp::wrap(get_n_eff_cpp(z, competition_sd, brute_force_opt));
    return rcpp_result_gen;
END_RCPP
}
// simd_size
int simd_size();
RcppExport SEXP _comrad_simd_size() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(simd_size());
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_comrad_apply_mutations_cpp", (DL_FUNC) &_comrad_apply_mutations_cpp, 2},
    {"_comrad_draw_nb_offspring_cpp", (DL_FUNC) &_comrad_draw_nb_offspring_cpp, 1},
    {"_comrad_sort_by_ref", (DL_FUNC) &_comrad_sort_by_ref, 1},
    {"_comrad_find_trait_gaps", (DL_FUNC) &_comrad_find_trait_gaps, 2},
    {"_comrad_get_n_eff_cpp", (DL_FUNC) &_comrad_get_n_eff_cpp, 3},
    {"_comrad_simd_size", (DL_FUNC) &_comrad_simd_size, 0},
    {NULL, NULL, 0}
};

RcppExport void R_init_comrad(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
