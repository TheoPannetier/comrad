library(comrad)

set_n_eff_cpp_algo("simd_omp")
system.time(run_simulation(path_to_output=NULL, nb_gens=1000, competition_sd = 0.01))
