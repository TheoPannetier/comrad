## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, include = FALSE---------------------------------------------------
library(comrad)
par(mai = rep(0, 4))

## ----lib_comrad, eval=FALSE---------------------------------------------------
#  remotes::install_github("TheoPannetier/comrad")

## ----run_sim------------------------------------------------------------------
temp_path_to_output <- paste0(tempfile("comrad_temp_output"), ".csv")

comrad_tbl <- comrad::run_simulation(
  path_to_output = temp_path_to_output,
  nb_gens = 1000
)

## ----read_comm----------------------------------------------------------------
comrad_tbl <- comrad::read_comrad_tbl(path_to_file = temp_path_to_output)
comrad_tbl

## -----------------------------------------------------------------------------
comrad_tbl <- comrad::read_comrad_tbl("demo_data/comrad_output_example.csv", skip = 20)

## ----data_viz-----------------------------------------------------------------
comrad_tbl %>% comrad::plot_comm_trait_evolution(xgrain = 200, ygrain = 0.02)

## ----data_viz2----------------------------------------------------------------
comrad_tbl %>% comrad::plot_comm_traits(generation = 10000)

## ----data_viz3----------------------------------------------------------------
comrad_tbl %>% plot_comm_size()

## ----phylo_build--------------------------------------------------------------
phylo <- comrad_tbl %>% comrad::sim_to_phylo()
phylo

## ----phylo_plot---------------------------------------------------------------
phylo %>% ape::plot.phylo()
phylo %>% ape::ltt.plot()

## ---- set_dd_model------------------------------------------------------------
dd_model <- comrad::dd_model_lc() # linear speciation, constant extinction
dd_model

## ----all_dd_models------------------------------------------------------------
comrad::dd_model_names()

## ----comrad_to_ddd------------------------------------------------------------
comrad::dd_model_comrad_to_ddd("lc")
comrad::dd_model_comrad_to_ddd("ll")
comrad::dd_model_comrad_to_ddd("px")

## ----waiting_times------------------------------------------------------------
wt_tbl <- comrad::waiting_times(phylo)
knitr::kable(wt_tbl)

## ----draw_init_params---------------------------------------------------------
phylos <- list()
phylos[[1]] <- phylo # the function below assumes a list of trees
init_params <- comrad::draw_init_params_dd_ml(
  phylos = phylos,
  nb_sets = 1,
  dd_model = dd_model
)[[1]]
init_params

## ----fit_dd_with_fossil-------------------------------------------------------
ml_tbl <- comrad::fit_dd_model_with_fossil(
  waiting_times_tbl = wt_tbl,
  dd_model = dd_model,
  init_params = init_params
)
knitr::kable(ml_tbl)

## ----extract_branching_times--------------------------------------------------
# Drop extinct lineages
phylo_extant <- phylo %>%
  ape::drop.fossil()
phylo_extant %>% ape::plot.phylo()
# Extract branching times
branching_times <- phylo_extant %>% 
  ape::branching.times()

## ---- fit_dd_model_without_fossil, eval=FALSE---------------------------------
#  # not run; optimisation may take a very long time
#  # and bugs may occur depending on the current state of DDD
#  ml_tbl <- comrad::fit_dd_model_without_fossil(
#    branching_times =  branching_times,
#    dd_model = dd_model,
#    init_params = init_params
#  )

