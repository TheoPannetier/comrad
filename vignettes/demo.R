## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, include = FALSE---------------------------------------------------
library(comrad)

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

## ----data_viz-----------------------------------------------------------------
comrad_tbl %>% comrad::plot_comm_trait_evolution(xgrain = 10, ygrain = 0.01)

## ----data_viz2----------------------------------------------------------------
comrad_tbl %>% comrad::plot_comm_traits(generation = 1000)

## ----data_viz3----------------------------------------------------------------
comrad_tbl %>% plot_comm_size()

## ----load_sim, echo=FALSE, warning=FALSE--------------------------------------
comrad_tbl <- comrad::read_comrad_tbl("demo_data/comrad_output_example.csv", skip = 20)

## ----data_viz4----------------------------------------------------------------
comrad_tbl %>% comrad::plot_comm_trait_evolution(xgrain = 200, ygrain = 0.05)
comrad_tbl %>% comrad::plot_comm_traits(generation = max(.$t))
comrad_tbl %>% plot_comm_size()

## ----phylo_build--------------------------------------------------------------
phylo <- comrad_tbl %>% comrad::sim_to_phylo()
phylo

## ----phylo_plot---------------------------------------------------------------
phylo %>% ape::plot.phylo()
phylo %>% ape::ltt.plot()

