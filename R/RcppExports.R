# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#' Draw a number of offspring given a fitness value
#'
#' The number of offspring is drawn in a Poisson distribution in
#' `rpois(lambda = fitness)`. Vectorized.
#'
#' @param fitness a vector of positive floats, the fitness value(s).
#'
#' @seealso get_fitness
#' @author Theo Pannetier
#' @export
#' @name draw_nb_offspring
#'
NULL

draw_nb_offspring <- function(fitness) {
    .Call('_comrad_draw_nb_offspring', PACKAGE = 'comrad', fitness)
}

#' Sort a vector by ascending value
#'
#' @param x a numeric vector
#'
#' @export
#' @name sort_by_ref
NULL

#' Find gaps in trait values
#'
#' Runs through an ordered vector of trait values, returns the positions of gaps
#' `>= trait_dist_sp` between consecutive values.
#'
#' @param traits a numeric vector, trait values **in ascending order**.
#' @param trait_dist_sp numeric, the minimal trait distance between two
#' clusters of individuals triggering speciation.
#'
#' @author Théo Pannetier
#' @export
#' @name find_trait_gaps
NULL

sort_by_ref <- function(x) {
    invisible(.Call('_comrad_sort_by_ref', PACKAGE = 'comrad', x))
}

find_trait_gaps <- function(traits, trait_dist_sp) {
    .Call('_comrad_find_trait_gaps', PACKAGE = 'comrad', traits, trait_dist_sp)
}

#' Compute the effective population size
#'
#' Computes \code{n_eff}, the effective population size experienced by an
#' individual.
#' @param z numeric vector, the trait values of all individuals in the
#' community.
#' @param competition_sd numeric `>= 0`. Width of the competition kernel.
#' @details `n_eff` sums the competitive effects an individual receives from
#' every individual in the community, including the individual itself. It is
#' called effective population size because it is the size of the population
#' that is relevant for competition.
#' @name get_n_eff
#' @author Thijs Janzen, Théo Pannetier
#' @export
NULL

get_n_eff <- function(z, competition_sd) {
    .Call('_comrad_get_n_eff', PACKAGE = 'comrad', z, competition_sd)
}

