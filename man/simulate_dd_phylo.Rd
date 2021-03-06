% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulate_dd_phylo.R
\name{simulate_dd_phylo}
\alias{simulate_dd_phylo}
\title{Simulate a phylogeny with a diversity-dependent birth-death model}
\usage{
simulate_dd_phylo(params, nb_gens, dd_model, stem_or_crown = "stem")
}
\arguments{
\item{params}{a named vector containing the values of the parameters of the
DD model. Names and length must match those expected by \code{dd_model$params_check}}

\item{nb_gens}{integer, the number of generations the simulation should run
for.}

\item{dd_model}{a list with five named elements that together specify the
diversity-dependent model:
\itemize{
\item \code{name} a two-letter code, the name of the model. First letter specifies the
form of the speciation function, second letter the form of the extinction
function: "l" for "linear", "x" for exponential, "c" for constant.
\item \code{speciation_func}, a function specifying the diversity-dependent speciation
rate. Must take arguments \code{params} and \code{N}.
\item \code{extinction_func}, a function specifying the diversity-dependent extinction
rate. Must take arguments \code{params} and \code{N}.
\item \code{constraints} a list of conditions that parameter values must satisfy. Each
element is a function that takes arguments \code{params} and \code{...}, and returns
\code{TRUE} if the constraint is satisfied, \code{FALSE} if it isn't.
\item \code{params_check} a function that controls the format of \code{params}. Returns an
error if the elements of \code{params} are named differently from what is expected
or if the length differs from the expectation.
}

\code{comrad} contains several \code{dd_model} functions, see for example
\code{\link[=dd_model_lc]{dd_model_lc()}}.}

\item{stem_or_crown}{character, either "stem" or "crown", should the
simulation start from 1 species (stem lineage) or 2 species (crown lineages)?}
}
\value{
a \code{phylo} object containign the simulated phylogeny, including
extinct lineages and the stem.
}
\description{
Use a Gillespie algorithm to create a \code{phylo} object from the DD model
specified by \code{dd_model} and \code{params}. The phylogeny is conditioned on the
survival of at least one lineage to the end of the simulation.
}
\author{
Theo Pannetier
}
