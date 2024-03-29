% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dd_model_xl.R
\name{dd_model_xl}
\alias{dd_model_xl}
\title{Diversity-dependent model with exponential (x) dependence on speciation and
linear (l) dependence on extinction}
\usage{
dd_model_xl()
}
\description{
A list specifying a DD model with exponential diversity-dependence on the
speciation rate and linear diversity-dependence extinction rate;
to be fed as argument \code{dd_model} to \code{\link[=fit_dd_model_with_fossil]{fit_dd_model_with_fossil()}}.
}
\details{
\deqn{\lambda(N) = \lambda_{0}(\alpha + (1 - \alpha) \frac{\mu_{0}}{\lambda_{0}})^{\frac{N}{K}}}
\deqn{\mu(N) = \mu_{0} + \alpha(\lambda_{0} - \mu_{0}) \frac{N}{K}}
}
\author{
Theo Pannetier
}
