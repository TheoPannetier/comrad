% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dd_model_lx.R
\name{dd_model_lx}
\alias{dd_model_lx}
\title{Diversity-dependent model with linear (l) dependence on speciation and
exponential (x) dependence on extinction}
\usage{
dd_model_lx()
}
\description{
A list specifying a DD model with linear diversity-dependence on the
speciation rate and exponential diversity-dependence on the extinction rate;
to be fed as argument \code{dd_model} to \code{\link[=fit_dd_model_with_fossil]{fit_dd_model_with_fossil()}}.
}
\details{
\deqn{\lambda(N) = \lambda_{0} - (1 - \alpha)(\lambda_{0} - \mu_{0}) \frac{N}{K}}
\deqn{\mu(N) = \mu_{0}((1 - \alpha) + \alpha \frac{\lambda_{0}}{\mu_{0}})^{\frac{N}{K}}}
}
\author{
Theo Pannetier
}
