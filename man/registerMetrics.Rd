% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metrics.R
\name{registerMetrics}
\alias{registerMetrics}
\alias{unregisterMetric}
\alias{getMetrics}
\title{Register, unregister, or access registered MCMC metric functions
for use by \code{compareMCMCs} or \code{addMetrics}}
\usage{
registerMetrics(metrics)

unregisterMetric(name)

getMetrics()
}
\arguments{
\item{metrics}{A named list of new metric functions to register}

\item{name}{Character name of a metric function to unregister}
}
\value{
\code{registerMetrics} and \code{getMetrics} return the environment of
registered metrics.

\code{unregisterMetric} returns the result (which should be NULL) of a
call to \code{rm} that attempts to remove a metric.
}
\description{
Register, unregister, or access registered MCMC metric functions
for use by \code{compareMCMCs} or \code{addMetrics}
}
\details{
These functions are called for their "side effects" of
modifying the list metric functions for MCMC results that will
be recognized by name from the \code{compareMCMCs} or \code{addMetrics}
functions.  Those functions take a \code{metrics} argument that can
be a character vector or a list.  Names in the character vector
will be looked up from the registered metric functions.

\code{registerMetrics} takes a named list and adds its elements to the list
of recognized metrics with the corresponding names.

\code{unregisterMetric} removes one metric from the list at a time.

\code{getMetrics} returns the list of registered metrics.
}
