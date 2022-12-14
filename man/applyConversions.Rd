% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/conversions.R
\name{applyConversions}
\alias{applyConversions}
\title{Apply a set of parameter conversions to MCMC output}
\usage{
applyConversions(samples, conversions)
}
\arguments{
\item{samples}{One of: an \code{\link{MCMCresult}} object; a named
list of \code{MCMCresult} objects (such as returned by
\code{\link{compareMCMCs}}); a matrix of MCMC samples (such as
the \code{samples} element of an \code{MCMCresult} object); or a named
list of such matrices.  In the first two cases, conversions will
be done in place (as a "side effect" modifying the arguments)
because \code{\link{MCMCresult}} objects are R6 objects and are thus
passed by reference.}

\item{conversions}{One of: a list of conversion specifications (see
below); a named list of conversion specifications, with names
matching those of a list provided for \code{samples}.}
}
\value{
An object of the same type as \code{samples} after application
of conversions.
}
\description{
Create transformed parameters from original parameters in MCMC output
}
\details{
A conversion specification is a named list. For each
element:
\itemize{
\item its name will be the name of a new column appended to a \code{samples} matrix.
\item its value should be a character string that can be parsed as code
to calculate elements of the new column.  It can use existing
column names in \code{samples}.  Calculations will be done row-wise.
Column names are often something like "beta[2]".  To have this
used as a name, enclose it in backticks, e.g. "\code{`beta[2]`}".
For example, an entry could be \verb{log_beta2 = "log(`beta\[2\]`)"}.
A list value of \code{NULL} will remove the named column.
}

The conversion specification list will be processed in order.
This allows creating new columns and removing old ones in a
sensible order.

If both \code{conversions} and \code{samples} are named lists, they will be
matched: the \code{conversions} element (itself a list of conversion
specifications) used on a \code{samples} element will have the same
name.  If there is no \code{conversions} element for a given \code{samples}
element, that \code{samples} element will be included in the returned
list without any conversions.
}
