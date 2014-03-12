\name{search.match}
\alias{search.match}
\title{Heuristic search of match solutions}
\description{
Heuristic search of match solutions
}
\usage{
search.match(data, treatment, vars, depth=3, min.vars =1, group=1, useCP, ...) 
}
\arguments{
  \item{data}{the original data.}
  \item{treatment}{name of the treatment variable.}
  \item{depth}{level of interaction and squares. See Details.}
  \item{vars}{vector of variables' names to match on.}
  \item{min.vars}{minimum number of variables to consider in the model.}
  \item{group}{the indentifier of the treated group, usually 1 or the level of the fact variable \code{treatment}.}
  \item{useCP}{the cutpoints for the calculation of the L1 measure}
  \item{...}{passed to \code{matchit}}
}

\details{
This is a tool to help the user in the search of different choices models for matching.
For example, for the search of different propensity score models.
The tool tries all submodels of \code{k} variables starting from one covariate up to the full model. Then adds
interactions to the full model trying all pairs, triplets etc according to the parameter \code{depth}.
Then, for continuous variables only, adds squared terms to the full model.

This tool is useful in combination with \code{\link{imbspace.plot}}.
} 
\value{
  \item{val}{an invisible object of class \code{list}.}
}
\references{Stefano Iacus, Gary King, Giuseppe Porro, ``Matching for
  Casual Inference Without Balance Checking: Coarsened Exact Matching,''
  http://gking.harvard.edu/files/abs/cem-abs.shtml}
\author{Stefano Iacus, Gary King, and Giuseppe Porro}
\seealso{ \code{\link{imbspace.plot}} }
\keyword{datagen}

