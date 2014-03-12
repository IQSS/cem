\name{imbspace}
\alias{imbspace}
\title{Diagnostic tool for CEM}
\description{
Diagnostic tools for CEM
}
\usage{
imbspace(obj, data, depth = 1, L1.breaks = NULL, 
plot = TRUE, fixed = NULL, minimal = 1, maximal = 6,
M=250, raw.profile=NULL) 
}
\arguments{
  \item{obj}{an object of class \code{cem.match}}
  \item{data}{the original data.}
  \item{depth}{if 1, relaxes up to dropping one var, if 2 relaxes (up to
   dropping) two vars, etc.}
  \item{L1.breaks}{list of cutpoints for the calculation of the L1 measure.}
  \item{plot}{plot the space of solutions?}  
  \item{fixed}{vector of variable names which will not be relaxed.}  
  \item{minimal}{the minimal number of intervals acceptable after
  relaxation. Should be a nameed list of positive integers or if a number, this is
  applied to all variables.}
  \item{maximal}{the maximal number of intervals acceptable after
  relaxation. Should be a nameed list of positive integers or if a number, this is
  applied to all variables.}
  \item{M}{number of possible random coarsening for the L1 measure}
  \item{raw.profile}{and object of class \code{L1profile}. If passed, the \code{L1.breaks} are
  ignored.}
}

\details{
This is a diagnostic tool to help the user in the search of different choices
of coarsenings. The algorithm tries all possible combination of coarsenings into
intervals between \code{minimal} and \code{maximal} one variable at time, for
pairs, triplets, etc depending on the value of \code{depth}.


Calling directly \code{plot} on the output of \code{imbspace} has the same
effect of calling directly \code{\link{imbspace.plot}}.

} 
\value{
  \item{val}{an invisible object of class \code{imbalance.space}.}
}
\references{Stefano Iacus, Gary King, Giuseppe Porro, ``Matching for
  Casual Inference Without Balance Checking: Coarsened Exact Matching,''
  http://gking.harvard.edu/files/abs/cem-abs.shtml}
\author{Stefano Iacus, Gary King, and Giuseppe Porro}
\seealso{ \code{\link{imbspace.plot}} }
\keyword{datagen}

