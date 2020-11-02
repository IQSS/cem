\name{cemspace}
\alias{cemspace}
\title{Exploration tool for CEM}
\description{
Exploration tool for CEM
}
\usage{
cemspace(treatment=NULL, data = NULL, R=100, grouping = NULL, drop=NULL,
L1.breaks = NULL, L1.grouping=NULL, plot = TRUE, fixed = NULL, 
minimal = 1, maximal = 5, M=250, raw.profile=NULL, keep.weights=FALSE,
verbose=1) 
}
\arguments{
  \item{treatment}{character, name of the treatment variable.}
  \item{data}{a data.frame.}
  \item{verbose}{integer, controls output.}
  \item{R}{number of possible random coarsening for the CEM.}
  \item{grouping}{named list, each element of which is a list of
  groupings for a single categorical variable. For more details see \code{\link{cem}}.}
  \item{drop}{a vector of variable names in the data frame to ignore
    during matching}
  \item{L1.breaks}{list of cutpoints for the calculation of the L1 measure.}
  \item{L1.grouping}{as \code{grouping} but only needed in the calculation
   of the L1 measure not in matching.}
 \item{plot}{plot the space of solutions?}  
  \item{fixed}{vector of variable names which will not be relaxed.}  
  \item{minimal}{the minimal number of intervals acceptable after
  relaxation. Should be a nameed list of positive integers or if a number, this is
  applied to all variables.}
  \item{maximal}{the maximal number of intervals acceptable after
  relaxation. Should be a nameed list of positive integers or if a number, this is
  applied to all variables.}
  \item{M}{number of possible random coarsening for the L1 measure}
  \item{raw.profile}{an object of class \code{L1profile}. If passed, the \code{L1.breaks} are
  ignored and set to median cutpoints of L1 profile.}
  \item{keep.weights}{if \code{TRUE}, for each matching solutions the CEM-weights are stored.}
}

\details{
This is a tool to help the user to explore different cem solutions by choosing
random coarsenings. The algorithm tries \code{R} random choiches
of coarsenings into
intervals between \code{minimal} and \code{maximal} for numerical, integer or
ordered factors. It drops or include dichotomous or boolean variables. 


Calling directly \code{plot} on the output of \code{cemspace} has the same
effect of calling directly \code{\link{imbspace.plot}}.

If you want to relax a given cem solution, use the function 
\code{\link{imbspace}} instead.
} 
\value{
  \item{val}{an invisible object of class \code{imbalance.space}.}
}
\references{
Iacus, King, Porro (2011) \url{https://doi.org/10.1198/jasa.2011.tm09599}

Iacus, King, Porro (2012) \url{https://doi.org/10.1093/pan/mpr013}
	
Iacus, King, Porro (2019) \url{ https://doi.org/10.1017/pan.2018.29}
}
\author{Stefano Iacus, Gary King, and Giuseppe Porro}
\examples{
\donttest{
data(LL)
set.seed(123)
tmp <- cemspace("treated", LL, drop="re78", M=50)
}
}
\seealso{ \code{\link{imbspace.plot}}, \code{\link{cemspace}}}
\keyword{datagen}

