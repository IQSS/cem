\name{L1.profile}
\alias{L1.profile}
\title{Calculates L1 distance for different coarsenings}
\description{
Calculates L1 distance for different coarsenings
}
\usage{
L1.profile(group, data, drop = NULL, min.cut = 2, max.cut = 12, 
weights, plot = TRUE, add = FALSE, col = "red", 
lty = 1, M=100, useCP=NULL, grouping=NULL, progress=TRUE)
}
\arguments{
  \item{group}{the group variable}
  \item{data}{the data}
  \item{drop}{a vector of variable names in the data frame to ignore}
  \item{min.cut}{minimum number of cut points per variable}
  \item{max.cut}{maximum number of cut points per variable}
  \item{weights}{weights}
  \item{useCP}{a list which elements is a list of cutpoints, usually passed from
   a previous instance of \code{L1.profile}. If not \code{NULL} these coarsenings
   are used instead of generating them randomly.}
  \item{M}{number of random coarsenings} 
  \item{plot}{plot a graph?}
  \item{add}{add graph to an existing plot? Makes sense only if \code{plot} is \code{TRUE}}
  \item{col}{draw in specified color}
  \item{lty}{draw using specified lty}
  \item{grouping}{named list, each element of which is a list of
  groupings for a single categorical variable. See Details.}
  \item{progress}{if \code{TRUE}, feedback on progress is given. See Details.}
}
\details{
  The L1 measure depends on the coarsening chosen to calculate it, and
  as such the comparison of different matching solutions may differ
  depending on this somewhat arbitrary choice.  This function computes
  L1 for a random  range of possible
  coarsenings.  The point of this function is that if
  one matching solution has a lower L1 than another, then it dominates
  without regard to the choice of coarsening.  A graphic display conveys
  the results succinctly.  (The logic is similar to
  that for ROC curves used for classification algorithms.)
  (This degree of coarsening should remain fixed for different CEM
  runs.)

For each variables the function generates a random number of cutpoints 
between \code{min.cut} and \code{max.cut} in which to cut  the support of each
variable. This procedure is repeated \code{M} times. The out is sorted in
increasing values of L1 just for graphical representation.
 
Non numeric variables are  grouped randomly unless they appear specified in
 the \code{grouping} argument.

A \code{plot} method exists for the returned object.
}
\value{
  An invisible object of class \code{L1profile} which contains a named list of coarsenings and 
  values of the L1 measure for each coarsening.
}
\references{Stefano Iacus, Gary King, Giuseppe Porro, ``Matching for
  Casual Inference Without Balance Checking: Coarsened Exact Matching,''
  http://gking.harvard.edu/files/abs/cem-abs.shtml}
\author{Stefano Iacus, Gary King, and Giuseppe Porro}
\examples{
\dontrun{
data(LL)
for(i in c(4:6,10:12))
 LL[[i]] <- factor(LL[[i]])

imb0 <- L1.profile(LL$treated,LL, drop=c("treated","re78"))

if(require(MatchIt)){
 m2 <- matchit(treated ~ black + hispanic + married + nodegree + 
  u74 + u75 + education + age + re74 + re75, data=LL, 
  distance="logit")

 m3 <- matchit(treated ~ black + hispanic + married + nodegree + 
  u74 + u75 + education + age + re74 + re75, data=LL, 
  distance="mahalanobis")
 
 L1.profile(LL$treated,LL, drop=c("treated","re78"), 
  weights=m2$w, add=TRUE, col="green", lty=2, useCP=imb0$CP)

 L1.profile(LL$treated,LL, drop=c("treated","re78"), 
  weights=m3$w, add=TRUE, col="orange", lty=3, useCP=imb0$CP)
}

m1 <- cem("treated", LL, drop="re78")

L1.profile(LL$treated,LL, drop=c("treated","re78"), 
 weights=m1$w>0, add=TRUE, col="blue", lty=4, useCP=imb0$CP)

legend(5, 0.9, legend=c("raw data", "pscore", "mahalanobis", "cem"), 
 lty=1:4, col=c("red", "green", "orange", "blue"))
}
}
\keyword{ datagen }
