\name{shift.cem}
\alias{shift.cem}
\title{Diagnostic tool for CEM}
\description{
Diagnostic tools for CEM. Applies leftward and rightward shifts of the cutpoints.
}
\usage{
shift.cem(obj, data, shifts=NULL, verbose=0, plot=TRUE)
}
\arguments{
  \item{obj}{and object of class cem}
  \item{data}{the original data}
  \item{shifts}{a vector of proportions of shifts}
  \item{verbose}{controls the level of verbosity}
  \item{plot}{whether to plot a graphic representation of the search} }

\details{ For each variable, shift all the cutpoints left and right by
 \code{shifts} times the smallest epsilon of the
  coarsening. Shifting to the right produces a new cell on the left;
  shift to the left, adds a new cell to the coarsening on the right.
  Only positive proportions should be used; the algorithm
  will produce shifting on the left or on the right.  The best shifting
  of the original cem match is produced as output, where best is defined
  in terms of the maximal total number of matched units \code{mT+mC} (see below).  

By default, the function returns minimal information about the execution
of the algorithm. By setting a value greater than 0 in option \code{verbose}
more feedback on the process is returned.


Option \code{plot} = \code{TRUE} plots the number of treated units 
matched \code{mT}, the number of control units 
matched \code{mC}, and the sum \code{mT+mC}, as a function of the shifts.
}
\value{
  \item{tab}{an invisible object containing a new cem object}
}
\references{Stefano Iacus, Gary King, Giuseppe Porro, ``Matching for
  Casual Inference Without Balance Checking: Coarsened Exact Matching,''
  http://gking.harvard.edu/files/abs/cem-abs.shtml}
\author{Stefano Iacus, Gary King, and Giuseppe Porro}
\seealso{ \code{\link{cem}} }
\examples{
\dontrun{
data(LL)

m74 <- max(LL$re74, na.rm=TRUE)
s74 <- seq(0,m74,by=sd(LL$re74))
l74 <- length(s74)
if(max(s74) < m74) s74 <- c(s74, m74)

m75 <- max(LL$re75, na.rm=TRUE)
s75 <- seq(0,m75,by=sd(LL$re75))
l75 <- length(s75)
if(max(s75) < m75) s75 <- c(s75, m75)

mybr = list(re74=s74, 
 re75 = s75,
 age = hist(LL$age,plot=FALSE)$breaks,
 education = hist(LL$education,plot=FALSE)$breaks)

mat <- cem(treatment="treated",data=LL, drop="re78",cut=mybr)
mat

shift.cem(mat, data=LL, shifts=seq(0.01, 0.5, length=10), verb=1)
}
}
\keyword{datagen}
