\name{imbspace.plot}
\alias{imbspace.plot}
\title{Plot of imbalance space diagnostic tool for CEM}
\description{
Plot of imbalance space diagnostic tool for CEM
}
\usage{
imbspace.plot(obj, group="1", data, explore=TRUE, verbose=1)
}
\arguments{
  \item{obj}{an object of class \code{imbalance.space}}
  \item{group}{character string denoting group id. Defaults to \code{"1"}.}
  \item{data}{data for running additional matching solutions.}
  \item{explore}{if \code{TRUE} the user can interact and find new solutions.}
  \item{verbose}{integer, controls output.}
}

\details{
For an interactive device a two panels plot is given. On the left panel
the user can select a CEM solution and the number of cutpoints
used in that matching solution is plotted as a parallel plot on the right
plot. On exit (right-click on the left panel), the function returns
all the cem solutions highlighted in the last selection of the user.

For non-interactive devices, only the space of the solutions are plotted.

This plot shows the tradeoff in matching as a function of
imbalance and sample size.

The imbalance of the raw data is represented as a red plot and the initial
CEM solution as a green plot. All solutions below the green dot and left to
it are better than the user choice in terms of imbalance and number of
units matched.

} 
\value{
  \item{tab}{an invisible object containing the selection of cem solutions and their
  coarsenings.}
}
\references{
Iacus, King, Porro (2011) \doi{10.1198/jasa.2011.tm09599}

Iacus, King, Porro (2012) \doi{10.1093/pan/mpr013}
	
Iacus, King, Porro (2019) \doi{10.1017/pan.2018.29}
}
\author{Stefano Iacus, Gary King, and Giuseppe Porro}
\seealso{ \code{\link{imbspace}} }
\examples{
\donttest{
require(cem)

data(LL)
set.seed(123)
mat <- cem("treated", LL, drop=c("re78","treated"), 
 cut=list(age=4, edu=4, re74=3, re75=3), keep.all=TRUE)
mat

imb.raw <- L1.profile(LL$treated, LL[, mat$vars], M=250, plot=FALSE)

imbsp <- imbspace(mat, LL,depth=2, raw.profile=imb.raw, 
 maximal=6, minimal=2, fixed=c("hispanic", "black", "married", 
 "nodegree","u74","u75"), plot=FALSE)

tmp <- plot(imbsp,data=LL,explore=interactive())
tmp 
}
}
\keyword{datagen}

