\name{pair}
\alias{pair}
\title{Produces a paired sample out of a CEM match solution}
\description{
Produces a paired sample out of a CEM match solution
}
\usage{
pair(obj, data, method=NULL, mpower=2, verbose=1)
}
\arguments{
  \item{obj}{an object as output from \code{cem}}
  \item{data}{the original data.frame used by \code{cem}}
  \item{method}{distance method to use in \code{k2k} matching. See Details.}
  \item{mpower}{power of the Minkowski distance. See Details.}
  \item{verbose}{controls level of verbosity. Default=0.}
}
\details{
This function returns a vector of paired matched units index.

The user can choose a \code{method} (between `\code{euclidean}',
`\code{maximum}', `\code{manhattan}', `\code{canberra}', `\code{binary}'
and `\code{minkowski}') for nearest neighbor matching inside each
\code{cem} strata. By default \code{method} is set to `\code{NULL}',
which means random matching inside \code{cem} strata. For the Minkowski
distance the power can be specified via the argument \code{mpower}'.
For more information on \code{method != NULL}, refer to
\code{\link{dist}} help page.

}
\value{
  \item{obj}{a list with the fields \code{paired}, \code{full.paired},
  \code{reservoir} and \code{reservoir2}. The latter contain the indexes
  of the unmatched units.}
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
# cem match: automatic bin choice
mat <- cem(data=LL, drop="re78")

# we want a set of paired units
psample <- pair(mat, data=LL)
table(psample$paired)
psample$paired[1:100]

table(psample$full.paired)
psample$full.paired[1:10]



# cem match: automatic bin choice, we drop one row from the data set
mat1 <- cem(data=LL[-1,], drop="re78")

# we want a set of paired units but we have an odd number of units in the data
psample <- pair(mat1, data=LL[-1,])
table(psample$full.paired)
}
}
\keyword{multivariate}
\keyword{datagen}


