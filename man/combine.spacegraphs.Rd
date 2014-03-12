\name{combine.spacegraphs}
\alias{combine.spacegraphs}
\title{Combine two spacegraph objects.}
\description{
Combine two spacegraph objects so that their results can be plotted together. Both spacegraphs must be from the same dataset using the same distance metric.  
}
\usage{
combine.spacegraphs(x,y) 
}
\arguments{
  \item{x}{a spacegraph object to be combined}
  \item{y}{a spacegraph object to be combined}
}

\details{
This allows users to combine two spacegraph objects rather than having to re-run the spacegraph command from the start.

Inputs must be created using \code{\link{spacegraph}}.
} 
\value{
  \item{val}{an object of class \code{spacegraph}.}
}
\author{Richard Nielsen}
\seealso{ \code{\link{spacegraph}} }
\examples{
data(LL)

sp1 <- spacegraph("treated", LL, drop="re78", M=5,
                  R=list(cem=5,psm=5, mdm=0))

## Note that we must use the same L1 measure from the first spacegraph!
sp2 <- spacegraph("treated", LL, drop="re78", raw.profile=sp1$raw.profile,
                  R=list(cem=0,psm=0, mdm=5))

sp3 <- combine.spacegraphs(sp1,sp2)

plot(sp3)

}
\keyword{datagen}
