\name{plot.spg}
\alias{plot.spg}
\title{Plot of randomly generated matching solutions}
\description{
Plot of randomly generated matching solutions
}
\usage{
plot.spg(spg.obj, clickforlabels=T,
                      cem.col="#0000ff50", psm.col="#00ff0085",
                      mdm.col="#ff000050", cem.pch=20, psm.pch=20,
                      mdm.pch=20,...)
}
\arguments{
  \item{spg.obj}{a list object created by \code{spacegraph}.}
  \item{clickforLabels}{Boolean.  If set to \code{TRUE}, it uses \code{identify} to add labels to selected points.}
  \item{cem.col}{the color for plotting cem solutions.}
  \item{psm.col}{the color for plotting psm solutions.}
  \item{mdm.col}{the color for plotting mdm solutions.}
  \item{cem.pch}{the point type for plotting cem solutions.}
  \item{psm.pch}{the point type for plotting psm solutions.}
  \item{mdm.pch}{the point type for plotting mdm solutions.}
  \item{...}{takes additional plotting commands.}
}

\details{
Makes spacegraph plots using the output of the \code{spacegraph} function.

} 
\value{
  Returns a plot.
}
\author{Richard Nielsen}
\seealso{ \code{\link{spacegraph}}}
\examples{
\dontrun{
data(LL)
myvars <- names(LL)[c(2:8,10:12)]
#out <- spacegraph(treatment="treated",
#                  rgrouping=F,
#                  data = LL,
#                  mvars = myvars,
#                  cem.R=10,psm.R=10,mdm.R=10,rawprof.M=100,
#                  mdmpoly=1,psmpoly=2,
#                  maximal=30)

#plot.spg(out, main="Example Space Graph\nLalonde Data")

# click to identify points

# accessing CEM solution 2
#out$cem$space
#out$cem$cem.objs[[2]]

# accessing PSM solution 5
#out$psm$tab[5,]
#out$psm$matched[[5]]

# accessing MDM solution 3
#out$mdm$tab[3,]
#out$mdm$matched[[3]]
}
}
\keyword{datagen}

