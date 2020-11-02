\name{LeLonde}
\alias{LeLonde}
\docType{data}
\title{Modified Lalonde dataset}
\description{
This is a modified version of the Lalonde experimental dataset used for
explanatory reasons only.
}
\usage{data(LL)}
\format{
  A data frame with 722 observations on the following 11 variables.
  \describe{
    \item{\code{treated}}{treatment variable indicator}
    \item{\code{age}}{age}
    \item{\code{education}}{years of education}
    \item{\code{black}}{race indicator variable}
    \item{\code{married}}{marital status indicator variable}
    \item{\code{nodegree}}{indicator variable for not possessing a degree}
    \item{\code{re74}}{real earnings in 1974}
    \item{\code{re75}}{real earnings in 1975}
    \item{\code{re78}}{real earnings in 1978 (post-treatment outcome)}
    \item{\code{hispanic}}{ethnic indicator variable}
    \item{\code{u74}}{unemployment in 1974 indicator variable}
    \item{\code{u75}}{unemployment in 1975 indicator variable}
    \item{\code{q1}}{answer to survey question n1}	
  }
}
\details{
This data is a copy of the original Lalonde (1986) data set (see \code{\link{LL}})
with 10\% of missing data and an additional variable \code{q1} which is the
fictituous answer to the questionarie on ``Agreement on this job training program''.
}
\source{see references}
\references{
Iacus, King, Porro (2011) \url{https://doi.org/10.1198/jasa.2011.tm09599}

Iacus, King, Porro (2012) \url{https://doi.org/10.1093/pan/mpr013}
	
Iacus, King, Porro (2019) \url{https://doi.org/10.1017/pan.2018.29}

Lalonde (1986) Evaluating the Econometric Evaluations of Training Programs with Experimental Data, \emph{The American Economic Review},
76(4), 604-620. 
}
\keyword{datasets}
