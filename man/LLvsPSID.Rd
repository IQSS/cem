\name{LLvsPSID}
\alias{LLvsPSID}
\docType{data}
\title{Lalonde treated units versus PSID control individuals}
\description{
The Lalonde set of treated units versus PSID (Panel Study of Income Dynamics) 
control individuals
}
\usage{data(LLvsPSID)}
\format{
  A data frame with 2787 observations on the following 10 variables.
  \describe{
    \item{\code{treated}}{treated variable indicator}
    \item{\code{age}}{age}
    \item{\code{education}}{years of education}
    \item{\code{black}}{race indicator variable}
    \item{\code{married}}{marital status indicator variable}
    \item{\code{nodegree}}{indicator variable of not possessing a degree}
    \item{\code{re74}}{real earnings in 1974}
    \item{\code{re75}}{real earnings in 1975}
    \item{\code{re78}}{real earnings in 1978 (post treatment outcome)}
    \item{\code{hispanic}}{ethnic indicator variable}
	\item{\code{u74}}{unemployment in 1974 indicator variable}
    \item{\code{u75}}{unemployment in 1975 indicator variable}
  }
}
\details{
These two sets of treated and control units can be hardly matched.
}
\source{see references}
\references{
Lalonde, R. (1986) Evaluating the Econometric Evaluations 
of Training Programs, \emph{American Economic Review}, 76, 604-620.
}
\keyword{datasets}
