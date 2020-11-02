\name{pscoreSelect}
\alias{pscoreSelect}
\title{Heuristic search of the best propensity score model specification}
\description{
Heuristic search of the best propensity score model specification
}
\usage{
pscoreSelect(formula, data, C.L=2*(pnorm(-1,0,1)), C.Q=0.1,verbose=1) 
}
\arguments{
  \item{data}{the original data.}
  \item{formula}{formula type specification.}
  \item{C.L}{if at leat one likelihood ration test statistic is greater
  than \code{C.L}, then the covariate with higher likelihood ration test statistic
  is added linearly to the model.}
  \item{C.Q}{if the highest likelihood ratio statistic is greater than \code{C.Q},
  then interaction terms are included in the pscore model specification.}
  \item{verbose}{integer, prints more info on screen.}
}

\details{
This is a tool to help the user to the search for the best
propensity score model specification along the lines
suggested by Imbens and Rubin (forthcoming). The output of the function is
a model formula to be passed to \code{glm} or such, in order
to estimate the propensity score model and then perform 
propensity score matching.

This tool is useful in combination with \code{\link{imbspace.plot}}.
} 
\value{
  \item{val}{an invisible object of class \code{list}.}
}
\references{
Iacus, King, Porro (2011) \url{https://doi.org/10.1198/jasa.2011.tm09599}

Iacus, King, Porro (2012) \url{https://doi.org/10.1093/pan/mpr013}
	
Iacus, King, Porro (2019) \url{ https://doi.org/10.1017/pan.2018.29}

Imbens,  Rubin (2015)  \url{https://doi.org/10.1017/CBO9781139025751}
}
\author{Richard Nielsen}
\seealso{ \code{\link{cemspace}} }
\examples{
\donttest{
set.seed(123)
data(LL)
mod <- pscoreSelect( treated ~ age + education + black+ married + nodegree + 
 re74 + re75 + hispanic + u74 + u75, data=LL)
print(mod)
}
}
\keyword{datagen}

