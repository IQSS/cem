\name{att}
\alias{att}
\alias{plot.cem.att}
\alias{summary.cem.att}
\title{Example of ATT estimation from CEM output}
\description{
An example of ATT estimation from CEM output
}
\usage{
att(obj, formula, data, model="linear", extrapolate=FALSE, ntree=2000)
\method{plot}{cem.att}(x, obj, data, vars=NULL, plot=TRUE, ecolors, ...)
\method{summary}{cem.att}(object, ...)
}
\arguments{
  \item{obj}{a \code{cem.match} or \code{cem.match.list} object}
  \item{formula}{a model formula. See Details.}
  \item{data}{a single data.frame or a list of data.frame's in case of \code{cem.match.list}}
  \item{model}{one model. See Details.}
  \item{extrapolate}{extrapolate the CEM restriced estimate to the whole data. Default = \code{FALSE}.}
  \item{ntree}{number of trees to generate in random forest model. Default = \code{2000}.}
  \item{x}{the output from the \code{att} function}
  \item{vars}{a vector of variable names to be used in the parallel plots. By default all variables
    involved in data matching are used.}
  \item{object}{an object of class \code{cem.att} function}
  \item{plot}{if \code{TRUE} the plot is produced, otherwise only calculations are made.}
  \item{ecolors}{a vector of three colors respectively for positive, zero and negative 
      treatment effect. Default \code{c("blue","black","red")}.}
  \item{...}{passed to the plot function or to \code{printCoefmat} for the method summary}
}
\details{

Argument \code{model} can be \code{lm, linear} for linear regression
model; \code{logit} for the the logistic model;
\code{lme, linear-RE} for the linear model with random effects. 
Also \code{rf, forest} for the randomforest algorithm.

If the outcome is \code{y} and the
treatment variable is \code{T}, then a \code{formula} like \code{y ~ T}
will produce the simplest estimate the ATT: with lm, it is just the
coefficient on \code{T}, which is the same as the difference in means,
weighted by CEM stratum size.  Users can add covariates to span any
remaining imbalance after the match, such as \code{y ~ T + age + sex},
to adjust for variables \code{age} and \code{sex}.

In the case of multiply imputed datasets, the model is applied to each
single matched data and the ATT and is the standard error estimated
using the standard formulas for combining results of multiply imputed
data.  

When \code{extrapolate} = \code{TRUE}, the estimate model is extrapolated
to the whole set of data.


There is a \code{print} method for the output of \code{att}. Specifying the
option \code{TRUE} in a \code{print} command gives complete output from the
estimated model when availalble.

}
\value{
  A matrix of estimates with their standard error, or a list in
  the case of \code{cem.match.list}.  For \code{plot.att} a list of strata 
  estimated treatment effect and group ("positive", "negative", "zero") and 
  individual treatment and effect and group. The individual treatment effect
  and group is given by the treatment effect of the strata. Similarly for
  the group ("positive", "negative", "zero"). Also, colors associated to
  estimated treatment effects are returned for easy subsequent plotting.
} 
\references{
Iacus, King, Porro (2011) \url{https://doi.org/10.1198/jasa.2011.tm09599}

Iacus, King, Porro (2012) \url{https://doi.org/10.1093/pan/mpr013}
	
Iacus, King, Porro (2019) \url{ https://doi.org/10.1017/pan.2018.29}
}
\author{Stefano Iacus, Gary King, and Giuseppe Porro}

\examples{ 
data(LL)

# cem match: automatic bin choice
mat <- cem(treatment="treated",data=LL, drop="re78", keep.all=TRUE)
mat
mat$k2k

# ATT estimate
homo1 <- att(mat, re78~treated,  data=LL)
rand1 <- att(mat, re78~treated,  data=LL, model="linear-RE")
rf1 <- att(mat, re78~treated,  data=LL, model="rf")

homo2 <- att(mat, re78~treated,  data=LL, extra=TRUE)
rand2 <- att(mat, re78~treated,  data=LL, model="linear-RE", extra=TRUE)
rf2 <- att(mat, re78~treated,  data=LL, model="rf", extra=TRUE)

homo1
summary(homo1)

rand1
rf1

homo2
rand2
rf2

plot( homo1, mat, LL, vars=c("age","education","re74","re75"))
plot( rand1, mat, LL, vars=c("age","education","re74","re75"))
plot( rf1, mat, LL, vars=c("age","education","re74","re75"))

plot( homo2, mat, LL, vars=c("age","education","re74","re75"))
plot( rand2, mat, LL, vars=c("age","education","re74","re75"))
plot( rf2, mat, LL, vars=c("age","education","re74","re75"))


# reduce the match into k2k using euclidean distance within cem strata
mat2 <- k2k(mat, LL, "euclidean", 1)
mat2
mat2$k2k

# ATT estimate after k2k
att(mat2, re78~treated, data=LL)

# example with missing data
# using multiply imputated data
# we use Amelia for multiple imputation
\donttest{

 if(require(Amelia)){
  data(LL)
  n <- dim(LL)[1]
  k <- dim(LL)[2]

# we generate missing values in 30 percent of the rows of LL data
# randomly in one colum per row
  set.seed(123)
  LL1 <- LL
  idx <- sample(1:n, .3*n)
  for(i in idx){
    LL1[i,sample(2:k,1)] <- NA
  }

  imputed <- amelia(LL1)
  imputed <- imputed$imputations[1:5]

  mat <- cem("treated", datalist=imputed, data=LL1, drop="re78")

  print(mat)
  
  att(mat, re78 ~ treated, data=imputed)
 }
}
}
\keyword{multivariate}

