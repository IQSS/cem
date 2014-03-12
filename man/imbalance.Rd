\name{imbalance}
\alias{imbalance}
\title{Calculates several imbalance measures}
\description{
Calculates several imbalance measures for the original
and matched data sets}
\usage{
imbalance(group, data, drop=NULL, breaks = NULL, weights, grouping = NULL)
}
\arguments{
 \item{group}{the group variable}
  \item{data}{the data}
  \item{drop}{a vector of variable names in the data frame to ignore}
  \item{breaks}{a list of vectors of cutpoints used to calculate the L1
  measure. See Details.}
  \item{weights}{weights}
  \item{grouping}{named list, each element of which is a list of
  groupings for a single categorical variable. See Details.}
}
\details{
This function calculates several imbalance measures.
For numeric variables, the difference in means (under the column
 \code{statistic}), the difference in quantiles and the L1 measure is
 calculated. For categorical variables the L1 measure and the
 Chi-squared distance (under column \code{statistic}) is calculated.
 Column \code{type} reports either \code{(diff)} or \code{(Chi2)} to
 indicate the type of statistic being calculated.

If \code{breaks} is not specified, the Scott automated bin calculation
is used (which coarsens less than Sturges, which used in
\code{\link{cem}}).  Please refer to \code{\link{cem}} help page. In
this case, breaks are used to calculate the L1 measure.

This function also calculate the global L1 imbalance measure. 
If \code{breaks} is missing, the default rule to calculate cutpoints
is the Scott's rule. 

The \code{grouping} option is a list where each element is itself a
list. For example, suppose for variable \code{quest1} you have the
following possible levels \code{"no answer", NA, "negative", "neutral",
"positive"} and you want to collect \code{("no answer", NA, "neutral")}
into a single group, then the \code{grouping} argument should contain
\code{list(quest1=list(c("no answer", NA, "neutral")))}. Or if you have
a discrete variable \code{elements} with values \code{1:10} and you want
to collect it into groups ``\code{1:3,NA}'', ``\code{4}'',
``\code{5:9}'', ``\code{10}'' you specify in \code{grouping} the
following list \code{list(elements=list(c(1:3,NA), 5:9))}.  Values not
defined in the \code{grouping} are left as they are. If \code{cutpoints}
and \code{groupings} are defined for the same variable, the
\code{groupings} take precedence and the corresponding cutpoints are set
to \code{NULL}.

See \code{\link{L1.meas}} help page for details.  
}
\value{
  An object of class \code{imbalance} which is a list with the following
  two elements
  \item{tab}{Table of imbalance measures}
  \item{L1}{The global L1 measure of imbalance}
}
\references{Stefano Iacus, Gary King, Giuseppe Porro, ``Matching for
  Casual Inference Without Balance Checking: Coarsened Exact Matching,''
  http://gking.harvard.edu/files/abs/cem-abs.shtml}
\author{Stefano Iacus, Gary King, and Giuseppe Porro}
\examples{ 

data(LL)

todrop <- c("treated","re78")
   
imbalance(LL$treated, LL, drop=todrop)

# cem match: automatic bin choice
mat <- cem(treatment="treated", data=LL, drop="re78")
}
\keyword{ datagen }
