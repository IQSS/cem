\name{L1.meas}
\alias{L1.meas}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Evaluates L1 distance between multidimensional histograms}
\description{
Evaluates L1 distance between multidimensional histograms
}
\usage{
L1.meas(group, data, drop=NULL, breaks = NULL, weights, grouping = NULL)
}
\arguments{
  \item{group}{the group variable}
  \item{data}{the data}
  \item{drop}{a vector of variable names in the data frame to ignore}
  \item{breaks}{a list of vectors of cutpoints; if not specified,
  automatic choice will be made}
  \item{weights}{weights}
  \item{grouping}{named list, each element of which is a list of
  groupings for a single categorical variable. See Details.}
}
\details{
This function calculates the L1 distance on the k-dimensional histogram
in order to measure the level of imbalance in a matching solution.

If \code{breaks} is not specified, the Scott automated bin calculation
is used (which coarsens less than Sturges, which used in
\code{\link{cem}}).  Please refer to \code{\link{cem}} help page. In
this case, breaks are used to calculate the L1 measure.

When choosing \code{breaks} for L1, a very fine coarsening (many cut
points) produces values of L1 close to 1.  A very mild coarsening (very
fex cutpoints), is not able to discriminate, i.e. L1 close to 0
(particularly true when the number of observations is small with respect
to the number of continuous variables).  

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


The \code{\link{L1.profile}} function
shows how to compare matching solutions for any level of (i.e., without
regard to) coarsening.

This code also calculate the Local Common Support (LCS) measure, which is
the proportion of non empty k-dimensional cells of the histogram which
contain at least one observation per group.
}
\value{
  An object of class \code{L1.meas} which is a list with the following fields
  \item{L1}{The numerical value of the L1 measure}
  \item{breaks}{A list of cutpoints used to calculate the L1 measure}
  \item{LCS}{The numerical value of the Local Common Support proportion}
}
\references{
Iacus, King, Porro (2011) \doi{10.1198/jasa.2011.tm09599}

Iacus, King, Porro (2012) \doi{10.1093/pan/mpr013}
	
Iacus, King, Porro (2019) \doi{10.1017/pan.2018.29}
}
\author{Stefano Iacus, Gary King, and Giuseppe Porro}
\examples{
data(LL)
set.seed(123)
L1.meas(LL$treated,LL, drop=c("treated","re78"))
}
\keyword{ datagen }
