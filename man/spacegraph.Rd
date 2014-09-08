\name{spacegraph}
\alias{spacegraph}
\title{Randomly compute many different matching solutions}
\description{
Randomly compute many different matching solutions
}
\usage{
spacegraph(treatment=NULL, data = NULL, 
     R=list("cem"=50,"psm"=0,"mdm"=0,"matchit"=0),
     grouping = NULL, drop=NULL, 
     L1.breaks = NULL, L1.grouping=NULL, fixed = NULL, 
     minimal = 1, maximal = 15, M=100, 
     raw.profile=NULL, keep.weights=FALSE, progress=TRUE,
     rgrouping=FALSE, groups=NULL, psmpoly=1, mdmpoly=1, 
     other.matches=NULL, heuristic=FALSE, linear.pscore=FALSE) 

}

\arguments{
  \item{treatment}{character, name of the treatment variable}
  \item{data}{a data.frame}
  \item{drop}{a vector of variable names in the data frame to ignore 
    during matching.}
  \item{R}{a named list that gives the number of of possible random solutions for each matching method.  Allowed methods are \code{cem}, \code{psm}, \code{mdm}, and \code{matchit}.}
  \item{grouping}{named list, each element of which is a list of
  groupings for a single categorical variable. For more details see \code{\link{cem}}.}
  \item{L1.breaks}{list of cutpoints for the calculation of the L1 measure.}
  \item{L1.grouping}{as \code{grouping} but only needed in the calculation
   of the L1 measure not in matching.}
  \item{fixed}{vector of variable names which will not be relaxed.} 
  \item{minimal}{the minimal number of intervals acceptable after
  relaxation. Should be a named list of positive integers or if a number, this is
  applied to all variables.}
  \item{maximal}{the maximal number of intervals acceptable after
  relaxation. Should be a named list of positive integers or if a number, this is
  applied to all variables.}
  \item{M}{number of possible random coarsening for the L1 measure}
  \item{raw.profile}{an object of class \code{L1profile}. If passed, the \code{L1.breaks} are
  ignored and set to median cutpoints of L1 profile.}
  \item{keep.weights}{if \code{TRUE}, for each matching solutions the CEM-weights are stored.}
  \item{rgrouping}{Boolean, specifies whether levels of categorical variables should be randomly grouped together by CEM.}
  \item{groups}{named list, each element of which is a list of
    allowable groupings for a single categorical variable.}
  \item{psmpoly}{numeric, specifying the order of polynomials to include in the propensity score models.  At the moment, only \code{psmpoly=1} is available and other values will throw warnings.}
  \item{mdmpoly}{numeric, specifying the order of polynomials to include in Mahalanobis matching.  At the moment, only \code{mdmpoly=1} is available and other values will throw warnings.}
  \item{other.matches}{This argument allows plotting of user-specified matching solutions.  The solutions must be supplied in a specific format: as a list of data frames, where each data frame contains the observation IDs, observations weights, and the method.  These must be provided in three columns of the data frame, with names (exactly) "id", "weight", "method".  See the example.}
  \item{heuristic}{Boolean, if TRUE spacegraph uses a heuristic method to randomly select covariates for MDM and PSM rather than sampling from all possible combinations of covariates.  The heuristic tends to select most of the main effects and a small number of interactions.  This method is automatically applied with large numbers of covariates.}
  \item{progress}{show progress bars.}
 \item{linear.pscore}{does \code{linear.pscore}}
}

\details{
Spacegraph is a tool to help the user to the search for optimal matching solutions by generating many matching solutions from a variety of matching algorithms (currently CEM, Mahalanobis distance matching, and propensity score matching are supported).  The resulting object can be plotted with \code{plot()} to show where each solution falls along the bais-variance tradeoff.

The \code{spacegraph} function currently calculates two measures of balance for each solution: the L1 metric (see \code{\link{L1.meas}} ) and the difference in means of the covariates.  Typically, analyists look at the difference in means seperately for each variable, but this can't be plotted on a two-dimensional graph.  We summarize the difference in means by calculating the average difference in means for all of the covariates.  Specifically, we calculate the difference in means for each variable as mean(treated)-mean(control)/sd(treated) and then average across all covariates.

} 
\value{
  \item{val}{an object of class \code{spacegraph} that can be used directly with \code{plot()} to produce a spacegraph.}
}

\author{Richard Nielsen, Stephano Iacus, Gary King, and Guiseppe Porro}
\seealso{ \code{\link{combine.spacegraphs}} }
\examples{
\dontrun{
data(LL)

sp <- spacegraph("treated", LL, drop="re78", M=5,
                  R=list(cem=5,psm=5, mdm=5))

plot(sp)
## ABOUT THE PLOTTING TOOL:
## The circled solution is the current selection.
## Solutions that are strictly better are also circled.
## The gui provides the exact call to re-run the selected matching solution.
## The call can also be edited, re-run by clicking the button, and 
## automatically added to the existing spacegraph.
## CEM solutions can also be adjusted variable by variable and re-run.


## Some plotting parameters can be changed
plot(sp, main="Comparison of Matching Methods",
  ylab="L1", xlim=c(300,50), ylim=c(0,.7))

## You can specify whether the x-axis shows treated units, 
## control units, or all units using the argument N, which 
## can take the values "treated", "control", or "all".
## Default is "treated".

plot(sp, N="all")

## You can specify how the x-axis is scaled.  Setting scale.var=T
## gives you the scaling as 1/sqrt(n).  Setting scale.var=F gives
## scales it linearly.  Default is scale.var=T.

plot(sp, scale.var=F)

## You can also specify which measure of balance to use
## by specifying the argument "balance.metric" as
## "L1", "mdiff" (Avg. standardized difference in means), 
## or "mdisc" (Average Malanobis Discrepancy).  Default is "L1".

plot(sp, balance.metric="mdiff")
plot(sp, balance.metric="mdisc")


## Matching solutions from other methods can be included in
## a spacegraph by using the argument "other.matches".
## First, Run a matching method.  Here, propensity scores from MatchIt.
library(MatchIt)
m.out <- matchit(formula=treated ~ education+age, data=LL, method = "nearest")

## Put the required information into a list of data frames.
## Note, there are many ways to do this.
mymatches <- list(data.frame(names(m.out$w)))
names(mymatches[[1]])[1] <- "id"
mymatches[[1]]$weight <- m.out$w
mymatches[[1]]$method <- "matchit psm"

sp <- spacegraph("treated", LL, drop="re78", M=5,
 R=list(cem=5,psm=5, mdm=5), other.matches=mymatches)
plot(sp)

}
}
\keyword{datagen}

