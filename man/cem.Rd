\name{cem}
\alias{cem}
\title{Coarsened Exact Matching}
\description{
Implementation of Coarsened Exact Matching
}
\usage{
cem(treatment=NULL, data = NULL, datalist=NULL, cutpoints = NULL,  
    grouping = NULL, drop=NULL, eval.imbalance = FALSE, k2k=FALSE,  
	method=NULL, mpower=2, L1.breaks = NULL, L1.grouping = NULL, 
    verbose = 0, baseline.group="1",keep.all=FALSE)
}
\arguments{
  \item{treatment}{character, name of the treatment variable}
  \item{baseline.group}{character, name of the baseline level treatment. See Details.}
  \item{data}{a data.frame}
  \item{datalist}{a list of optional multiply imputed data.frame's}
  \item{cutpoints}{named list each describing the cutpoints for numerical
  variables (the names are variable names).
  Each list element is either a vector of cutpoints, a number of
  cutpoints, or a method for automatic bin contruction. See Details.}
  \item{grouping}{named list, each element of which is a list of
  groupings for a single categorical variable. See Details.}
  \item{drop}{a vector of variable names in the data frame to ignore
    during matching}
  \item{eval.imbalance}{Boolean. See Details.}
  \item{k2k}{boolean, restrict to k-to-k matching? Default = \code{FALSE}}
  \item{method}{distance method to use in \code{k2k} matching. See Details.}
  \item{mpower}{power of the Minkowski distance. See Details.}
  \item{L1.breaks}{list of cutpoints for the calculation of the L1 measure.}
  \item{L1.grouping}{as \code{grouping} but only needed in the calculation
   of the L1 measure not in matching.}
  \item{verbose}{controls level of verbosity. Default=0.}
  \item{keep.all}{if \code{FALSE} the coarsened dataset is not returned. Default=\code{FALSE}}
}
\details{
For multilevel (and a binary) treatment variables, the cem weights
are calulated with respect to the \code{baseline}. Therefore,
matched units with treatment variable equal to the baseline level receive weight 1, the others the usual cem weights. Unless specified,
by default \code{baseline} is set
to \code{"1"}. If this level is not one of the possible values taken by
the \code{treatment} variable, then the baseline is set to the first level of the \code{treatment} variable.


When specifying cutpoints, several automatic methods may be chosen, including
``\code{sturges}'' (Sturges' rule, the default),
``\code{fd}'' (Freedman-Diaconis' rule), ``\code{scott}'' 
(Scott's rule) and ``ss'' (Shimazaki-Shinomoto's rule). 
See references for a description of each rule.

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


\code{verbose}: a number greater or equal to 0. The higher, the 
more info are provided during the execution of the algorithm. 

If \code{eval.imbalance} = \code{TRUE},
\code{cem$imbalance} contains the imbalance measure by absolute
difference in means for numerical variables and chi-square distance for
categorical variables.  If \code{FALSE} (the default) then \code{cem$imbalance} is set
to \code{NULL}. If data contains missing data, the imbalance measures
are not calculated.

If \code{L1.breaks} is missing, the default rule to calculate cutpoints
is the Scott's rule. 

If \code{k2k} is set to \code{TRUE}, the algorithm return strata with
the same number of treated and control units per stratum, otherwise all
the matched units are returned (default). When \code{k2k} = \code{TRUE},
the user can choose a \code{method} (between `\code{euclidean}',
`\code{maximum}', `\code{manhattan}', `\code{canberra}', `\code{binary}'
and `\code{minkowski}') for nearest neighbor matching inside each
\code{cem} strata. By default \code{method} is set to `\code{NULL}',
which means random matching inside \code{cem} strata. For the Minkowski
distance the power can be specified via the argument \code{mpower}'.
For more information on \code{method != NULL}, refer to
\code{\link{dist}} help page.
If \code{k2k} is set to \code{TRUE} also \code{keep.all} is set to \code{TRUE}.

By default, \code{cem} treats missing values as distinct categories and
matches observations with missing values in the same variable in the
same stratum provided that all the remaining (corasened) covariates
match. 

If argument \code{data} is non-\code{NULL} and \code{datalist} is
\code{NULL}, CEM is applied to the single data set in \code{data}.

Argument \code{datalist} is a list of (multiply imputed) data frames
(i.e., with missing cell values imputed).  If \code{data} is
\code{NULL}, the function \code{cem} is applied independently to each
element of the list, resulting in separately matched data sets with
different numbers of treated and control units.
 
When \code{data} and \code{datalist} are both non-\code{NULL}, each
multiply imputed observation is assigned to the stratum in which it has
been matched most frequently.  In this case, the algorithm outputs the
same matching solution for each multiply imputed data set (i.e., an
observation, and the number of treated and control units matched, in one
data set has the same meaning in all, and is the same for all)
}
\value{
  Returns an object of class \code{cem.match} if only \code{data} is not 
  \code{NULL} or an object of class \code{cem.match.list}, which is a list of
  objects of class \code{cem.match} plus a field called \code{unique} which
  is true only if \code{data} and \code{datalist} are not both \code{NULL}.   
  A \code{cem.match} object is a list
  with the following slots:
  \item{call}{the call}
  \item{strata}{vector of stratum number in which each observation belongs, 
				NA if the observation has not been matched}
  \item{n.strata}{number of strata generated}
  \item{vars}{report variables names used for the match}
  \item{drop}{variables removed from the match}
  \item{X}{the coarsened dataset or NULL if \code{keep.all}=\code{FALSE}}
  \item{breaks}{named list of cutpoints, eventually NULL}
  \item{treatment}{name of the treatment variable}
  \item{groups}{factor, each observation belong to one group generated by the 
				treatment variable}
  \item{n.groups}{number of groups identified by the treatment variable}
  \item{group.idx}{named list, index of observations belonging to each group}
  \item{group.len}{sizes of groups}
  \item{tab}{summary table of matched by group}
  \item{imbalance}{NULL or a vector of imbalances. See Details.}
}
\references{
Iacus, King, Porro (2011) \url{https://doi.org/10.1198/jasa.2011.tm09599}

Iacus, King, Porro (2012) \url{https://doi.org/10.1093/pan/mpr013}
	
Iacus, King, Porro (2019) \url{ https://doi.org/10.1017/pan.2018.29}
	
Shimazaki, Shinomoto (2007) \url{https://doi.org/10.1162/neco.2007.19.6.1503}
}
 
\author{Stefano Iacus, Gary King, and Giuseppe Porro}
\examples{
\donttest{
data(LL)

   
todrop <- c("treated","re78")
   
imbalance(LL$treated, LL, drop=todrop)

# cem match: automatic bin choice
mat <- cem(treatment="treated", data=LL, drop="re78")
mat

# cem match: user choiced coarsening
re74cut <- hist(LL$re74, br=seq(0,max(LL$re74)+1000, by=1000),plot=FALSE)$breaks
re75cut <- hist(LL$re75, br=seq(0,max(LL$re75)+1000, by=1000),plot=FALSE)$breaks
agecut <- hist(LL$age, br=seq(15,55, length=14),plot=FALSE)$breaks
mycp <- list(re75=re75cut, re74=re74cut, age=agecut)
mat <- cem(treatment="treated",data=LL, drop="re78",cutpoints=mycp)
mat


# cem match: user choiced coarsening, k-to-k matching
mat <- cem(treatment="treated",data=LL, drop="re78",cutpoints=mycp,k2k=TRUE)
mat

# mahalnobis matching: we use MatchIt
if(require(MatchIt)){
mah <- matchit(treated~age+education+re74+re75+black+hispanic+nodegree+married+u74+u75,
   distance="mahalanobis", data=LL)
mah
#imbalance
imbalance(LL$treated, LL, drop=todrop, weights=mah$weights)
}

# Multiply Imputed data
# making use of Amelia for multiple imputation
if(require(Amelia)){
 data(LL)
 n <- dim(LL)[1]
 k <- dim(LL)[2]

 set.seed(123)

 LL1 <- LL
 idx <- sample(1:n, .3*n)
 for(i in idx){
  LL1[i,sample(2:k,1)] <- NA
 }

 imputed <- amelia(LL1,noms=c("black","hispanic","treated","married",
                              "nodegree","u74","u75")) 
 imputed <- imputed$imputations[1:5]
# without information on which observation has missing values
 mat1 <- cem("treated", datalist=imputed, drop="re78")
 mat1

# ATT estimation
 out <- att(mat1, re78 ~ treated, data=imputed)


# with information about missingness
 mat2 <- cem("treated", datalist=imputed, drop="re78", data=LL1)
 mat2

# ATT estimation
 out <- att(mat2, re78 ~ treated, data=imputed)
}
}
}
\keyword{multivariate}
\keyword{datagen}


