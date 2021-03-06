% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/split.R
\name{conformal.fun.split}
\alias{conformal.fun.split}
\title{Functional Split Conformal Prediction Intervals}
\usage{
conformal.fun.split(
  x,
  t_x,
  y,
  t_y,
  x0,
  train.fun,
  predict.fun,
  alpha = 0.1,
  split = NULL,
  seed = FALSE,
  randomized = FALSE,
  seed.rand = FALSE,
  verbose = FALSE,
  rho = 0.5,
  s.type = "st-dev"
)
}
\arguments{
\item{x}{The input variable, a list of n elements. Each element is composed by a list
of p vectors(with variable length, since the evaluation grid may change).
If x is NULL, the function will sample it from a gaussian.}

\item{t_x}{The grid points for the evaluation of function x. It is a list of vectors.
If the x data type is "fData" or "mfData" is must be NULL.}

\item{y}{The response variable. It is either, as with x, a list of list of
vectors or an fda object (of type fd, fData, mfData).}

\item{t_y}{The grid points for the evaluation of function y_val. It is a list of vectors.
If the y_val data type is "fData" or "mfData" is must be NULL.}

\item{x0}{The new points to evaluate, a list of n0 elements. Each element is composed
by a list of p vectors(with variable length).}

\item{train.fun}{A function to perform model training, i.e., to produce an
estimator of E(Y|X), the conditional expectation of the response variable
Y given features X. Its input arguments should be x: list of features,
and y: list of responses.}

\item{predict.fun}{A function to perform prediction for the (mean of the)
responses at new feature values. Its input arguments should be out: output
produced by train.fun, and newx: feature values at which we want to make
predictions.}

\item{alpha}{Miscoverage level for the prediction intervals, i.e., intervals
with coverage 1-alpha are formed. Default for alpha is 0.1.}

\item{split}{Indices that define the data-split to be used (i.e., the indices
define the first half of the data-split, on which the model is trained).
Default is NULL, in which case the split is chosen randomly.}

\item{seed}{Integer to be passed to set.seed before defining the random
data-split to be used. Default is FALSE, which effectively sets no seed.
If both split and seed are passed, the former takes priority and the latter
is ignored.}

\item{randomized}{Should the randomized approach be used? Default is FALSE.}

\item{seed.rand}{The seed for the randomized version.Default is FALSE.}

\item{verbose}{Should intermediate progress be printed out? Default is FALSE.}

\item{rho}{Split proportion between training and calibration set.
Default is 0.5.}

\item{s.type}{The type of modulation function.
Currently we have 3 options: "identity","st-dev","alpha-max". Default is "std-dev".}
}
\value{
A list with the following components: t,pred,average_width,lo, up.
t is a list of vectors,
pred has the same interval structure of y_val, but the outside list is of length n0,
lo and up are lists of length n0 of lists of length
p, each containing a vector of lower and upper bounds respectively.
}
\description{
Compute prediction intervals using split conformal inference.
}
\examples{
###  mfData #

library(roahd)

N = 10
P= 5
grid = seq( 0, 1, length.out = P )
C = exp_cov_function( grid, alpha = 0.3, beta = 0.4 )
Data_1 = generate_gauss_fdata( N, centerline = sin( 2 * pi * grid ), Cov = C )
Data_2 = generate_gauss_fdata( N, centerline = log(1+ 2 * pi * grid ), Cov = C )
mfD=mfData( grid, list( Data_1, Data_2 ) )
x0=list(as.list(grid))
fun=mean_lists()
final.mfData = conformal.fun.split(NULL,NULL, mfD,NULL, x0, fun$train.fun, fun$predict.fun,
                             alpha=0.2,
                             split=NULL, seed=FALSE, randomized=FALSE,seed.rand=FALSE,
                             verbose=TRUE, rho=0.5,s.type="identity")




}
