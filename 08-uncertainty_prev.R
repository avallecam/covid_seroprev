library(tidyverse)
library(skimr)
library(purrr)
library(furrr)
library(tictoc)

set.seed(33)

# METHODS -----------------------------------------------------------------


# input for reproducible examples

# sensitivity = 0.93
# specificity = 0.975
# positive_pop <- c(321, 123, 100, 10)
# negative_pop <- c(1234, 500, 375, 30)

# . -------------------------------------------------------------------------
# . -------------------------------------------------------------------------

# FREQUENTIST -------------------------------------------------------------


# . -------------------------------------------------------------------------
# . -------------------------------------------------------------------------

# BAYESIAN ----------------------------------------------------------------

# 02 larremorre method 2020 --------------------------------------------------

# . -----------------------------------------------------------------------


# _ KNOWN performance -----------------------------------------------------------------


source("covid_serological_sampling.R")

# . -----------------------------------------------------------------------


# __ custom functions -----------------------------------------------------


# . -----------------------------------------------------------------------


# _ UNKNOWN performance ---------------------------------------------------------------



# . -------------------------------------------------------------------------
# . -------------------------------------------------------------------------


# _ EXTRA cleaning procedures ---------------------------------------------



# . -------------------------------------------------------------------------
# . -------------------------------------------------------------------------

# 03 diggle method 2011 ------------------------------------------------------
# this is for unknown Se or Sp
#' limitation: 
#' requires more theta for higher number of test performed
#' this complicates the parameter setup
#' generating error at
#' Error in while ((prob < coverage/bin.width) & (i < ntheta)) { :

#
# R function for Bayesian estimation of prevalence using an
# imperfect test.
#
# Notes
#
#' 1. Prior for prevalence is uniform on (0,1)
#' 
#' 2. Priors for sensitivity and specificity are independent scaled
#' beta distributions
#' 
#' 3. Function uses a simple quadrature algorithm with number of
#' quadrature points as an optional argument "ngrid" (see below);
#' the default value ngrid=20 has been sufficient for all examples
#' tried by the author, but is not guaranteed to give accurate
#' results for all possible values of the other arguments.
#
prevalence.bayes<-function(theta,T,n,
                           lowse=0.5,highse=1.0,
                           sea=1,seb=1,
                           lowsp=0.5,highsp=1.0,
                           spa=1,spb=1,
                           ngrid=20,coverage=0.95) {
  #
  #' [arguments]
  #' theta: vector of prevalences for which posterior density is required
  #' (will be converted internally to increasing sequence of equally
  #' spaced values, see "result" below)
  #' T: number of positive test results
  #' n: number of indiviudals tested
  #' lowse: lower limit of prior for sensitivity
  #' highse: upper limit of prior for sensitivity
  #' sea,seb: parameters of scaled beta prior for sensitivity
  #' lowsp: lower limit of prior for specificity
  #' highsp: upper limit of prior for specificity
  #' spa,spb: parameters of scaled beta prior for specificity
  #' ngrid: number of grid-cells in each dimension for quadrature
  #' coverage: required coverage of posterior credible interval
  #' (warning message given if not achieveable)
  #
  #' [result is a list with components]
  #' theta: vector of prevalences for which posterior density has
  #' been calculated
  #' post: vector of posterior densities
  #' mode: posterior mode
  #' interval: maximum a posteriori credible interval
  #' coverage: achieved coverage
  #
  ibeta<-function(x,a,b) {
    pbeta(x,a,b)*beta(a,b)
  }
  ntheta<-length(theta)
  bin.width<-(theta[ntheta]-theta[1])/(ntheta-1)
  theta<-theta[1]+bin.width*(0:(ntheta-1))
  integrand<-array(0,c(ntheta,ngrid,ngrid))
  h1<-(highse-lowse)/ngrid
  h2<-(highsp-lowsp)/ngrid
  for (i in 1:ngrid) {
    se<-lowse+h1*(i-0.5)
    pse<-(1/(highse-lowse))*dbeta((se-lowse)/(highse-lowse),sea,seb)
    for (j in 1:ngrid) {
      sp<-lowsp+h2*(j-0.5)
      psp<-(1/(highsp-lowsp))*dbeta((sp-lowsp)/(highsp-lowsp),spa,spb)
      c1<-1-sp
      c2<-se+sp-1
      f<-(1/c2)*choose(n,T)*(ibeta(c1+c2,T+1,n-T+1)-ibeta(c1,T+1,n-T+1))
      p<-c1+c2*theta
      density<-rep(0,ntheta)
      for (k in 1:ntheta) {
        density[k]<-dbinom(T,n,p[k])/f
      }
      integrand[,i,j]<-density*pse*psp
    }
  }
  post<-rep(0,ntheta)
  for (i in 1:ntheta) {
    post[i]<-h1*h2*sum(integrand[i,,])
  }
  ord<-order(post,decreasing=T)
  mode<-theta[ord[1]]
  take<-NULL
  prob<-0
  i<-0
  while ((prob<coverage/bin.width)&(i<ntheta)) {
    i<-i+1
    take<-c(take,ord[i])
    prob<-prob+post[ord[i]]
  }
  if (i==ntheta) {
    print("WARNING: range of values of theta too narrow")
  }
  interval<-theta[range(take)]
  list(theta=theta,post=post,mode=mode,interval=interval,coverage=prob*bin.width)
}
#
# example
#

# # reproducible example 05
# result<-prevalence.bayes(theta=0.001*(1:400),
#                          T = 20,
#                          n = 100,
#                          lowse = 0.7,highse = 0.95,
#                          sea = 2,seb = 2,
#                          lowsp = 0.8,highsp = 1.0,
#                          spa = 4,
#                          spb = 6,
#                          ngrid = 25,
#                          coverage = 0.9)
# result$mode # 0.115
# result$interval # 0.011 0.226
# plot(result$theta,result$post,type="l",xlab="theta",ylab="p(theta)")
