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

# 01 rogan-glanden estimator 1978 --------------------------------------------
#' from
#' https://github.com/sakitakahashi/COVID-sensitivity
#' 
#' limitation
#' this allows values out of 0-1 range

## Function to adjust the observed prevalence for a single Se & Sp
rogan_gladen_estimator <- function(prev.obs, Se, Sp) {
  
  return((prev.obs+Sp-1)/(Se+Sp-1))
  
}

# assumes to generate results from independent studies
rogan_gladen_stderr_unk <- function(prev.obs, stderr.obs, prev.tru, Se, Sp, n_Se, n_Sp) {
  out <- (1/(Se+Sp-1))*sqrt((stderr.obs+((Se*(1-Se))/n_Se)+((Sp*(1-Sp))/n_Sp)*(1-prev.tru)^2))
}

# prop.test(x = 321,n = 321+1234) %>% broom::glance()
# binom.test(x = 321,n = 321+1234) %>% broom::glance()
# https://stackoverflow.com/questions/17802320/r-proportion-confidence-interval-factor
# https://stackoverflow.com/questions/21719578/confidence-interval-for-binomial-data-in-r

# # reproducible example 88
# 
# tibble(positive=positive_pop,
#        negative=negative_pop) %>% 
#   mutate(total=positive+negative,
#          prev_app=positive_pop/(positive_pop+negative_pop),
#          # assumes random sample from large population
#          stde_app=sqrt(prev_app * (1 - prev_app)/(total))) %>% 
#   mutate(prev_tru=rogan_gladen_estimator(prev.obs = prev_app,
#                                   Se = 0.90,
#                                   Sp = 0.76),
#          stde_tru=rogan_gladen_stderr_unk(prev.obs = prev_app,
#                                           prev.tru = prev_tru,
#                                           stderr.obs = stde_app,
#                                           Se = 0.90,
#                                           Sp = 0.76,
#                                           n_Se = 1586,
#                                           n_Sp = 1586)) %>% 
#   mutate(prev_tru_low=prev_tru-qnorm(0.975)*stde_tru,
#          prev_tru_upp=prev_tru+qnorm(0.975)*stde_tru)

# # reproducible example 00
# tibble(
#   g=1:2,
#   p=seq(10L,20L,10L),
#   n=seq(200L,100L,-100L),
#   se=seq(0.9,0.8,-0.1),
#   sp=seq(0.8,0.9,0.1)
# ) %>%
#   mutate(raw=p/n) %>%
#   mutate(adjust=pmap_dbl(.l = select(.,prev.obs=raw, Se=se, Sp=sp),.f = rogan_gladen_estimator))
#   # mutate(adjust=future_pmap_dbl(.l = select(.,prev.obs=raw, Se=se, Sp=sp),.f = rogan_gladen_estimator))

# . -------------------------------------------------------------------------
# . -------------------------------------------------------------------------

# 02 larremorre method 2020 --------------------------------------------------

# . -----------------------------------------------------------------------


# _ KNOWN performance -----------------------------------------------------------------


source("covid_serological_sampling.R")

# __ ONE-POP -----------------------------------------------------------------

# reproduce this
# https://github.com/LarremoreLab/covid_serological_sampling/blob/master/codebase/prevalence_onepopulation_workbook.ipynb

result_one <- sample_posterior_r_mcmc_hyperR(samps = 10000,
                                             posi = positive_pop[1],
                                             ni = negative_pop[1],
                                             # se = sensitivity,
                                             # sp = specificity,
                                             se = 0.977,
                                             sp = 0.986,
                                             gam0 = 150
)

result_one %>% 
  as_tibble()

result_one %>% 
  skim()

result_one %>% 
  as_tibble() %>% 
  ggplot(aes(x = r1)) +
  geom_histogram(aes(y=..density..),binwidth = 0.005)

# __ SUB-POPS --------------------------------------------------------------

# reproduce this
# https://github.com/LarremoreLab/covid_serological_sampling/blob/master/codebase/prevalence_subpopulations_workbook.ipynb

result_sub <- sample_posterior_r_mcmc_hyperR(samps = 10000,
                                         posi = positive_pop,
                                         ni = positive_pop+negative_pop,
                                         se = sensitivity,
                                         sp = specificity,
                                         # se = 0.977,
                                         # sp = 0.986,
                                         gam0 = 150
                                         )

result_sub %>% 
  as_tibble()

result_sub %>% 
  skim()

result_sub %>% 
  as_tibble() %>% 
  rownames_to_column() %>% 
  select(-gam) %>% 
  pivot_longer(cols = -rowname,names_to = "estimates",values_to = "values") %>%
  ggplot(aes(x = values, color = estimates)) +
  geom_density()
  # ggplot() +
  # geom_density(aes(x = r)) +
  # geom_density(aes(x = r1)) +
  # geom_density(aes(x = r2)) +
  # geom_density(aes(x = r3)) +
  # geom_density(aes(x = r4))

# . -----------------------------------------------------------------------


# __ custom functions -----------------------------------------------------

seroprevalence_posterior <- function(positive_number_test,
                                     total_number_test,
                                     sensitivity,
                                     specificity) {
  
  posi <- positive_number_test
  ni <- total_number_test
  se <- sensitivity
  sp <- specificity
  
  result <- sample_posterior_r_mcmc_hyperR(samps = 10000,
                                           # posi = 321,ni = 321+1234,
                                           # posi = 16,ni = 16+84,
                                           # se = 0.93,sp = 0.975,
                                           posi = posi,ni = ni,
                                           se = se,sp = sp,
                                           gam0 = 150 # value from paper # hyperprior variance parameter - https://github.com/LarremoreLab/covid_serological_sampling/blob/20122a214bdd5416f92eec1b831087de3409232c/codebase/seroprevalence.py#L191
  ) %>% 
    as_tibble()
  
  my_skim <- skim_with(
    numeric = sfl(p05 = ~ quantile(., probs = .05), # 90% credibility interval
                  mean = mean,
                  p95 = ~ quantile(., probs = .95)), 
    append = FALSE)
  
  result_sum <- result %>% 
    my_skim() %>% 
    as_tibble() %>% 
    filter(skim_variable=="r1") %>% 
    select(skim_variable,numeric.p05:numeric.p95)
  
  output <- tibble(
    posterior=list(result),
    summary=list(result_sum)
  )
}

cdc_srvyr_create_table_free <- function(data,
                                        estim_var,
                                        cilow_var,
                                        ciupp_var,
                                        estim_digits=3,
                                        cilow_digits=2,
                                        ciupp_digits=3) {
  data %>% 
    mutate(estim_tab={{estim_var}},
           cilow_tab={{cilow_var}},
           ciupp_tab={{ciupp_var}}) %>% 
    mutate_at(.vars = vars(estim_tab,cilow_tab,ciupp_tab),
              .funs = funs(.*100)) %>% 
    # mutate_at(.vars = vars(,),.funs = format,digits=3) %>%
    mutate_at(.vars = vars(estim_tab),.funs = format,digits=estim_digits) %>% 
    mutate_at(.vars = vars(cilow_tab),.funs = format,digits=cilow_digits) %>% 
    mutate_at(.vars = vars(ciupp_tab),.funs = format,digits=ciupp_digits) %>% 
    # mutate_at(.vars = vars(proportion_cv_tab),.funs = format,digits=2) %>%
    # mutate(prevalence=str_c(estim_tab,"%\n(",cilow_tab," - ",ciupp_tab,")")) %>%
    mutate(fused_tab=str_c(estim_tab,"% (",cilow_tab,"-",ciupp_tab,")")) #%>%
  # mutate(cv=str_c(proportion_cv_tab,"%")) #%>%
  # select(-starts_with("proportion"),-ig_clasificacion)
}

# # reproducible example 01
# tidy_result <- seroprevalence_posterior(positive_number_test = positive_pop[1],
#                                         total_number_test = positive_pop[1]+negative_pop[1],
#                                         # sensitivity = 1,specificity = 1
#                                         sensitivity = 0.93,
#                                         specificity = 0.975
#                                         )
# 
# tidy_result_out <- 
#   tidy_result %>%
#   select(summary) %>%
#   unnest(cols = c(summary)) %>%
#   cdc_srvyr_create_table_free(estim_var = numeric.mean,
#                               cilow_var = numeric.p05,
#                               ciupp_var = numeric.p95,
#                               estim_digits = 4,
#                               cilow_digits = 3,
#                               ciupp_digits = 3) %>%
#   # select(estim_tab:fused_tab) %>% 
#   print()
# 
# # posterior distribution
# tidy_result %>%
#   select(posterior) %>%
#   unnest(cols = c(posterior)) %>%
#   ggplot(aes(x = r1)) +
#   geom_histogram(aes(y=..density..),binwidth = 0.005) +
#   geom_density() +
#   geom_vline(aes(xintercept=tidy_result_out %>% pull(numeric.mean)),color="red",lwd=1) +
#   geom_vline(aes(xintercept=tidy_result_out %>% pull(numeric.p05)),color="red") +
#   geom_vline(aes(xintercept=tidy_result_out %>% pull(numeric.p95)),color="red") +
#   scale_x_continuous(breaks = scales::pretty_breaks())

# # reproducible example 02
# # plan(sequential)
# plan(multisession, workers = availableCores())
# tic()
# result <- tibble(
#   g=1:2,
#   p=seq(10L,20L,10L),
#   n=seq(200L,100L,-100L),
#   se=seq(0.9,0.8,-0.1),
#   sp=seq(0.8,0.9,0.1)
# ) %>%
#   # mutate(fix=pmap(.l = select(.,
#   mutate(fix=future_pmap(.l = select(.,
#                               positive_number_test=p,
#                               total_number_test=n,
#                               sensitivity=se,
#                               specificity=sp),
#                   .f = possibly(seroprevalence_posterior,otherwise = NA_real_)))
# toc()
# 
# result %>%
#   unnest(fix) %>%
#   unnest(summary) %>%
#   mutate(raw=p/n) %>%
#   cdc_srvyr_create_table_free(estim_var = numeric.mean,
#                               cilow_var = numeric.p05,
#                               ciupp_var = numeric.p95,
#                               estim_digits = 2,
#                               cilow_digits = 3,
#                               ciupp_digits = 3) %>%
#   select(estim_tab:fused_tab)


# . -----------------------------------------------------------------------


# _ UNKNOWN performance ---------------------------------------------------------------

# # reproducible example 04
# result_unk <- sample_posterior_r_mcmc_testun(samps = 10000,
#                                            #in population
#                                            pos = positive_pop[1], #positive
#                                            n = negative_pop[1], #negatives
#                                            # in lab
#                                            # tp = 30,tn = 50,fp = 0,fn = 0
#                                            tp = 670,tn = 640,fp = 202,fn = 74
# )

# # reproducible example YY
# 
# result_unk %>%
#   as_tibble() %>%
#   skim()
# 
# result_unk %>%
#   as_tibble() %>%
#     ggplot(aes(x = r)) +
#     geom_histogram(aes(y=..density..),binwidth = 0.005) +
#     geom_density()
# 
# result_unk %>%
#   as_tibble() %>%
#   rownames_to_column() %>%
#   pivot_longer(cols = -rowname,names_to = "estimates",values_to = "values") %>%
#   ggplot(aes(x = values)) +
#   geom_histogram(aes(y=..density..),binwidth = 0.005) +
#   geom_density() +
#   facet_grid(~estimates,scales = "free_x")

seroprevalence_posterior_unk <- function(positive_number_test,
                                     total_number_test,
                                     true_positive,
                                     true_negative,
                                     false_positive,
                                     false_negative) {
  
  negative_number_test <- total_number_test - positive_number_test
  
  pos <- positive_number_test
  neg <- negative_number_test
  tp <- true_positive
  tn <- true_negative
  fp <- false_positive
  fn <- false_negative
  
  result <- sample_posterior_r_mcmc_testun(samps = 10000,
                                           #in population
                                           pos = pos, #positive
                                           n = neg, #negatives
                                           # in lab
                                           tp = tp,tn = tn,
                                           fp = fp,fn = fn
  ) %>% 
    as_tibble()
  
  my_skim <- skim_with(
    numeric = sfl(p05 = ~ quantile(., probs = .05), # 90% credibility interval
                  mean = mean,
                  p50 = ~ quantile(., probs = .50),
                  p95 = ~ quantile(., probs = .95)), 
    append = FALSE)
  
  result_sum <- result %>% 
    my_skim() %>% 
    as_tibble() %>% 
    filter(skim_variable=="r") %>% 
    select(skim_variable,numeric.p05:numeric.p95)
  
  performance_sum <- result %>% 
    my_skim() %>% 
    as_tibble() %>% 
    filter(!(skim_variable=="r")) %>% 
    select(skim_variable,numeric.p05:numeric.p95)
  
  output <- tibble(
    posterior=list(result),
    summary=list(result_sum),
    performance=list(performance_sum)
  )
}

# # reproducible example xx
# 
# result_unk_x <- seroprevalence_posterior_unk(positive_number_test = positive_pop[1],
#                                              total_number_test = positive_pop[1]+negative_pop[1],
#                                              true_positive = 670,
#                                              true_negative = 640,
#                                              false_positive = 202,
#                                              false_negative = 74)
# 
# result_unk_x %>%
#   unnest(summary)
# result_unk_x %>%
#   unnest(performance)
# 
# result_unk_x %>% 
#   unnest(posterior) %>%
#   as_tibble() %>%
#   rownames_to_column() %>%
#   select(-summary) %>% 
#   pivot_longer(cols = -rowname,names_to = "estimates",values_to = "values") %>%
#   ggplot(aes(x = values)) +
#   geom_histogram(aes(y=..density..),binwidth = 0.005) +
#   geom_density() +
#   facet_grid(~estimates,scales = "free_x")

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
