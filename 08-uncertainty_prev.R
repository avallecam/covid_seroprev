library(tidyverse)
library(skimr)

set.seed(33)

seroprevalence_posterior <- function(positive_number_test,
                                     total_number_test,
                                     sensibility,
                                     specificity) {
  
  source("covid_serological_sampling.R")
  
  posi <- positive_number_test
  ni <- total_number_test
  se <- sensibility
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
    numeric = sfl(p05 = ~ quantile(., probs = .05), 
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

# tidy_result <- seroprevalence_posterior(positive_number_test = 16,
#                                         total_number_test = 16+84,
#                                         # sensibility = 1,specificity = 1
#                                         sensibility = 0.93,
#                                         specificity = 0.975
#                                         )
# 
# tidy_result %>%
#   select(summary) %>%
#   unnest(cols = c(summary)) %>%
#   # mutate_if(.predicate = is.numeric,.funs = ~round(.x,4)) %>%
#   # mutate_all(.funs = as.character) %>% 
#   cdc_srvyr_create_table_free(estim_var = numeric.mean,
#                               cilow_var = numeric.p05,
#                               ciupp_var = numeric.p95,
#                               estim_digits = 4,
#                               cilow_digits = 3,
#                               ciupp_digits = 3) %>% 
#   select(estim_tab:fused_tab)
#' 
#' # posterior distribution
#' tidy_result %>% 
#'   select(posterior) %>% 
#'   unnest(cols = c(posterior)) %>% 
#'   ggplot(aes(x = r1)) +
#'   geom_histogram(aes(y=..density..),binwidth = 0.005) +
#'   geom_density() +
#'   geom_vline(aes(xintercept=result_sum %>% pull(numeric.mean)),color="red",lwd=1) +
#'   geom_vline(aes(xintercept=result_sum %>% pull(numeric.p05)),color="red") +
#'   geom_vline(aes(xintercept=result_sum %>% pull(numeric.p95)),color="red") +
#'   scale_x_continuous(breaks = scales::pretty_breaks())

# result <- tibble(
#   g=1:9,
#   p=seq(10,90,10),
#   n=seq(900,100,-100),
#   se=seq(0.9,0.1,-0.1),
#   sp=seq(0.1,0.9,0.1)
# ) %>% 
#   # group_by(g) %>% 
#   # nest()
#   # slice(1) %>% 
#   mutate(fix=pmap(.l = select(.,
#                               positive_number_test=p,
#                               total_number_test=n,
#                               sensibility=se,
#                               specificity=sp),
#                   .f = possibly(seroprevalence_posterior,otherwise = NA_real_)))
# 
# result %>% 
#   unnest(fix) %>% 
#   unnest(summary) %>% 
#   mutate(raw=p/n) %>% 
#   cdc_srvyr_create_table_free(estim_var = numeric.mean,
#                               cilow_var = numeric.p05,
#                               ciupp_var = numeric.p95,
#                               estim_digits = 4,
#                               cilow_digits = 3,
#                               ciupp_digits = 4) %>% 
#   select(estim_tab:fused_tab)
