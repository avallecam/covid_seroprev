library(tidyverse)
library(skimr)

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
                  p90 = ~ quantile(., probs = .90)), 
    append = FALSE)
  
  result_sum <- result %>% 
    my_skim() %>% 
    as_tibble() %>% 
    filter(skim_variable=="r1") %>% 
    select(skim_variable,numeric.p05:numeric.p90)
  
  output <- tibble(
    posterior=list(result),
    summary=list(result_sum)
  )
}

#' tidy_result <- seroprevalence_posterior(positive_number_test = 16,
#'                                         total_number_test = 16+84,
#'                                         sensibility = 0.93,
#'                                         specificity = 0.975)
#' 
#' tidy_result %>% 
#'   select(summary) %>% 
#'   unnest(cols = c(summary)) %>% 
#'   mutate_if(.predicate = is.numeric,.funs = ~round(.x,4)) %>% 
#'   mutate_all(.funs = as.character)
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
#'   geom_vline(aes(xintercept=result_sum %>% pull(numeric.p90)),color="red") +
#'   scale_x_continuous(breaks = scales::pretty_breaks())
