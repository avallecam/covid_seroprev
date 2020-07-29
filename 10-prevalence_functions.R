library(tidyverse)

tidy_srvyr_tibble <- function(data) {
  data %>% 
    mutate(covariate=colnames(.)[1]) %>% 
    select(covariate,everything()) %>% 
    rename_at(.vars = vars(2),
              .funs = str_replace,"(.+)","category")
}

cdc_srvyr_tibble_02 <- function(data) {
  data %>% 
    mutate(covariate=if_else(is.na(covariate),colnames(.)[1],covariate)) %>% 
    mutate(outcome=colnames(.)[1]) %>% 
    mutate(category=if_else(is.na(category),"overall",category)) %>% 
    select_if(.predicate = negate(is.factor)) %>% 
    select(covariate,category,outcome,everything())
}

cdc_srvyr_tibble_03 <- function(data) {
  data %>% 
    mutate(covariate=if_else(is.na(covariate),colnames(.)[1],covariate)) %>% 
    select(-covariate,covariate) %>% 
    select(-category,category) %>% 
    mutate(outcome=colnames(.)[1]) %>% 
    # mutate(category=if_else(is.na(category),"overall",category)) %>% 
    select(-1) %>% 
    # select_if(.predicate = negate(is.factor)) %>% 
    select(covariate,category,outcome,everything())
}

cdc_srvyr_create_table <- function(data,
                                   estim_digits=3,
                                   cilow_digits=2,
                                   ciupp_digits=3) {
  data %>% 
    mutate_if(.predicate = is.numeric,
              .funs = funs("tab"=.*100)) %>% 
    # mutate_at(.vars = vars(,),.funs = format,digits=3) %>%
    mutate_at(.vars = vars(proportion_tab),.funs = format,digits=estim_digits) %>% 
    mutate_at(.vars = vars(proportion_low_tab),.funs = format,digits=cilow_digits) %>% 
    mutate_at(.vars = vars(proportion_upp_tab),.funs = format,digits=ciupp_digits) %>% 
    mutate_at(.vars = vars(proportion_cv_tab),.funs = format,digits=2) %>%
    mutate(prevalence=str_c(proportion_tab,"%\n(",proportion_low_tab," - ",proportion_upp_tab,")")) %>%
    mutate(prevalence_tab=str_c(proportion_tab,"% (",proportion_low_tab,"-",proportion_upp_tab,")")) %>%
    mutate(cv=str_c(proportion_cv_tab,"%")) #%>%
  # select(-starts_with("proportion"),-ig_clasificacion)
}

cdc_srvyr_create_table_02 <- function(data,
                                      proportion_tab=proportion_tab,
                                      proportion_low_tab=proportion_low_tab,
                                      proportion_upp_tab=proportion_upp_tab,
                                      estim_digits=3,
                                      cilow_digits=2,
                                      ciupp_digits=3) {
  data %>% 
    mutate_if(.predicate = is.numeric,
              .funs = funs("tab"=.*100)) %>% 
    # mutate_at(.vars = vars(,),.funs = format,digits=3) %>%
    mutate_at(.vars = vars({{proportion_tab}}),.funs = format,digits=estim_digits) %>% 
    mutate_at(.vars = vars({{proportion_low_tab}}),.funs = format,digits=cilow_digits) %>% 
    mutate_at(.vars = vars({{proportion_upp_tab}}),.funs = format,digits=ciupp_digits) %>% 
    # mutate_at(.vars = vars(proportion_cv_tab),.funs = format,digits=2) %>%
    # mutate(prevalence=str_c(proportion_tab,"%\n(",proportion_low_tab,"-",proportion_upp_tab,")")) %>%
    mutate(prevalence_tab=str_c({{proportion_tab}},"% (",{{proportion_low_tab}},"-",{{proportion_upp_tab}},")")) #%>%
  # mutate(cv=str_c(proportion_cv_tab,"%")) #%>%
  # select(-starts_with("proportion"),-ig_clasificacion)
}

ggplot_prevalence <- function(data) {
  data %>% 
    ggplot(aes(x = category,y = proportion,color=outcome,group=outcome)) +
    # geom_point(aes(size=proportion_cv),position = position_dodge(width = 0.5)) +
    geom_point(position = position_dodge(width = 0.5)) +
    geom_errorbar(aes(max=proportion_upp,min=proportion_low),position = position_dodge(width = 0.5)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                       breaks = scales::pretty_breaks(n = 10)) +
    # scale_size_continuous(labels = scales::percent_format()) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
}
