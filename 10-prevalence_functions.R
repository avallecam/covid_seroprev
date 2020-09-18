library(tidyverse)

# create output tables ----------------------------------------------------

tidy_srvyr_tibble <- function(data) {
  data %>% 
    mutate(covariate=colnames(.)[1]) %>% 
    select(covariate,everything()) %>% 
    rename_at(.vars = vars(2),
              .funs = str_replace,"(.+)","category")
}

cdc_srvyr_tibble_02 <- function(data,colname_number=1) {
  data %>% 
    mutate(covariate=if_else(is.na(covariate),colnames(.)[colname_number],covariate)) %>% 
    mutate(outcome=colnames(.)[colname_number]) %>% 
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

# generate prevalence plots -----------------------------------------------


# trials to use srvyr with purrr ------------------------------------------

cdc_srvyr_prevalence_outcome <- function(design,outcome) {
  design %>%
    # filter(!is.na({{covariate}})) %>% 
    # group_by({{covariate}},{{outcome}}) %>% #group_by
    group_by({{outcome}}) %>% #group_by
    summarize(proportion = survey_mean(vartype = c("ci","cv","se"),
                                       # proportion = TRUE,
                                       deff = TRUE,
                                       prop_method = "logit"
                                       ),
              total = survey_total(vartype = c("ci","cv","se")),
              n = unweighted(n())
    ) %>% 
    # group_by({{covariate}}) %>% #group_by
    mutate(p = prop.table(n),
           t = sum(n),
           sum_total = sum(total)) %>% 
    ungroup() %>% 
    filter({{outcome}}=="positivo")
}


cdc_srvyr_prevalence_one_covariate <- function(design,covariate,outcome) {
  design %>%
    filter(!is.na({{covariate}})) %>% 
    group_by({{covariate}},{{outcome}}) %>% #group_by
    summarize(proportion = survey_mean(vartype = c("ci","cv"),
                                       #proportion = TRUE,
                                       deff = TRUE,
                                       prop_method = "logit"
                                       ),
              total = survey_total(vartype = c("ci","cv")),
              n = unweighted(n())
    ) %>% 
    group_by({{covariate}}) %>% #group_by
    mutate(p = prop.table(n),
           t = sum(n),
           sum_total = sum(total)) %>% 
    ungroup() %>% 
    filter({{outcome}}=="positivo")
}

cdc_srvyr_prevalence_numerator_denominator <- function(design,denominator,numerator) {
  design %>%
    filter(!is.na({{numerator}})) %>% 
    filter(!is.na({{denominator}})) %>% 
    group_by({{denominator}},{{numerator}}) %>% #group_by
    summarize(proportion = survey_mean(vartype = c("ci","cv"),
                                       #proportion = TRUE,
                                       deff = TRUE,
                                       prop_method = "logit"
                                       ),
              total = survey_total(vartype = c("ci","cv")),
              n = unweighted(n())
    ) %>% 
    group_by({{denominator}}) %>% #group_by
    mutate(p = prop.table(n),
           t = sum(n),
           sum_total = sum(total)) %>% 
    ungroup() #%>% 
    # filter({{numerator}}=="positivo")
}

# example
# cdc_srvyr_prevalence_numerator_denominator(design = design,
#                                            denominator = sintomas_cualquier_momento_cat,
#                                            numerator = ig_clasificacion) %>% 
#   select(-ends_with("_low"),-ends_with("_upp"),-ends_with("_cv"),-ends_with("_deff"))
# 
# cdc_srvyr_prevalence_numerator_denominator(design = design,
#                                            denominator = ig_clasificacion,
#                                            numerator = sintomas_cualquier_momento_cat) %>% 
#   glimpse()
# 
# cdc_srvyr_prevalence_numerator_denominator(design = design,
#                                            denominator = edad_decenios,
#                                            numerator = ig_clasificacion) %>% 
#   select(-ends_with("_cv"),-ends_with("_deff"))


# this goes to avallecam --------------------------------------------------

outcome_to_numeric <- function(variable) {
  as.numeric({{variable}})-1
}

# if quest1=No then quest2=NA ---------------------------------------------


riesgo_extend_na <- function(variable,referencia) {
  case_when(
    {{referencia}}=="no"~"0",
    TRUE~{{variable}})
}

# # reference
# uu_clean_data %>% 
#   select(riesgo,condicion_riesgo_diabetes) %>% 
#   count(riesgo,condicion_riesgo_diabetes)
# 
# # example
# uu_clean_data %>% 
#   select(riesgo,condicion_riesgo_diabetes) %>% 
#   # riesgo_extend(variable = condicion_riesgo_diabetes) %>% 
#   mutate(condicion_riesgo_diabetes=riesgo_extend_na(variable = condicion_riesgo_diabetes,
#                                                     referencia = riesgo)) %>% 
#   count(riesgo,condicion_riesgo_diabetes)


# proportion = TRUE -------------------------------------------------------





