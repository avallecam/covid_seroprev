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



#' 
#' inspiracion
#' 
#' https://github.com/gergness/srvyr/issues/13
#' solution: https://github.com/gergness/srvyr/issues/13#issuecomment-321407979
#' 
#' ejemplo
#' 
# library(tidyverse)
# library(srvyr)
# library(survey)
# data(api)
# dstrata <- apistrat %>% as_survey_design(strata = stype, weights = pw)
# dstrata2 <- apistrat %>% 
#   mutate(pw2=1) %>% 
#   as_survey_design(strata = stype, weights = pw2)
# dstrata %>% 
#   summarise(pct = survey_mean(awards=="Yes",proportion = TRUE))
# dstrata2 %>% 
#   summarise(pct = survey_mean(awards=="Yes",proportion = TRUE))
#' 
srvyr_prop_step_01 <- function(design,numerator,denominator) {
  
  c_var <- enquo(denominator)
  c_var_name <- c_var %>% rlang::as_name()
  
  d_var <- enquo(design)
  d_var_name <- d_var %>% rlang::as_name()
  
  n_var <- enquo(numerator)
  n_var_name <- n_var %>% rlang::as_name()
  
  num_levels <- unique(design %>% as_tibble() %>% pull({{numerator}})) %>%
    enframe(name = NULL,value = "numerator_level") %>% 
    mutate(design=list(design)) %>% 
    mutate(denominator=c_var_name,
           # design=d_var_name,
           numerator=n_var_name) %>%
    mutate(
      denominator=map(denominator,dplyr::sym),
      # design=map(design,dplyr::sym),
      numerator=map(numerator,dplyr::sym)
    ) %>% 
    filter(!is.na(numerator_level))
  
  return(num_levels)
}

# srvyr_prop_step_01(design = dstrata,
#                      numerator = awards,
#                      denominator = stype)

srvyr_prop_step_02 <- function(design,
                               numerator,
                               denominator,
                               numerator_level) {
  
  design %>% 
    filter(!is.na({{numerator}})) %>%
    filter(!is.na({{denominator}})) %>%
    group_by({{denominator}}) %>%
    summarize(
      prop = survey_mean({{numerator}} == numerator_level, 
                         proportion = TRUE,
                         prop_method = "logit",
                         vartype = c("ci","cv","se")
      ),
      total = survey_total({{numerator}} == numerator_level, 
                           # proportion = TRUE,
                           # prop_method = "logit",
                           deff = TRUE,
                           vartype = c("ci","cv","se"))#,
      # n = unweighted(n()),
      # nx = unweighted(length(na.omit({{numerator}})))
      ) %>%
    ungroup() %>%
    rename_at(.vars = vars(1),
              .funs = str_replace,"(.+)","denominator_level")
  # mutate(awards = cn)
}

# srvyr_prop_step_01(design = dstrata,
#                      numerator = awards,
#                      denominator = stype) %>% 
#   mutate(resultado=pmap(.l = select(.,design=design,
#                                     numerator = numerator,
#                                     denominator = denominator,
#                                     numerator_level=numerator_level),
#                        .f = srvyr_prop_step_02)) %>% 
#   unnest(resultado)

srvyr_prop_step_03 <- function(design,
                               numerator,
                               denominator) {
  # create the raw estimates
  design %>% 
    as_tibble() %>% 
    group_by({{denominator}},{{numerator}}) %>% 
    summarise(n=n()) %>% 
    ungroup() %>% 
    group_by({{denominator}}) %>% 
    mutate(
      p = prop.table(n),
      t = sum(n)#,
      # sum_total = sum(total)
    ) %>%
    ungroup()
}

# srvyr_prop_step_01(design = dstrata,
#                      numerator = awards,
#                      denominator = stype) %>%
#   mutate(resultado=pmap(.l = select(.,design=design,
#                                     numerator = numerator,
#                                     denominator = denominator,
#                                     numerator_level=numerator_level),
#                        .f = srvyr_prop_step_02)) %>%
#   unnest(resultado) %>% 
#   mutate(crudo=pmap(.l = select(.,design=design,
#                                 numerator=numerator,
#                                 denominator=denominator),
#                     .f = srvyr_prop_step_03)) %>% 
#   unnest(crudo) %>% 
#   select(-design:-numerator) %>% 
#   filter(numerator_level==awards & denominator_level==stype)

cdc_survey_proportion <- function(design,numerator,denominator) {
  srvyr_prop_step_01({{design}},
                       {{numerator}},
                       {{denominator}}) %>% 
    
    # estimate proportion using sampling weight
    mutate(resultado=pmap(.l = select(.,design=design,
                                      numerator = numerator,
                                      denominator = denominator,
                                      numerator_level=numerator_level),
                          .f = srvyr_prop_step_02)) %>% 
    unnest(resultado) %>% 
    
    # recover the denominator for each estimate
    group_by(denominator_level) %>%
    mutate(
      total_den = 1*total/prop,
      total_den_low = 1*total_low/prop_low,
      total_den_upp = 1*total_upp/prop_upp
    ) %>%
    ungroup() %>% 
    
    # make raw unweighted estimates  
    mutate(crudo=pmap(.l = select(.,design=design,
                                  numerator=numerator,
                                  denominator=denominator),
                      .f = srvyr_prop_step_03)) %>% 
    unnest(crudo) %>% 
    filter(numerator_level=={{numerator}} & denominator_level=={{denominator}}) %>% 
    select(-{{numerator}},-{{denominator}}) %>% 
    
    # wrangling
    select(-design) %>% 
    mutate_if(.predicate = is.list,.funs = as.character) %>% 
    select(denominator,denominator_level,numerator,numerator_level,everything()) %>% 
    arrange(denominator_level,numerator_level) %>% 
    
    # exact binomial test for raw uncertainty
    rename(raw_den=t,raw_num=n) %>% 
    mutate(raw=pmap(.l = select(.,x=raw_num,n=raw_den),
                    .f = binom.test),
           raw=map(.x = raw,.f = broom::tidy)) %>% 
    unnest(raw) %>% 
    select(-statistic,-p.value,-parameter,-method,-alternative,-p) %>% 
    rename(raw_prop=estimate,
           raw_prop_low=conf.low,
           raw_prop_upp=conf.high)
}

# cdc_survey_proportion(design = dstrata,
#                       numerator = awards,
#                       denominator = stype)

