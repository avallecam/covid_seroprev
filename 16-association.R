#'
#' OBJETIVES:
#' - create association estimates with survey design
#' - table and figure output
#' write_rds("data/____")
#' write_xlsx("table/____")
#' ggsave("figure/____")
#' 
#' PENDIENTE
#' - for ind_hacin: sub-analysis comparing general characteristics of missings with non-missings
#' - finish figure
#' 
#' DETAILS
#' - % of missing distribution

library(tidyverse)
library(skimr)
library(survey)
library(srvyr)
# library(purrr)
# library(furrr)
# library(writexl)
# library(tictoc)

theme_set(theme_bw())


# functions ---------------------------------------------------------------

# source("10-prevalence_functions.R")

# source("08-uncertainty_prev.R")

outcome_to_numeric <- function(variable) {
  as.numeric({{variable}})-1
}

library(serosurvey)

set.seed(33)

# inputs ------------------------------------------------------------------

uu_clean_data <- read_rds("data/uu_clean_data.rds") %>% 
  mutate(survey_all="survey_all",
         weight_nul=1) %>% 
  # transformar a factor (prevlaencia ajustada)
  mutate_at(.vars = vars(igg,igm,ig_clasificacion,positividad_peru),
            .funs = as.factor) %>% 
  # transformar a numerico (prevalencia cruda)
  mutate_at(.vars = vars(igg,igm,ig_clasificacion,positividad_peru),
            .funs = list("num"=outcome_to_numeric)) %>%
  mutate(contacto_covid=fct_relevel(contacto_covid,"no")) %>%
  mutate(contacto_covid_tipo=fct_relevel(contacto_covid_tipo,"no")) %>% 
  mutate(diris=fct_relevel(diris,"DIRIS CENTRO")) %>% 
  mutate(nm_prov=fct_relevel(nm_prov,"lima")) %>% 
  mutate(rubro=case_when(
    is.na(rubro) & trabajo_reciente=="no" ~"no",
    TRUE~rubro
  )) %>% 
  mutate(rubro=fct_relevel(rubro,"no")) %>%
  mutate(etnia_cat2=fct_collapse(etnia_cat,
                                 "otros_afro"=c("otros","afro"))) %>% 
  mutate(across(c(
    etnia_cat,
    etnia_cat2,
    seguro_salud,
    desague,
    agua#,
    #tipo_vivienda
  ),
  fct_infreq)) %>%
  identity()
# reordenar contacto_covid, contacto_covid_tipo
# # extender respuestas por condicicion de riesgo
# mutate_at(.vars = vars(starts_with("condicion_riesgo_")),
#           .funs = list("ext"=~riesgo_extend_na(variable = .x,referencia = riesgo))) #%>% 
# # de ord a fct
# mutate(edad_decenios=as.character(edad_decenios),
#        edad_decenios=as.factor(edad_decenios))

uu_clean_data %>% 
  count(ig_clasificacion,ig_clasificacion_num,positividad_peru) %>% 
  avallecam::print_inf()

# uu_clean_data %>% count(trabajo_reciente,motivo_no_trabajo, rubro, prof_salud)
# uu_clean_data %>% count(trabajo_reciente,rubro)

# uu_clean_data %>% skim(ind_hacin)
# uu_clean_data %>% count(sintomas_cualquier_momento_cat_fecha_14d_v1)

uu_clean_data %>% 
  janitor::tabyl(nse_estrato_cat,etnia_cat) %>% 
  avallecam::adorn_ame()
  # janitor::chisq.test() %>% 
  # broom::tidy()

# ___________ -------------------------------------------------------------

# SELECT COVARIATES -------------------------------------------------------

covariate_set01 <- uu_clean_data %>% #NEWMOD
  select(survey_all,
         sexo,
         edad_etapas_de_vida_t,
         # edad_decenios,
         # edad_quinquenal,
         # nse_estrato,
         nse_estrato_cat,
         diris,
         # pobreza_dico,
         ind_hacin,
         # ind_hacin_qrt,
         ind_hacin_cut2,
         ind_hacin_cat,
         hacinamiento,
         # nro_dormitorios_cat,
         nm_prov,
         sintomas_cualquier_momento_cat,
         sintomas_cualquier_momento_cat_fecha_14d_v1,
         # sintomas_cualquier_momento_cat_fecha_rangos,
         # riesgo,
         # ends_with("_ext"),
         # -contains("ninguna"),
         # -contains("otro"),
         # -contains("salud"),
         # -contains("renal"),
         # -contains("60a"),
         contacto_covid,
         contacto_covid_tipo,
         prueba_previa,
         prueba_previa_cat,
         prueba_previa_res,
         etnia_cat,
         etnia_cat2,
         trabajo_reciente,
         rubro,
         # atencion,
         # seguro_salud,
  ) %>% 
  colnames()



# ____________ ------------------------------------------------------------


# DESCRIPTIVO -------------------------------------------------------------



# __ valores perdidos -----------------------------------------------------

uu_clean_data %>% 
  select(all_of(covariate_set01)) %>% 
  naniar::miss_var_summary() %>% 
  avallecam::print_inf()


# __ descripción poblacional -------------------------------------------------

uu_clean_data %>% 
  select(ig_clasificacion,all_of(covariate_set01)) %>%
  compareGroups::compareGroups(ig_clasificacion~.,
                               # compareGroups::compareGroups(survey_all~.,
                               # include.miss = T,
                               data = .,
                               method = c(ind_hacin=2),
                               max.xlev = 30,
                               chisq.test.perm = TRUE,
                               byrow = T
  ) %>%
  compareGroups::createTable(digits = 1,
                             show.all = T,
                             sd.type = 2,
                             show.p.overall = T,
                             # show.ratio = T,
                             show.n = T) %>%
  compareGroups::export2xls("table/04-seroprev-supp-table11.xls")
# compareGroups::export2xls("table/02-seroprev-supp-table01-a.xls")




# ____________ ------------------------------------------------------------

# SVY DESIGN ---------------------------------------------------------

# tratamiento de stratos con un solo conglomerado
options(survey.lonely.psu = "certainty")

uu_clean_data %>% count(CONGLOMERADO,VIVIENDA)

# diseño muestral de la encuesta ---------------------------------

design <- uu_clean_data %>% 
  
  filter(!is.na(ig_clasificacion)) %>% #CRITICAL! ON OUTCOME
  filter(!is.na(factorfinal)) %>% #NO DEBEN DE HABER CONGLOMERADOS SIN WEIGHT
  
  as_survey_design(c(CONGLOMERADO, VIVIENDA),
                   #id = CONGLOMERADO, #clusters or psu (primary sampling unit) 
                   #add vivienda como *SSU*
                   strata = ESTRATO, #clusters need to be nested in the strata
                   weights= PONDERACION # factores de expancion
  )


# ______________ ----------------------------------------------------------

# SVY GLM ---------------------------------------------------------

# models ------------------------------------------------------------------

library(broom)
library(epitidy)

# null model --------------------------------------------------------------

uu_clean_data %>% pull(ig_clasificacion_num) %>% mean()

glm_null <- svyglm(ig_clasificacion_num ~ 1, 
                   design = design,
                   # family = quasibinomial(link = "log")#,
                   family = poisson(link = "log")#,
                   # na.action = na.exclude
                   )

glm_null %>% epi_tidymodel_pr()

# # _ simple regression -----------------------------------------------------
# 
# # ?survey::svyglm
# 
# my_model <- svyglm(ig_clasificacion_num ~ hacinamiento,
#                    design,
#                    family = poisson(link = "log"))
# # summary(my_model)
# # tidy(my_model)
# epitidy::epi_tidymodel_pr(my_model)
# my_model <- survey::svyglm(ig_clasificacion_num ~ hacinamiento, design)
# epitidy::epi_tidymodel_pr(my_model)

# __ more than one simple model ------------------------------------------------------------

# crear matriz
# set 01 of denominator-numerator
simple_models <- expand_grid(
  design=list(design),
  # denominator=covariate_set01[c(1,5)],
  denominator=covariate_set01[-c(1)],
  numerator=c("ig_clasificacion_num")
  ) %>%
  # crear simbolos
  mutate(
    denominator=map(denominator,dplyr::sym),
    numerator=map(numerator,dplyr::sym)
  ) %>% 
  #purrr::map
  #create symbol, update null model, tidy up the results
  mutate(variable=map(denominator,dplyr::sym),
         simple_rawm=map(.x = variable, 
                         .f = epi_tidymodel_up, 
                         reference_model = glm_null),
         simple_tidy=map(.x = simple_rawm, 
                         .f = epi_tidymodel_pr,
                         digits = 5)
  ) %>%
  #unnest coefficients
  unnest(cols = c(simple_tidy)) %>%
  #filter out intercepts
  filter(term!="(Intercept)")

simple_models %>% 
  select(-(1:5)) %>% 
  avallecam::print_inf()



# _ 01 multiple regression ----------------------------------------------------------

# __ define confounder set ---------------

counfunder_set01 <- c("sexo","edad_etapas_de_vida_t","nm_prov","nse_estrato_cat") #NEWMOD #nse_estrato_cat

glm_adjusted01 <- 
  epi_tidymodel_up(reference_model = glm_null,
                   variable = dplyr::sym(counfunder_set01[1])) %>% 
  epi_tidymodel_up(variable = dplyr::sym(counfunder_set01[2])) %>% 
  epi_tidymodel_up(variable = dplyr::sym(counfunder_set01[3])) %>% 
  epi_tidymodel_up(variable = dplyr::sym(counfunder_set01[4])) #NEWMOD

glm_adjusted01 %>% epi_tidymodel_pr()

# _ final table -----------------------------------------------------------

final_table01 <- 
  glm_adjusted01 %>%
  epi_tidymodel_pr() %>% 
  # round numeric values
  mutate_at(.vars = vars(pr,conf.low,conf.high),
            .funs = round, digits=2) %>%
  # mutate_at(.vars = vars(p.value.s,p.value.m),
  #           .funs = round, digits=3) %>%
  #join confidence intervals
  mutate(ci=str_c(conf.low," - ",conf.high)) %>%
  #remove and reorder columns
  select(starts_with("value"),term,
         starts_with("pr"),starts_with("ci"),starts_with("p.val"),
         -starts_with("conf")) %>%
  # select(starts_with("value"),term,ends_with(".s"),ends_with(".m")) %>%
  # select(-value.m) %>%
  #add ref to estimates
  # mutate(pr.s=if_else(str_detect(term,".ref"),"Ref.",as.character(pr.s)),
         # pr.m=if_else(str_detect(term,".ref"),"Ref.",as.character(pr.m))) %>%
  # ungroup() %>% 
  # transform p value columns
  mutate(p.value=case_when(
    p.value>=0.001 ~ as.character(round(p.value,digits=3)),
    p.value<0.001 ~ "<0.001"
  ))

final_table01 %>% 
  avallecam::print_inf()




# _ 02 multiple regression ----------------------------------------------------------

# __ define confounder set ---------------

counfunder_set <- c("sexo","edad_etapas_de_vida_t","diris","nse_estrato_cat") #NEWMOD nse_estrato_cat

glm_adjusted <- 
  epi_tidymodel_up(reference_model = glm_null,
                   variable = dplyr::sym(counfunder_set[1])) %>% 
  epi_tidymodel_up(variable = dplyr::sym(counfunder_set[2])) %>% 
  epi_tidymodel_up(variable = dplyr::sym(counfunder_set[3])) %>% 
  epi_tidymodel_up(variable = dplyr::sym(counfunder_set[4])) #NEWMOD

# __ more than one multiple models ------------

multiple_models <- expand_grid(
  design=list(design),
  # denominator=covariate_set01[c(1,5)],
  denominator=covariate_set01[-c(1)],
  numerator=c("ig_clasificacion_num")
) %>%
  # #remove unwanted covariates: adjustment set
  filter(!magrittr::is_in(denominator,counfunder_set)) %>% 
  # crear simbolos
  mutate(
    denominator=map(denominator,dplyr::sym),
    numerator=map(numerator,dplyr::sym)
  ) %>% 
  #purrr::map
  #create symbol, update null model, tidy up the results
  mutate(variable=map(denominator,dplyr::sym),
         multiple_rawm=map(.x = variable, 
                         .f = epi_tidymodel_up, 
                         reference_model = glm_adjusted),
         multiple_tidy=map(.x = multiple_rawm, 
                         .f = epi_tidymodel_pr,
                         digits = 5)
  ) %>%
  #unnest coefficients
  unnest(cols = c(multiple_tidy)) %>%
  #filter out intercepts
  filter(term!="(Intercept)") %>% 
  #remove confounders from estimated coefficients
  filter(!str_detect(term,counfunder_set[1])) %>% 
  filter(!str_detect(term,counfunder_set[2])) %>% 
  filter(!str_detect(term,counfunder_set[3])) %>% 
  filter(!str_detect(term,counfunder_set[4])) #NEWMOD

multiple_models %>% 
  select(-(1:5)) %>% 
  avallecam::print_inf()


# _ final table -----------------------------------------------------------

final_table <- simple_models %>%
  select(-c(1,3:5)) %>% 
  # select(-variable,-simple_rawm) %>%
  full_join(multiple_models %>% select(-c(1,3:5)),
            by = "term",
            suffix=c(".s",".m")) %>%
  mutate(across(contains("denominator"),as.character)) %>% 
  select(-denominator.m) %>% 
  rename(value.s=denominator.s) %>% 
  #filter(!is.na(p.value.m)) %>%
  #add to upper rows to add covariate name and reference category
  group_by(value.s) %>%
  nest() %>%
  mutate(data=map(.x = data,
                  .f = ~add_row(.data = .x,
                                term=".ref",
                                .before = 1)),
         data=map(.x = data,
                  .f = ~add_row(.data = .x,
                                term=".name",
                                .before = 1))) %>%
  unnest(cols = c(data)) %>%
  #retire columns
  select(-contains("log.pr"),-contains("se.")) %>%
  # round numeric values
  mutate_at(.vars = vars(pr.s,conf.low.s,conf.high.s,
                         pr.m,conf.low.m,conf.high.m),
            .funs = round, digits=2) %>%
  # mutate_at(.vars = vars(p.value.s,p.value.m),
  #           .funs = round, digits=3) %>%
  #join confidence intervals
  mutate(ci.s=str_c(conf.low.s," - ",conf.high.s),
         ci.m=str_c(conf.low.m," - ",conf.high.m)) %>%
  #remove and reorder columns
  select(starts_with("value"),term,
         starts_with("pr"),starts_with("ci"),starts_with("p.val"),
         -starts_with("conf")) %>%
  select(starts_with("value"),term,ends_with(".s"),ends_with(".m")) %>%
  # select(-value.m) %>%
  #add ref to estimates
  mutate(pr.s=if_else(str_detect(term,".ref"),"Ref.",as.character(pr.s)),
         pr.m=if_else(str_detect(term,".ref"),"Ref.",as.character(pr.m))) %>%
  ungroup() %>% 
  # transform p value columns
  mutate(p.value.s=case_when(
    p.value.s>=0.001 ~ as.character(round(p.value.s,digits=3)),
    p.value.s<0.001 ~ "<0.001"
  )) %>% 
  mutate(p.value.m=case_when(
    p.value.m>=0.001 ~ as.character(round(p.value.m,digits=3)),
    p.value.m<0.001 ~ "<0.001"
  ))

final_table %>% 
  avallecam::print_inf()


# ______________ ----------------------------------------------------------


# OUTPUTS -----------------------------------------------------------------


# table -------------------------------------------------------------------


# __ write missings -------------------------------------------------------

uu_clean_data %>% 
  select(all_of(covariate_set01)) %>% 
  naniar::miss_var_summary() %>% 
  writexl::write_xlsx("table/04-seroprev-supp-table10.xlsx")

# __ write output ---------------------------------------------------------

final_table01 %>% 
  writexl::write_xlsx("table/04-seroprev-table03pre.xlsx")

final_table %>% 
  # clean name
  mutate(clean=str_replace(term,value.s,".")) %>% 
  select(value.s,contains("term"),contains("clean"),everything()) %>% 
  select(-term) %>% 
  writexl::write_xlsx("table/04-seroprev-table03.xlsx")


# __ all positives -----------------------------------------------------------

uu_clean_data %>% count(positividad_peru_num,edad_etapas_de_vida_t)
my_model <- survey::svyglm(positividad_peru_num ~ edad_etapas_de_vida_t, design)
epitidy::epi_tidymodel_pr(my_model)

# figure ------------------------------------------------------------------

multiple_models %>% 
  select(-c(1,3:5)) %>% 
  mutate(across(contains("denominator"),as.character)) %>% 
  
  # filter(magrittr::is_in(denominator,covariate_set01[c(4:7,9:11)])) %>% 
  
  mutate(clean=str_replace(term,denominator,".")#,
         # clean=str_c(denominator,clean)
         ) %>%
  # avallecam::print_inf()
  ggplot(aes(x = pr,y = clean)) +
  geom_point() +
  geom_errorbarh(aes(xmax=conf.high,xmin=conf.low),
                position = position_dodge(width = 0.5)) +
  facet_grid(denominator~.,scales = "free") +
  geom_vline(aes(xintercept=1))
