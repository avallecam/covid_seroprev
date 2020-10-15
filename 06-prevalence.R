#'
#' OBJETIVES:
#' - create prevalence table output with survey design
#' - correct seroprevalence due to missclassification
#' write_rds("data/outcome_01_adj_tbl.rds")
#' 

library(tidyverse)
library(skimr)
library(survey)
library(srvyr)
library(purrr)
library(furrr)
library(writexl)
library(tictoc)

theme_set(theme_bw())


# functions ---------------------------------------------------------------

# source("10-prevalence_functions.R")

# source("08-uncertainty_prev.R")

library(serosurvey)

set.seed(33)

# inputs ------------------------------------------------------------------

uu_clean_data <- read_rds("data/uu_clean_data.rds") %>% 
  mutate(survey_all="survey_all",
         weight_nul=1) %>% 
  # transformar a factor (prevlaencia ajustada)
  mutate_at(.vars = vars(igg,igm,ig_clasificacion,positividad_peru),
            .funs = as.factor) #%>% 
  # # transformar a numerico (prevalencia cruda)
  # mutate_at(.vars = vars(igg,igm,ig_clasificacion,positividad_peru),
  #           .funs = list("num"=outcome_to_numeric)) %>% 
  # # extender respuestas por condicicion de riesgo
  # mutate_at(.vars = vars(starts_with("condicion_riesgo_")),
  #           .funs = list("ext"=~riesgo_extend_na(variable = .x,referencia = riesgo))) #%>% 
  # # de ord a fct
  # mutate(edad_decenios=as.character(edad_decenios),
  #        edad_decenios=as.factor(edad_decenios))

# QC exposure | outcomes! ---------------------------------------------------------------

uu_clean_data %>% 
  select(edad,sexo) %>% 
  naniar::miss_var_summary()

uu_clean_data %>% 
  count(ig_clasificacion,convResultado,positividad_peru) %>% 
  avallecam::print_inf()
  
# uu_clean_data %>%
#   count(cd_dist,conglomerado,numero_vivienda,#numero_hogar,
#         participante,nro_convivientes,n_registros_vv) %>% 
#   naniar::miss_var_summary()

# pobreza hacinamiento
uu_clean_data %>% 
  select(
    starts_with("nbi_"),
    pobreza,pobreza_dico,ind_pobreza,
    nro_dormitorios,nro_convivientes,participante,n_registros_vv,
    ind_hacin,hacinamiento
    ) %>% 
  # glimpse()
  # filter(is.na(ind_hacin)|is.na(hacinamiento)) %>%
  # filter(!is.na(nro_convivientes)) %>%
  # # filter(is.na(nro_convivientes)) %>% 
  # # skimr::skim(ind_hacin)
  # avallecam::print_inf()
  # # count(ind_pobreza,sort = T)
  naniar::miss_var_summary()

uu_clean_data %>% count(pobreza_dico)
uu_clean_data %>% count(hacinamiento)

#edad
uu_clean_data %>% 
  count(edad_etapas_de_vida_c,edad_etapas_de_vida_t,edad_etapas_de_vida_n)

# sintomas
uu_clean_data %>% 
  count(sintomas_cualquier_momento,sintomas_si_no,sintomas_previos)

# laboratorio
uu_clean_data %>% 
  count(tipo_muestra_pcr,convResultado)

# serología
uu_clean_data %>% 
  count(resultado_pr,resultado_pr2,ig_clasificacion,igg,igg_igm,igm)

# peru
uu_clean_data %>% 
  count(ig_clasificacion,igg,igm,convResultado,positividad_peru)

# explorar ----------------------------------------------------------------

uu_clean_data %>% naniar::miss_var_summary()

# uu_clean_data %>%
#   filter(cd_dist=="150102") %>%
#   count(cd_dist,nm_dist,conglomerado,numero_vivienda)
# 
# temporary_just_1_psu <- uu_clean_data %>%
#   count(cd_dist,nm_dist,conglomerado,sort = T) %>%
#   count(cd_dist,nm_dist,sort = T) %>%
#   filter(n==1) # un conglomerado por distrito

uu_clean_data %>% 
  janitor::tabyl(ig_clasificacion)

uu_clean_data %>% 
  janitor::tabyl(sexo,ig_clasificacion) %>% 
  avallecam::adorn_ame()

# uu_clean_data %>% 
#   select(ig_clasificacion,sexo,
#          edad_etapas_de_vida_c,edad_decenios,edad_quinquenal,
#          diris,convResultado,sintomas_si_no)

uu_clean_data %>% 
  select(cd_dist,nm_dist,conglomerado,numero_vivienda,distrito.y:factorfinal) %>% 
  # filter(cd_dist=="150132") %>%
  # filter(conglomerado=="22483") #%>%
  # # count(conglomerado)
  filter(is.na(factorfinal))

uu_clean_data %>% 
  select(edad_decenios,ig_clasificacion) %>% 
  count(edad_decenios,ig_clasificacion)
  # naniar::miss_var_summary()

uu_clean_data %>% pull(ig_clasificacion) %>% levels()

# ____________ ------------------------------------------------------------


# DESCRIPTIVO -------------------------------------------------------------



# __ valores perdidos -----------------------------------------------------

uu_clean_data %>% 
  naniar::miss_var_summary() %>% 
  avallecam::print_inf()


# __ descripción poblacional -------------------------------------------------

uu_clean_data %>% 
  select(survey_all,
         sexo,
         edad_inei_grupos_labels,
         edad_quinquenal,
         # edad_etapas_de_vida_c,
         # edad_etapas_de_vida_t,
         # edad_decenios,
         # edad_quinquenal,
         # diris,
         nm_prov,
         # pais_origen,
         # starts_with("nm_"),
         # pobreza_dico,
         # tipo_vivienda,
         agua,
         desague,
         electricidad,
         # nro_convivientes_cut,
         # nro_dormitorios_cut,
         ind_hacin_cut,
         hacinamiento,
         # sintomas_cualquier_momento_cat,
         # riesgo,
         # ends_with("_ext"),
         # -contains("ninguna"),
         # -contains("otro"),
         # -contains("salud"),
         # -contains("renal"),
         # -contains("60a"),
         # contacto_covid,
         # etnia_cat,
         # trabajo_reciente,
         # atencion,
         seguro_salud,
         convResultado,
         ig_clasificacion
  ) %>%
  mutate(across(c(#etnia_cat,
                  seguro_salud,
                  desague,
                  agua#,
                  #tipo_vivienda
                  ),
                fct_infreq)) %>% 
  # compareGroups::compareGroups(ig_clasificacion~.,
  compareGroups::compareGroups(survey_all~.,
                               # include.miss = T,
                               data = .,
                               max.xlev = 30,
                               chisq.test.perm = TRUE,
                               # byrow = T
                               ) %>%
  compareGroups::createTable(digits = 1,
                             # show.all = T,
                             sd.type = 2,
                             show.p.overall = F,
                             # show.ratio = T,
                             show.n = T) %>%
  # compareGroups::export2xls("table/01-compareGroups-output-01.xls")
  compareGroups::export2xls("table/02-seroprev-supp-table01-a.xls")

# uu_clean_data %>%
#   compareGroups::compareGroups(positividad_peru~.,data = .,max.xlev = 20,
#                                chisq.test.perm = TRUE,byrow = T) %>%
#   compareGroups::createTable(digits = 1,sd.type = 2,show.ratio = T,show.n = T) %>%
#   compareGroups::export2xls("table/01-compareGroups-output-02.xls")


# ___________ -------------------------------------------------------------

# SELECT COVARIATES -------------------------------------------------------

covariate_set01 <- uu_clean_data %>% 
  select(survey_all,
         sexo,
         edad_etapas_de_vida_t,
         edad_decenios,
         # edad_quinquenal,
         diris,
         # pobreza_dico,
         hacinamiento,
         # nro_dormitorios_cat,
         nm_prov,
         sintomas_cualquier_momento_cat,
         sintomas_cualquier_momento_cat_fecha_14d_v1,
         sintomas_cualquier_momento_cat_fecha_rangos,
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
         prueba_previa_res
         # etnia_cat,
         # trabajo_reciente,
         # atencion,
         # seguro_salud,
         ) %>% 
  colnames()

#' [sub-prevalencias]
#' nuevo __group_by__ para cada uno
#' 
#' ** sintomas_cualquier_momento_cat
#' dentro de sintomáticos en cualquier momento
#' - inicio de sintomas: en últimos 14 días
#' - necesitó atencion medica? atencion_sintomas
#' - faltó labores? falto_labores
#' - tuvo que ser hospitalizado? situa_hospitalizado #solo sintomas previos
#' 
#' ** contacto_covid
#' dentro de los contactados
#' - cuarentena: si o no
#' - contacto_tipo: 5 tipos
#' - fecha_last_contacto: [categorizar]
#' 
#' ** trabajo_reciente
#' dentro de los que trabajaron
#' - rubro: 6 rubros

#' [analisis 2: dentro de los positivos]
#' # nuevo __denominador__ 
#' 
#' asintomáticos
#' cuarentena
#' contacto_tipo ------> $ contacto_tipo + cuarentena
#' fecha_last_contacto
#' seguro_salud

covariate_set02 <- uu_clean_data %>% 
  select(#survey_all,
         # sexo,
         # edad_etapas_de_vida_t,
         # edad_decenios,
         # # edad_quinquenal,
         # diris,
         # # pobreza_dico,
         # hacinamiento,
         # nro_dormitorios_cat,
         # nm_prov,
         sintomas_cualquier_momento_cat,
         sintomas_cualquier_momento_cat_fecha_14d_v1,
         sintomas_cualquier_momento_cat_fecha_rangos,
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
         prueba_previa_res
         # etnia_cat,
         # trabajo_reciente,
         # atencion,
         # seguro_salud,
  ) %>% 
  colnames()


# ____________ ------------------------------------------------------------

# SEROPREVALENCIA ---------------------------------------------------------

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

# tablas de prevalencia (e.g.) ------

#' ejemplos
#' 1. fraccion de positivos en cada grupo de sinto, oligo, asinto
#' 2. fraccion de sinto, oligo, asinto en el grupo de positivos o negativos

# serosvy_proportion(design = design,
#                       denominator = sintomas_cualquier_momento_cat,
#                       numerator = ig_clasificacion) %>% 
#   select(-ends_with("_low"),-ends_with("_upp"),-ends_with("_cv"),-ends_with("_deff"))
# 
# serosvy_proportion(design = design,
#                       denominator = ig_clasificacion,
#                       numerator = sintomas_cualquier_momento_cat) %>% 
#   select(-ends_with("_low"),-ends_with("_upp"),-ends_with("_cv"),-ends_with("_deff"))
# 
# serosvy_proportion(design = design,
#                       denominator = edad_decenios,
#                       numerator = ig_clasificacion) %>% 
#   select(-ends_with("_cv"),-ends_with("_deff"),-ends_with("_se"),-denominator,-numerator)
# 
# serosvy_proportion(design = design,
#                       denominator = survey_all,
#                       numerator = ig_clasificacion) %>% 
#   glimpse()
#   # select(#-ends_with("_low"),-ends_with("_upp"),
#   #   -ends_with("_cv"),-ends_with("_deff"))
# 
# serosvy_proportion(design = design,
#                       denominator = sexo,
#                       numerator = ig_clasificacion) %>% 
#   select(#-ends_with("_low"),-ends_with("_upp"),
#          -ends_with("_cv"),-ends_with("_deff"),-ends_with("_se"),-contains("total"))

# _ 1. estimates: raw + weighted ---------------------------------------------------------------

outcome_01_pre <- 
  # crear matriz
  # set 01 of denominator-numerator
  expand_grid(
    design=list(design),
    denominator=covariate_set01,
    numerator=c("ig_clasificacion","positividad_peru")
  ) %>% 
  # set 02 of denominator-numerator (e.g. within main outcome)
  union_all(
    expand_grid(
      design=list(design),
      denominator=c("ig_clasificacion","positividad_peru"),
      numerator=covariate_set02
    )
  ) %>% 
  # crear simbolos
  mutate(
    denominator=map(denominator,dplyr::sym),
    numerator=map(numerator,dplyr::sym)
  ) %>% 
  # estimar prevalencia
  
  mutate(output=pmap(.l = select(.,design,denominator,numerator),
                     .f = serosvy_proportion)) %>% 
  
  # mutate(output=map(.x = output,.f = tidy_srvyr_tibble)) %>% 
  select(-design,-denominator,-numerator) %>% 
  unnest(cols = c(output)) #%>% 

# outcome_01_pre %>% avallecam::print_inf()


# _ 2. test performance ---------------------------------------------------

# __ filter + add values --------------------------------------------------

outcome_01_adj_pre <- outcome_01_pre %>% 
  # only serological results
  filter(numerator=="ig_clasificacion") %>% 
  # only positives
  filter(numerator_level=="positivo") %>% 
  # remove some covariates
  # filter(!magrittr::is_in(denominator,c("edad_decenios","nm_prov","hacinamiento","contacto_covid"))) %>% 
  # round numbers are required
  mutate_at(.vars = vars(total,total_den,
                         total_low,total_den_low,
                         total_upp,total_den_upp),
            .funs = list("round"=round),digits = 0) %>%
  # unknown test local validation results
  # for sensitivity:
  # 30 true positives among 30 RT-PCR sars-cov-2 positive samples
  # for specificity:
  # 0 false positives among 50 RT-PCR sars-cov-2 negative samples
  # for cross reactivity:
  # 2 false positives among 50 prepandemic samples positives for other pathogens
  mutate(
    true_positive = 30,
    false_negative = 0,
    false_positive = 0+2,
    true_negative = 50+48
  ) %>% 
  rownames_to_column() %>%
  mutate(rowname=as.numeric(rowname))


# __ only one -------------------------------------------------------------


  
# __ apply + extract ----------------------------------------------------------------
# 56-60sec por covariable 
# 4GB RAM
# paralelizando en 8 nucleos usando purrr y furrr

plan(multisession, workers = availableCores())
tic()

out <- tibble()

for (i in 1:nrow(outcome_01_adj_pre)) {
  
  out <- outcome_01_adj_pre %>% 
    
    slice(i) %>% 
    
    # dot
    mutate(adj_dot_unk=future_pmap(.l = select(.,
                                               positive_number_test=total_round,
                                               total_number_test=total_den_round),
                                   .f = possibly(serosvy_unknown_sample_posterior,otherwise = NA_real_),
                                   true_positive = true_positive,
                                   false_negative = false_negative,
                                   false_positive = false_positive,
                                   true_negative = true_negative)) %>% 
    serosvy_extract_posterior(variable = adj_dot_unk) %>% 
    
    # low
    mutate(adj_low_unk=future_pmap(.l = select(.,
                                               positive_number_test=total_low_round,
                                               total_number_test=total_den_low_round),
                                   .f = possibly(serosvy_unknown_sample_posterior,otherwise = NA_real_),
                                   true_positive = true_positive,
                                   false_negative = false_negative,
                                   false_positive = false_positive,
                                   true_negative = true_negative)) %>%
    serosvy_extract_posterior(variable = adj_low_unk) %>%
    
    # upp
    mutate(adj_upp_unk=future_pmap(.l = select(.,
                                               positive_number_test=total_upp_round,
                                               total_number_test=total_den_upp_round),
                                   .f = possibly(serosvy_unknown_sample_posterior,otherwise = NA_real_),
                                   true_positive = true_positive,
                                   false_negative = false_negative,
                                   false_positive = false_positive,
                                   true_negative = true_negative)) %>%
    serosvy_extract_posterior(variable = adj_upp_unk) %>% 
    
    # union all outputs
    union_all(out)
  
  out %>% print()
  # # known test
  # mutate(adj_dot_kno=future_pmap(.l = select(.,
  #                                        positive_number_test=total_round,
  #                                        total_number_test=total_den_round),
  #                            .f = possibly(serosvy_known_sample_posterior,otherwise = NA_real_),
  #                            sensitivity=0.999,
  #                            specificity=0.960))
  
}

toc()

outcome_01_adj <- out  %>% 
  mutate(rowname=as.numeric(rowname)) %>% 
  arrange(rowname) %>% 
  select(-rowname) 

# _ 3. create output format -----------------------------------------------



outcome_01_adj_tbl <- 
  # start from original dataset
  outcome_01_pre %>% 
  # only positives
  filter(numerator_level=="positivo"|denominator_level=="positivo") %>% 
  # left join with db with test performance update
  left_join(outcome_01_adj) %>% 
  # naniar::miss_var_summary() %>% 
  # avallecam::print_inf() %>% 
  
  # apply format
  unite_dotwhiskers(variable_dot = raw_prop, 
                    variable_low = raw_prop_low,
                    variable_upp = raw_prop_upp,
                    digits_dot = 2,
                    digits_low = 1,
                    digits_upp = 2) %>% 
  unite_dotwhiskers(variable_dot = prop, 
                    variable_low = prop_low,
                    variable_upp = prop_upp,
                    digits_dot = 2,
                    digits_low = 1,
                    digits_upp = 2) %>% 
  unite_dotwhiskers(variable_dot = adj_dot_unk_p50,
                    variable_low = adj_low_unk_p50,
                    variable_upp = adj_upp_unk_p50,
                    digits_dot = 2,
                    digits_low = 2,
                    digits_upp = 3)

# _ 4. evaluate output ----------------------------------------------------


outcome_01_adj_tbl %>% 
  select(1:4,starts_with("unite1_")) %>% 
  # view()
  avallecam::print_inf()




# pryr::mem_used()


# ___________ -------------------------------------------------------------

# OUTPUTS -----------------------------------------------------------------

# write -------------------------------------------------------------------


outcome_01_adj_tbl %>% readr::write_rds("data/outcome_01_adj_tbl.rds")
