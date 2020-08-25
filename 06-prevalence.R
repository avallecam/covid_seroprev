#'
#' OBJETIVO:
#' - crear tablas de prevalencia usando survey design
#' - temporalmente, creación y grabacion final de base limpia
#' write_rds("data/uu_clean_data.rds")
#' 
#' PENDIENTES:
#' (x) ¿cómo podemos usar la proporción para proyectar la cantidad de positivos y engativos expandida?
#' (x) agrupa de 80 años a más + para decenios y eliminar 
#' ( ) actualizar flujo con nuevas funciones (proportion = TRUE)
#' ( ) pasar funciones a paquete serosurvey (idea: combo project repo + package functions)
#' (x) recover results from RAW n, p, t group_by(denominator,numerator)
#' ( ) muestreo: calcular n o % de cobertura a nivel vivienda con respecto a nro convivientes


library(tidyverse)
library(survey)
library(srvyr)
library(writexl)

theme_set(theme_bw())


# functions ---------------------------------------------------------------

source("10-prevalence_functions.R")

source("08-uncertainty_prev.R")


# inputs ------------------------------------------------------------------

uu_clean_data <- read_rds("data/uu_clean_data.rds") %>% 
  mutate(survey_all="survey_all") %>% 
  # transformar a factor (prevlaencia ajustada)
  mutate_at(.vars = vars(igg,igm,ig_clasificacion,positividad_peru),
            .funs = as.factor) %>% 
  # transformar a numerico (prevalencia cruda)
  mutate_at(.vars = vars(igg,igm,ig_clasificacion,positividad_peru),
            .funs = list("num"=outcome_to_numeric)) %>% 
  # extender respuestas por condicicion de riesgo
  mutate_at(.vars = vars(starts_with("condicion_riesgo_")),
            .funs = list("ext"=~riesgo_extend_na(variable = .x,referencia = riesgo))) #%>% 
  # # de ord a fct
  # mutate(edad_decenios=as.character(edad_decenios),
  #        edad_decenios=as.factor(edad_decenios))

# QC exposure | outcomes! ---------------------------------------------------------------

read_rds("data/uu_clean_data.rds") %>% 
  select(edad,sexo) %>% 
  naniar::miss_var_summary()

read_rds("data/uu_clean_data.rds") %>% 
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
  compareGroups::compareGroups(ig_clasificacion~.,data = .,max.xlev = 20,
                               chisq.test.perm = TRUE,byrow = T) %>%
  compareGroups::createTable(digits = 1,sd.type = 2,show.ratio = T,show.n = T) %>%
  compareGroups::export2xls("table/01-compareGroups-output-01.xls")

uu_clean_data %>%
  compareGroups::compareGroups(positividad_peru~.,data = .,max.xlev = 20,
                               chisq.test.perm = TRUE,byrow = T) %>%
  compareGroups::createTable(digits = 1,sd.type = 2,show.ratio = T,show.n = T) %>%
  compareGroups::export2xls("table/01-compareGroups-output-02.xls")

# ________ ----------------------------------------------------------------


# PROPORCION CRUDA --------------------------------------------------------

# PENDIENTE: REPREX con cdcper dotwhiskes plot
# usando .funs = funs("num"=outcome_to_numeric)
# https://stackoverflow.com/questions/35953394/calculating-length-of-95-ci-using-dplyr

library(moderndive)
library(infer)
library(gmodels)
raw_prop_table <- uu_clean_data %>% 
  select(rowname,ends_with("_num")) %>% 
  pivot_longer(cols = -rowname,names_to = "outcome",values_to = "value") %>% 
  group_by(outcome) %>% 
  summarise(
    raw_obse=n(),
    raw_prop=ci.binom(value)[1],
    raw_lowc=ci.binom(value)[2],
    raw_uppc=ci.binom(value)[3],
    raw_semc=ci.binom(value)[3]
  ) %>% 
  cdc_srvyr_create_table_free(estim_var = raw_prop,
                              cilow_var = raw_lowc,
                              ciupp_var = raw_uppc,
                              cilow_digits = 3,
                              ciupp_digits = 3) %>% 
  select(-estim_tab,-cilow_tab,-ciupp_tab) %>% 
  rename(raw_proportion_tab=fused_tab)

raw_prop_table

# ___________ -------------------------------------------------------------

uu_clean_data %>% 
  select(sintomas_cualquier_momento_cat) %>% 
  mutate(sintomas_cualquier_momento_cat=labelled::to_labelled(sintomas_cualquier_momento_cat)) %>% 
  count(sintomas_cualquier_momento_cat)

# SELECT COVARIATES -------------------------------------------------------

covariate_list <- uu_clean_data %>% 
  select(sexo,
         edad_etapas_de_vida_t,
         # edad_decenios,
         # edad_quinquenal,
         diris,
         pobreza_dico,
         hacinamiento,
         nro_dormitorios_cat,
         nm_prov,
         sintomas_cualquier_momento_cat,
         riesgo,
         ends_with("_ext"),
         -contains("ninguna"),
         -contains("otro"),
         -contains("salud"),
         -contains("renal"),
         -contains("60a"),
         contacto_covid,
         etnia_cat,
         trabajo_reciente,
         atencion,
         seguro_salud,
         prueba_previa) %>% 
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
#' contacto_tipo
#' fecha_last_contacto
#' seguro_salud

# ____________ ------------------------------------------------------------

# SEROPREVALENCIA ---------------------------------------------------------

# tratamiento de stratos con un solo conglomerado
options(survey.lonely.psu = "certainty")

uu_clean_data %>% count(CONGLOMERADO,VIVIENDA)

# diseño muestral de la encuesta ---------------------------------

design <- uu_clean_data %>% 
  # mutate_at(.vars = vars(igg,igm,ig_clasificacion,positividad_peru),
  #           .funs = labelled::to_labelled) %>%
  # mutate_at(.vars = vars(covariate_list),
  #           .funs = ~labelled::to_labelled(as.factor(.x))) %>%
  # mutate(sintomas_cualquier_momento_cat=as.numeric(sintomas_cualquier_momento_cat)) %>% 
  # select(sintomas_cualquier_momento_cat,ig_clasificacion=ig_clasificacion_num,
  #        factorfinal,CONGLOMERADO,VIVIENDA,ESTRATO,PONDERACION) %>% 
  
  filter(!is.na(ig_clasificacion)) %>% #CRITICAL! ON OUTCOME
  filter(!is.na(factorfinal)) %>% #NO DEBEN DE HABER CONGLOMERADOS SIN WEIGHT
  
  as_survey_design(c(CONGLOMERADO, VIVIENDA),
                   #id = CONGLOMERADO, #clusters or psu (primary sampling unit) 
                   #add vivienda como *SSU*
                   strata = ESTRATO, #clusters need to be nested in the strata
                   weights= PONDERACION # factores de expancion
                   )

# tablas de prevalencia ------

#' ejemplos
#' 1. fraccion de positivos en cada grupo de sinto, oligo, asinto
#' 2. fraccion de sinto, oligo, asinto en el grupo de positivos o negativos
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

cdc_survey_proportion(design = design,
                      denominator = sintomas_cualquier_momento_cat,
                      numerator = ig_clasificacion) %>% 
  select(-ends_with("_low"),-ends_with("_upp"),-ends_with("_cv"),-ends_with("_deff"))

cdc_survey_proportion(design = design,
                      denominator = ig_clasificacion,
                      numerator = sintomas_cualquier_momento_cat)

cdc_survey_proportion(design = design,
                      denominator = edad_decenios,
                      numerator = ig_clasificacion)

cdc_survey_proportion(design = design,
                      denominator = survey_all,
                      numerator = ig_clasificacion) %>% 
  glimpse()
  # summarise_at(.vars = vars(starts_with("total")),.funs = sum)

# 01_general ----------------------------------------------------------------

out0101 <- cdc_srvyr_prevalence_outcome(design = design,
                                        outcome = ig_clasificacion)
out0101 %>% glimpse()

# 02_espacial: diris ----------------------------------------------------------------

out0106 <- cdc_srvyr_prevalence_one_covariate(design = design,
                                              covariate = diris,
                                              outcome = ig_clasificacion)
out0106

# 03_edad: decenio ----------------------------------------------------------------

out0104 <- cdc_srvyr_prevalence_one_covariate(design = design,
                                              covariate = edad_decenios,
                                              outcome = ig_clasificacion)
out0104

# 03_edad: quinquenio ----------------------------------------------------------------#

# out0105 <- cdc_srvyr_prevalence_one_covariate(design = design,
#                                               covariate = edad_quinquenal,
#                                               outcome = ig_clasificacion)
# out0105


# 04_covariates ---------------------------------------------------------------

outcome_01_pre <- 
  # crear matriz
  tibble(
    design=list(design),
    covariate=covariate_list,
    outcome="ig_clasificacion"
  ) %>% 
  # crear simbolos
  mutate(
    covariate=map(covariate,dplyr::sym),
    outcome=map(outcome,dplyr::sym)
  ) %>% 
  # estimar prevalencia
  mutate(output=pmap(.l = select(.,design,covariate,outcome),
                     .f = cdc_srvyr_prevalence_one_covariate)) %>% 
  mutate(output=map(.x = output,.f = tidy_srvyr_tibble)) %>% 
  select(-design,-covariate,-outcome) %>% 
  unnest(cols = c(output)) #%>% 
# cdc_srvyr_tibble_02(colname_number = 3)

# ___________ -------------------------------------------------------------

# IgG ---------------------------------------------------------------------

# diseño muestral de la encuesta ---------------------------------

design_02 <- uu_clean_data %>% 
  # filter(!magrittr::is_in(cd_dist,temporary_just_1_psu$cd_dist)) %>% #3041
  
  # as_factor() %>% #importar variable+value labels
  
  filter(!is.na(igg)) %>% #CRITICAL! ON OUTCOME
  filter(!is.na(factorfinal)) %>% #NO DEBEN DE HABER CONGLOMERADOS SIN WEIGHT
  
  as_survey_design(c(CONGLOMERADO, VIVIENDA),
                   #id = CONGLOMERADO, #clusters or psu (primary sampling unit) 
                   #add vivienda como *SSU*
                   strata = ESTRATO, #clusters need to be nested in the strata
                   weights= PONDERACION # factores de expancion
  )

# tablas de prevalencia ------


# 01_general ----------------------------------------------------------------

out0201 <- cdc_srvyr_prevalence_outcome(design = design_02,
                                        outcome = igg)
out0201

# 02_espacial: diris ----------------------------------------------------------------

out0206 <- cdc_srvyr_prevalence_one_covariate(design = design_02,
                                              covariate = diris,
                                              outcome = igg)
out0206

# 03_edad: decenio ----------------------------------------------------------------

out0204 <- cdc_srvyr_prevalence_one_covariate(design = design_02,
                                              covariate = edad_decenios,
                                              outcome = igg)
out0204 

# 03_edad: quinquenio ----------------------------------------------------------------#

# out0205 <- cdc_srvyr_prevalence_one_covariate(design = design_02,
#                                               covariate = edad_quinquenal,
#                                               outcome = igg)
# out0205 #%>% write_xlsx("table/tab05-sarscov2-edad_quinquenal-20c.xlsx")


# 04_covariates ---------------------------------------------------------------

outcome_02_pre <- 
  # crear matriz
  tibble(
    design=list(design_02),
    covariate=covariate_list,
    outcome="igg"
  ) %>% 
  # crear simbolos
  mutate(
    covariate=map(covariate,dplyr::sym),
    outcome=map(outcome,dplyr::sym)
  ) %>% 
  # estimar prevalencia
  mutate(output=pmap(.l = select(.,design,covariate,outcome),
                     .f = cdc_srvyr_prevalence_one_covariate)) %>% 
  mutate(output=map(.x = output,.f = tidy_srvyr_tibble)) %>% 
  select(-design,-covariate,-outcome) %>% 
  unnest(cols = c(output)) #%>% 
# cdc_srvyr_tibble_02(colname_number = 3)



# ___________ -------------------------------------------------------------

# IgM ---------------------------------------------------------------------

# diseño muestral de la encuesta ---------------------------------

design_03 <- uu_clean_data %>% 
  # filter(!magrittr::is_in(cd_dist,temporary_just_1_psu$cd_dist)) %>% #3041
  
  # as_factor() %>% #importar variable+value labels
  
  filter(!is.na(igm)) %>% #CRITICAL! ON OUTCOME
  filter(!is.na(factorfinal)) %>% #NO DEBEN DE HABER CONGLOMERADOS SIN WEIGHT
  
  as_survey_design(c(CONGLOMERADO, VIVIENDA),
                   #id = CONGLOMERADO, #clusters or psu (primary sampling unit) 
                   #add vivienda como *SSU*
                   strata = ESTRATO, #clusters need to be nested in the strata
                   weights= PONDERACION # factores de expancion
  )

# tablas de prevalencia ------


# 01_general ----------------------------------------------------------------

out0301 <- cdc_srvyr_prevalence_outcome(design = design_03,
                                        outcome = igm)
out0301

# 02_espacial: diris ----------------------------------------------------------------

out0306 <- cdc_srvyr_prevalence_one_covariate(design = design_03,
                                              covariate = diris,
                                              outcome = igm)
out0306


# 03_edad: decenio ----------------------------------------------------------------

out0304 <- cdc_srvyr_prevalence_one_covariate(design = design_03,
                                              covariate = edad_decenios,
                                              outcome = igm)
out0304 

# 03_edad: quinquenio ----------------------------------------------------------------#

# out0305 <- cdc_srvyr_prevalence_one_covariate(design = design_03,
#                                               covariate = edad_quinquenal,
#                                               outcome = igm)
# out0305


# 04_covariates ---------------------------------------------------------------

outcome_03_pre <- 
  # crear matriz
  tibble(
    design=list(design_03),
    covariate=covariate_list,
    outcome="igm"
  ) %>% 
  # crear simbolos
  mutate(
    covariate=map(covariate,dplyr::sym),
    outcome=map(outcome,dplyr::sym)
  ) %>% 
  # estimar prevalencia
  mutate(output=pmap(.l = select(.,design,covariate,outcome),
                     .f = cdc_srvyr_prevalence_one_covariate)) %>% 
  mutate(output=map(.x = output,.f = tidy_srvyr_tibble)) %>% 
  select(-design,-covariate,-outcome) %>% 
  unnest(cols = c(output)) #%>% 
# cdc_srvyr_tibble_02(colname_number = 3)



# ____________ ------------------------------------------------------------

# CONFIRMADOS PERU --------------------------------------

# diseño muestral de la encuesta ---------------------------------

design_04 <- uu_clean_data %>% 
  # filter(!magrittr::is_in(cd_dist,temporary_just_1_psu$cd_dist)) %>% #3041
  
  # as_factor() %>% #importar variable+value labels
  
  filter(!is.na(positividad_peru)) %>% #CRITICAL! ON OUTCOME
  filter(!is.na(factorfinal)) %>% #NO DEBEN DE HABER CONGLOMERADOS SIN WEIGHT
  
  as_survey_design(c(CONGLOMERADO, VIVIENDA),
                   #id = CONGLOMERADO, #clusters or psu (primary sampling unit) 
                   #add vivienda como *SSU*
                   strata = ESTRATO, #clusters need to be nested in the strata
                   weights= PONDERACION # factores de expancion
  )

# tablas de prevalencia ------

# 01_general ----------------------------------------------------------------

out0401 <- cdc_srvyr_prevalence_outcome(design = design_04,
                                        outcome = positividad_peru)
out0401

# 02_espacial: diris ----------------------------------------------------------------

out0406 <- cdc_srvyr_prevalence_one_covariate(design = design_04,
                                              covariate = diris,
                                              outcome = positividad_peru)
out0406

# 03_edad: decenio ----------------------------------------------------------------

out0404 <- cdc_srvyr_prevalence_one_covariate(design = design_04,
                                              covariate = edad_decenios,
                                              outcome = positividad_peru)
out0404 

# 03_edad: quinquenio ----------------------------------------------------------------#

# out0405 <- cdc_srvyr_prevalence_one_covariate(design = design_04,
#                                               covariate = edad_quinquenal,
#                                               outcome = positividad_peru)
# out0405


# 04_covariates ---------------------------------------------------------------

outcome_04_pre <- 
  # crear matriz
  tibble(
    design=list(design_04),
    covariate=covariate_list,
    outcome="positividad_peru"
  ) %>% 
  # crear simbolos
  mutate(
    covariate=map(covariate,dplyr::sym),
    outcome=map(outcome,dplyr::sym)
  ) %>% 
  # estimar prevalencia
  mutate(output=pmap(.l = select(.,design,covariate,outcome),
                     .f = cdc_srvyr_prevalence_one_covariate)) %>% 
  mutate(output=map(.x = output,.f = tidy_srvyr_tibble)) %>% 
  select(-design,-covariate,-outcome) %>% 
  unnest(cols = c(output)) #%>% 
# cdc_srvyr_tibble_02(colname_number = 3)




# __________ --------------------------------------------------------------


# CORRECCION Sen/Spe ------------------------------------------------------

# pryr::mem_used()
# 
# rm(list = ls())

# source("10-prevalence_functions.R")
# source("08-uncertainty_prev.R")

# figura01 <- read_rds("data/33-seroprev-figure01.rds")

# pryr::mem_used()


# ___________ -------------------------------------------------------------

# OUTPUTS -----------------------------------------------------------------

#' table with raw, population adjusted, test performance adjusted
#' figure 1 (overall, sex, age) showing the four outcomes!
#' figure 2 (age decenio) showing trend
#' figure 3 map per diris
#' 
#' more
#' performance of surveillance due to underreporting

# tables ------------------------------------------------------------------


outcome_01 <- out0101 %>% 
  union_all(outcome_01_pre) %>% 
  # union_all(out0102 %>% tidy_srvyr_tibble()) %>% 
  # union_all(out0103 %>% tidy_srvyr_tibble()) %>% 
  # union_all(out0106 %>% tidy_srvyr_tibble()) %>% 
  # union_all(out0107 %>% tidy_srvyr_tibble()) %>% 
  # union_all(out0108 %>% tidy_srvyr_tibble()) %>% 
  # union_all(out0109 %>% tidy_srvyr_tibble()) %>% 
  # union_all(out0110 %>% tidy_srvyr_tibble()) %>% 
  cdc_srvyr_tibble_02()

outcome_02 <- out0201 %>% 
  union_all(outcome_02_pre) %>% 
  # union_all(out0202 %>% tidy_srvyr_tibble()) %>% 
  # union_all(out0203 %>% tidy_srvyr_tibble()) %>% 
  # union_all(out0206 %>% tidy_srvyr_tibble()) %>% 
  # union_all(out0207 %>% tidy_srvyr_tibble()) %>% 
  # union_all(out0208 %>% tidy_srvyr_tibble()) %>% 
  # union_all(out0209 %>% tidy_srvyr_tibble()) %>% 
  # union_all(out0210 %>% tidy_srvyr_tibble()) %>% 
  cdc_srvyr_tibble_02()

outcome_03 <- out0301 %>% 
  union_all(outcome_03_pre) %>% 
  # union_all(out0302 %>% tidy_srvyr_tibble()) %>% 
  # union_all(out0303 %>% tidy_srvyr_tibble()) %>% 
  # union_all(out0306 %>% tidy_srvyr_tibble()) %>% 
  # union_all(out0307 %>% tidy_srvyr_tibble()) %>% 
  # union_all(out0308 %>% tidy_srvyr_tibble()) %>% 
  # union_all(out0309 %>% tidy_srvyr_tibble()) %>% 
  # union_all(out0310 %>% tidy_srvyr_tibble()) %>% 
  cdc_srvyr_tibble_02()

outcome_04 <- out0401 %>% 
  union_all(outcome_04_pre) %>% 
  # union_all(out0402 %>% tidy_srvyr_tibble()) %>% 
  # union_all(out0403 %>% tidy_srvyr_tibble()) %>% 
  # union_all(out0406 %>% tidy_srvyr_tibble()) %>% 
  # union_all(out0407 %>% tidy_srvyr_tibble()) %>% 
  # union_all(out0408 %>% tidy_srvyr_tibble()) %>% 
  # union_all(out0409 %>% tidy_srvyr_tibble()) %>% 
  # union_all(out0410 %>% tidy_srvyr_tibble()) %>% 
  cdc_srvyr_tibble_02()


# figure ------------------------------------------------------------------

# __fig01: all -----------------------------------------------------------------

figura01 <- outcome_01 %>% 
  union_all(outcome_02) %>% 
  union_all(outcome_03) %>% 
  union_all(outcome_04) %>% 
  filter(covariate!="diris") %>% 
  mutate(covariate=if_else(category=="overall","overall",covariate)) %>% 
  mutate(covariate=fct_relevel(covariate,"overall","sexo")) %>% 
  mutate(covariate=fct_recode(covariate,
                              "Pob. General"="overall",
                              "Sexo Biológico"="sexo",
                              "Etapas de Vida"="edad_etapas_de_vida_t",
                              "Hacinamiento"="hacinamiento",
                              "Pobreza"="pobreza_dico")) %>% 
  mutate(category=fct_relevel(category,"ninho","adolescente","joven","adulto",
                              "Pobre","No pobre")) %>% 
  mutate(category=fct_recode(category,"Prueba"="overall",
                             "Niño"="ninho","Adolescente"="adolescente",
                             "Joven"="joven","Adulto"="adulto",
                             "Adulto Mayor"="adulto_mayor",
                             "Femenino"="femenino",
                             "Masculino"="masculino")) %>% 
  mutate(outcome=fct_relevel(outcome,"igm","igg","ig_clasificacion")) %>% 
  mutate(outcome=fct_recode(outcome,"IgM+"="igm","IgG+"="igg",
                            "IgM+ o IgG+"="ig_clasificacion",
                            "IgM+ o IgG+ o PCR+"="positividad_peru",
  ))

figura01 %>% 
  cdc_srvyr_create_table(estim_digits = 2) %>% 
  # select(prevalence_tab)
  writexl::write_xlsx("table/33-seroprev-figura01.xlsx")

figura01 %>% 
  
  filter(outcome!="IgM+") %>%
  filter(outcome!="IgG+") %>%
  mutate(outcome=case_when(
    outcome=="IgM+ o IgG+"~"Prueba Rápida (IgM+ o IgG+)",
    outcome=="IgM+ o IgG+ o PCR+"~"Prueba Rápida (IgM+ o IgG+) o PCR+")) %>% 
  
  ggplot_prevalence() +
  theme(axis.text.x = element_text(angle = 0, vjust = 0, hjust=0)) +
  coord_flip() +
  facet_grid(covariate~.,scales = "free_y") +
  colorspace::scale_color_discrete_qualitative() +
  labs(title = "Prevalencia de SARS-CoV-2",
       subtitle = "En Lima Metropolitana y Callao, Julio 2020",
       y = "Prevalencia",x = "",
       color = "Prueba"#,size = "CV%"
  )
ggsave("figure/33-seroprev-figure01.png",height = 14,width = 7,dpi = "retina")

figura01 %>% write_rds("data/33-seroprev-figure01.rds")

# __fig02: edad decenio -----------------------------------------------------------------


figura02 <- out0104 %>% 
  tidy_srvyr_tibble() %>% 
  cdc_srvyr_tibble_03() %>% 
  union_all(out0204 %>% 
              tidy_srvyr_tibble() %>% 
              cdc_srvyr_tibble_03()) %>% 
  union_all(out0304 %>% 
              tidy_srvyr_tibble() %>% 
              cdc_srvyr_tibble_03()) %>% 
  union_all(out0404 %>% 
              tidy_srvyr_tibble() %>% 
              cdc_srvyr_tibble_03()) %>% 
  mutate(outcome=fct_relevel(outcome,"igm","igg","ig_clasificacion")) %>% 
  mutate(outcome=fct_recode(outcome,"IgM+"="igm","IgG+"="igg",
                            "IgM+ o IgG+"="ig_clasificacion",
                            "IgM+ o IgG+ o PCR+"="positividad_peru",
                            ))
figura02 %>% 
  cdc_srvyr_create_table(estim_digits = 2,ciupp_digits = 2) %>% 
  # select(prevalence_tab)
  writexl::write_xlsx("table/33-seroprev-figura02.xlsx")

figura02 %>% 
  
  filter(outcome!="IgM+") %>%
  filter(outcome!="IgG+") %>%
  mutate(outcome=case_when(
    outcome=="IgM+ o IgG+"~"Prueba Rápida (IgM+ o IgG+)",
    outcome=="IgM+ o IgG+ o PCR+"~"Prueba Rápida (IgM+ o IgG+) o PCR+")) %>% 
  
  ggplot_prevalence() +
  colorspace::scale_color_discrete_qualitative() +
  labs(title = "Prevalencia de SARS-CoV-2 por Prueba y Edad",
       subtitle = "En Lima Metropolitana y Callao, Julio 2020",
       y = "Prevalencia",x = "Edad por Decenios",
       color = "Prueba"#,size = "CV%"
       )
ggsave("figure/33-seroprev-figure02.png",height = 4,width = 6.5,dpi = "retina")

# out0105 %>% 
#   tidy_srvyr_tibble() %>% 
#   ggplot_prevalence()



# __fig03: espacial diris -----------------------------------------------------------------

library(sf)
library(ggspatial)
shapes_diris <- read_rds("data/per4-shp_distritos_janitor_diris.rds") %>% 
  st_as_sf(crs = 4610, agr = "constant") %>% 
  count(diris) 

figura03 <- out0106 %>% 
  tidy_srvyr_tibble() %>% 
  cdc_srvyr_tibble_03() %>% 
  union_all(out0206 %>% 
              tidy_srvyr_tibble() %>% 
              cdc_srvyr_tibble_03()) %>% 
  union_all(out0306 %>% 
              tidy_srvyr_tibble() %>% 
              cdc_srvyr_tibble_03()) %>% 
  union_all(out0406 %>% 
              tidy_srvyr_tibble() %>% 
              cdc_srvyr_tibble_03()) %>% 
  left_join(shapes_diris %>% select(-n,category=diris)) %>% 
  mutate(outcome=fct_relevel(outcome,"igm","igg","ig_clasificacion")) %>% 
  mutate(outcome=fct_recode(outcome,"IgM+"="igm","IgG+"="igg",
                            "IgM+ o IgG+"="ig_clasificacion",
                            "IgM+ o IgG+ o PCR+"="positividad_peru",
  )) %>% 
  cdc_srvyr_create_table() %>% 
  mutate(category=if_else(category=="CALLAO",category,str_replace(category,"DIRIS (.+)","LIMA \\1"))) %>% 
  mutate(prevalence_map=str_c(category,"\n",prevalence))

# figura03 %>% select(-geometry) %>% select(prevalence,prevalence_tab)
figura03 %>% select(-geometry) %>% writexl::write_xlsx("table/33-seroprev-figura03.xlsx")

figure_03_map <- figura03 %>%
  # select(proportion)
  
  filter(outcome!="IgM+") %>%
  filter(outcome!="IgG+") %>% 
  mutate(outcome=case_when(
    outcome=="IgM+ o IgG+"~"Prueba Rápida (IgM+ o IgG+)",
    outcome=="IgM+ o IgG+ o PCR+"~"Prueba Rápida (IgM+ o IgG+) o PCR+"))

figure_03_map %>% count(outcome)

figure_03_map %>%
  
  st_as_sf(crs = 4610, agr = "constant") %>% 
  ggplot() +
  geom_sf(aes(fill=proportion),colour = NA) +
  coord_sf() +
  # coord_sf(datum = NA) +
  facet_grid(~outcome) +
  # scale_fill_viridis_c()
  colorspace::scale_fill_continuous_sequential("Reds 3",
                                               limits = c(0.18,0.30),
                                               # rev = F,
                                               labels=scales::percent_format(accuracy = 1)) +
  # colorspace::scale_color_continuous_sequential(palette = "Grays",
  #                                               rev = F,
  #                                               guide = F) +
  # ggsflabel::geom_sf_label_repel(aes(label=prevalence_map,
  #                                    fill=proportion,
  #                                    color=proportion),
  #                                size=3,
  #                                fontface = "bold") +
  ggsflabel::geom_sf_label(data = figure_03_map %>% 
                                   filter(category=="CALLAO"|
                                            category=="LIMA NORTE" & 
                                            outcome=="Prueba Rápida (IgM+ o IgG+) o PCR+"),
                                 aes(label=prevalence_map,
                                     fill=proportion),
                                 color="white",
                                 size=3,
                                 fontface = "bold") +
  ggsflabel::geom_sf_label(data = figure_03_map %>% 
                                   filter(!(category=="CALLAO"|
                                            category=="LIMA NORTE" & 
                                            outcome=="Prueba Rápida (IgM+ o IgG+) o PCR+")),
                                 aes(label=prevalence_map,
                                     # color=proportion,
                                     fill=proportion),
                                 color="black",
                                 size=3,
                                 fontface = "bold") +
  # scale_x_continuous(breaks = scales::pretty_breaks(n = 3)) +
  labs(title = "Prevalencia de SARS-CoV-2 por Prueba y DIRIS",
       subtitle = "En Lima Metropolitana y Callao, Julio 2020",
       y = "Latitud",x = "Longitud",
       fill = "Prevalencia"#,size = "CV%"
  ) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", 
                         which_north = "true",
                         pad_x = unit(0.5, "in"),
                         pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
ggsave("figure/33-seroprev-figure03.png",height = 9,width = 10,dpi = "retina")


# __________ --------------------------------------------------------------


# CORRECCION Sen/Spe ------------------------------------------------------

# __fig00 -----------------------------------------------------------------

# ____ setup --------------------------------------------------------------


figura00_pre <- outcome_01 %>% 
  union_all(outcome_02) %>% 
  union_all(outcome_03) %>% 
  union_all(outcome_04) %>% 
  # round numbers are required
  mutate_at(.vars = vars(total,sum_total),.funs = round,digits = 0) %>%
  # select(1:4) %>% 
  #segun validacion local ins
  mutate(se_loc=case_when(
    outcome=="ig_clasificacion"~0.999,
    outcome=="igg"~0.999,
    outcome=="igm"~0.999,
    outcome=="positividad_peru"~NA_real_
  ),
  sp_loc=case_when(
    outcome=="ig_clasificacion"~0.96,
    outcome=="igg"~0.999,
    outcome=="igm"~0.96,
    outcome=="positividad_peru"~NA_real_
  )
  ) %>% 
  #segun fabricante
  mutate(se_fab=case_when(
    outcome=="ig_clasificacion"~0.9694,
    outcome=="igg"~0.9694,
    outcome=="igm"~0.9694,
    outcome=="positividad_peru"~NA_real_
  ),
  sp_fab=case_when(
    outcome=="ig_clasificacion"~0.9574,
    outcome=="igg"~0.9574,
    outcome=="igm"~0.9574,
    outcome=="positividad_peru"~NA_real_
  )
  )

# ____ apply serological sampling -----------------------------------------

plan(multisession, workers = availableCores())
# tic()
figura00_adj <- figura00_pre %>% 
  filter(category=="overall") %>% 
  # slice(1) %>% 
  filter(!is.na(se_loc)) %>% 
  
  #' 
  #' set validacion local
  #' 
  # mutate(adj_loc=pmap(.l = select(.,
  mutate(adj_loc=future_pmap(.l = select(.,
                                  positive_number_test=total,
                                  total_number_test=sum_total,
                                  sensitivity=se_loc,
                                  specificity=sp_loc),
                      .f = possibly(seroprevalence_posterior,otherwise = NA_real_))) %>% 
  mutate(adj_loc_rg=future_pmap_dbl(.l = select(.,
                                                prev.obs=proportion, 
                                                Se=se_loc, Sp=sp_loc),
                                .f = rogan_gladen_estimator)) %>% 
  #' 
  #' set validacion empresa
  #' 
  # mutate(adj_fab=pmap(.l = select(.,
  mutate(adj_fab=future_pmap(.l = select(.,
                                  positive_number_test=total,
                                  total_number_test=sum_total,
                                  sensitivity=se_fab,
                                  specificity=sp_fab),
                      .f = possibly(seroprevalence_posterior,otherwise = NA_real_))) %>% 
  mutate(adj_fab_rg=future_pmap_dbl(.l = select(.,
                                                prev.obs=proportion, 
                                                Se=se_fab, Sp=sp_fab),
                                    .f = rogan_gladen_estimator)) 
# toc()
# ____ create table -------------------------------------------------------

figura00_adj_loc <- figura00_adj %>% 
  select(1:6, ends_with("_loc")) %>% 
  cdc_srvyr_create_table_free(estim_var = proportion,
                              cilow_var = proportion_low,
                              ciupp_var = proportion_upp,
                              estim_digits = 3,
                              cilow_digits = 3,
                              ciupp_digits = 3) %>% 
  select(-estim_tab,-cilow_tab,-ciupp_tab,
         -proportion,-proportion_low,-proportion_upp) %>% 
  rename(proportion_tab=fused_tab) %>% 
  unnest(adj_loc) %>%
  unnest(summary) %>%
  cdc_srvyr_create_table_free(estim_var = numeric.mean,
                              cilow_var = numeric.p05,
                              ciupp_var = numeric.p95,
                              estim_digits = 4,
                              cilow_digits = 3,
                              ciupp_digits = 3) %>%
  # glimpse()
  select(-posterior,-skim_variable,
         -estim_tab,-cilow_tab,-ciupp_tab,
         -starts_with("numeric.")) %>% 
  rename("loc_prev_90pct_credibility_interval"=fused_tab)

figura00_adj_fab <- figura00_adj %>% 
  select(1:6, ends_with("_fab")) %>% 
  cdc_srvyr_create_table_free(estim_var = proportion,
                              cilow_var = proportion_low,
                              ciupp_var = proportion_upp,
                              estim_digits = 3,
                              cilow_digits = 3,
                              ciupp_digits = 3) %>% 
  select(-estim_tab,-cilow_tab,-ciupp_tab,
         -proportion,-proportion_low,-proportion_upp) %>% 
  rename(proportion_tab=fused_tab) %>% 
  unnest(adj_fab) %>%
  unnest(summary) %>%
  cdc_srvyr_create_table_free(estim_var = numeric.mean,
                              cilow_var = numeric.p05,
                              ciupp_var = numeric.p95,
                              estim_digits = 3,
                              cilow_digits = 3,
                              ciupp_digits = 4) %>%
  # glimpse()
  select(-posterior,-skim_variable,
         -estim_tab,-cilow_tab,-ciupp_tab,
         -starts_with("numeric.")) %>% 
  rename("fab_prev_90pct_credibility_interval"=fused_tab)

figura00_adj_loc %>% 
  writexl::write_xlsx("table/33-seroprev-figura04-loc.xlsx")

figura00_adj_fab %>% 
  writexl::write_xlsx("table/33-seroprev-figura04-fab.xlsx")


# temporary ---------------------------------------------------------------

adj_prev_table <- figura00_pre %>% 
  filter(category=="overall") %>% 
  select(1:6) %>% 
  cdc_srvyr_create_table_free(estim_var = proportion,
                              cilow_var = proportion_low,
                              ciupp_var = proportion_upp,
                              estim_digits = 3,
                              cilow_digits = 3,
                              ciupp_digits = 3) %>%
  # glimpse()
  select(-estim_tab,-cilow_tab,-ciupp_tab#,
         # -starts_with("proportion")
         ) %>% 
  rename("adj_prevalence"=fused_tab)

raw_prop_table %>% writexl::write_xlsx("table/33-seroprev-figura04-raw.xlsx")
adj_prev_table %>% writexl::write_xlsx("table/33-seroprev-figura04-adj.xlsx")
figura00_adj_loc
