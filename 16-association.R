#'
#' OBJETIVES:
#' - create association estimates with survey design
#' - table and figure output
#' write_rds("data/____")
#' write_xlsx("table/____")
#' ggsave("figure/____")
#' 
#' PENDIENTE
#' - modelo múltiple adjusted por age + sex
#' - make tables outputs
#' - add them to supplementary report
#' - for ind_hacin: sub-analysis comparing general characteristics of missings with non-missings

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
  mutate(diris=fct_relevel(diris,"DIRIS CENTRO"))
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
# uu_clean_data %>% count(ind_hacin_cut2)

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
         ind_hacin,
         # ind_hacin_qrt,
         ind_hacin_cut2,
         hacinamiento,
         # nro_dormitorios_cat,
         nm_prov,
         sintomas_cualquier_momento_cat,
         # sintomas_cualquier_momento_cat_fecha_14d_v1,
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
         # etnia_cat,
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
  # mutate(across(c(#etnia_cat,
  #   seguro_salud,
  #   desague,
  #   agua#,
  #   #tipo_vivienda
  # ),
  # fct_infreq)) %>% 
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
                             show.n = T) #%>%
# compareGroups::export2xls("table/01-compareGroups-output-01.xls")
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

# more than one simple model ------------------------------------------------------------

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



