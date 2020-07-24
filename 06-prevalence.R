#'
#' OBJETIVO:
#' - crear tablas de prevalencia usando survey design
#' - temporalmente, creación y grabacion final de base limpia
#' write_rds("data/uu_clean_data.rds")
#' 
#' PENDIENTES:
#' (x) revisar eliminación de distritos
#' (x) revisar diseño en seteo
#' (x) pasar limpieza total a 01-clean.R
#' 
#' PENDIENTE
#' (x) proporcion cruda
#' (x) ajustado por ponderacion poblacional
#' (x) retirar cv (diferencias no analizables) pero da certeza de la inferencia
#' (-) crear grafico solo de edades. tabla con valores puntuales generales y sexo
#' (x) mapa con dos capas de label diris
#' ( ) ajustado por test performance: (sens 0.9694 | spec 0.9574)
#' ( ) reproduce gelman adjustment approach!


library(tidyverse)
library(survey)
library(srvyr)
library(writexl)

theme_set(theme_bw())

# inputs ------------------------------------------------------------------

uu_clean_data <- read_rds("data/uu_clean_data.rds") %>% 
  filter(edad_decenios!="[100,Inf]") %>% 
  mutate(igg=as.factor(igg),
         igm=as.factor(igm),
         positividad_peru=as.factor(positividad_peru))

# QC exposure | outcomes! ---------------------------------------------------------------

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

# ____________ ------------------------------------------------------------


# DESCRIPTIVO -------------------------------------------------------------


# descripción poblacional -------------------------------------------------


uu_clean_data %>% 
  naniar::miss_var_summary() %>% 
  avallecam::print_inf()

uu_clean_data %>%
  # select(edad,ig_clasificacion) %>%
  # mutate(edad=as.numeric(edad)) %>%
  # naniar::miss_var_summary()
  # cdcper::cdc_edades_peru(edad) %>%
  filter(ig_clasificacion!="missing") %>% 
  compareGroups::compareGroups(ig_clasificacion~.,data = .,max.xlev = 20,
                               chisq.test.perm = TRUE,byrow = T) %>%
  compareGroups::createTable(digits = 1,sd.type = 2,show.ratio = T,show.n = T) %>% 
  compareGroups::export2xls("table/01-compareGroups-output-01.xls")

# ____________ ------------------------------------------------------------

# SEROPREVALENCIA ---------------------------------------------------------

# tratamiento de stratos con un solo conglomerado
options(survey.lonely.psu = "certainty")

uu_clean_data %>% count(CONGLOMERADO,VIVIENDA)

# diseño muestral de la encuesta ---------------------------------

design <- uu_clean_data %>% 
  # filter(!magrittr::is_in(cd_dist,temporary_just_1_psu$cd_dist)) %>% #3041
  
  # as_factor() %>% #importar variable+value labels
  
  filter(!is.na(ig_clasificacion)) %>% #CRITICAL! ON OUTCOME
  filter(!is.na(factorfinal)) %>% #NO DEBEN DE HABER CONGLOMERADOS SIN WEIGHT
  
  as_survey_design(c(CONGLOMERADO, VIVIENDA),
                   #id = CONGLOMERADO, #clusters or psu (primary sampling unit) 
                   #add vivienda como *SSU*
                   strata = ESTRATO, #clusters need to be nested in the strata
                   weights= PONDERACION # factores de expancion
                   )

# tablas de prevalencia ------


# 01_general ----------------------------------------------------------------

out0101 <- design %>%
  group_by(ig_clasificacion) %>% #group_by
  summarize(proportion = survey_mean(vartype = c("ci","cv")),
            total = survey_total(),
            n = unweighted(n())
  ) %>% 
  mutate(p = prop.table(n),
         t = sum(n)) %>% 
  filter(ig_clasificacion=="positivo") 

out0101 #%>% write_xlsx("table/tab01-sarscov2-general.xlsx")

# 02_sexo ----------------------------------------------------------------

out0102 <- design %>%
  group_by(sexo,ig_clasificacion) %>% #group_by
  summarize(proportion = survey_mean(vartype = c("ci","cv")),
            total = survey_total(),
            n = unweighted(n())
  ) %>% 
  group_by(sexo) %>% #group_by
  mutate(p = prop.table(n),
         t = sum(n)) %>% 
  ungroup() %>% 
  filter(ig_clasificacion=="positivo") 

out0102 #%>% write_xlsx("table/tab02-sarscov2-sexo.xlsx")


# 03_edad: etapas ----------------------------------------------------------------

out0103 <- design %>%
  group_by(edad_etapas_de_vida_t,ig_clasificacion) %>% #group_by
  summarize(proportion = survey_mean(vartype = c("ci","cv")),
            total = survey_total(),
            n = unweighted(n())
  ) %>% 
  group_by(edad_etapas_de_vida_t) %>% #group_by
  mutate(p = prop.table(n),
         t = sum(n)) %>% 
  ungroup() %>% 
  filter(ig_clasificacion=="positivo") 

out0103 #%>% write_xlsx("table/tab03-sarscov2-edad_etapas_vida-5c.xlsx")


# 03_edad: decenio ----------------------------------------------------------------

out0104 <- design %>%
  group_by(edad_decenios,ig_clasificacion) %>% #group_by
  summarize(proportion = survey_mean(vartype = c("ci","cv")),
            total = survey_total(),
            n = unweighted(n())
  ) %>% 
  group_by(edad_decenios) %>% #group_by
  mutate(p = prop.table(n),
         t = sum(n)) %>% 
  ungroup() %>% 
  filter(ig_clasificacion=="positivo") 

out0104 #%>% write_xlsx("table/tab04-sarscov2-edad_decenios-10c.xlsx")

# 03_edad: quinquenio ----------------------------------------------------------------

out0105 <- design %>%
  group_by(edad_quinquenal,ig_clasificacion) %>% #group_by
  summarize(proportion = survey_mean(vartype = c("ci","cv")),
            total = survey_total(),
            n = unweighted(n())
  ) %>% 
  group_by(edad_quinquenal) %>% #group_by
  mutate(p = prop.table(n),
         t = sum(n)) %>% 
  ungroup() %>% 
  filter(ig_clasificacion=="positivo")

out0105 #%>% write_xlsx("table/tab05-sarscov2-edad_quinquenal-20c.xlsx")



# 04_espacio: diris ---------------------------------------------------------------

out0106 <- design %>%
  group_by(diris,ig_clasificacion) %>% #group_by
  summarize(proportion = survey_mean(vartype = c("ci","cv")),
            total = survey_total(),
            n = unweighted(n())
  ) %>% 
  group_by(diris) %>% #group_by
  mutate(p = prop.table(n),
         t = sum(n)) %>% 
  ungroup() %>% 
  filter(ig_clasificacion=="positivo")

out0106 #%>% write_xlsx("table/tab06-sarscov2-diris.xlsx")




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

out0201 <- design_02 %>%
  group_by(igg) %>% #group_by
  summarize(proportion = survey_mean(vartype = c("ci","cv")),
            total = survey_total(),
            n = unweighted(n())
  ) %>% 
  mutate(p = prop.table(n),
         t = sum(n)) %>% 
  filter(igg=="positivo") 

out0201 #%>% write_xlsx("table/tab01-sarscov2-general.xlsx")

# 02_sexo ----------------------------------------------------------------

out0202 <- design_02 %>%
  group_by(sexo,igg) %>% #group_by
  summarize(proportion = survey_mean(vartype = c("ci","cv")),
            total = survey_total(),
            n = unweighted(n())
  ) %>% 
  group_by(sexo) %>% #group_by
  mutate(p = prop.table(n),
         t = sum(n)) %>% 
  ungroup() %>% 
  filter(igg=="positivo") 

out0202 #%>% write_xlsx("table/tab02-sarscov2-sexo.xlsx")


# 03_edad: etapas ----------------------------------------------------------------

out0203 <- design_02 %>%
  group_by(edad_etapas_de_vida_t,igg) %>% #group_by
  summarize(proportion = survey_mean(vartype = c("ci","cv")),
            total = survey_total(),
            n = unweighted(n())
  ) %>% 
  group_by(edad_etapas_de_vida_t) %>% #group_by
  mutate(p = prop.table(n),
         t = sum(n)) %>% 
  ungroup() %>% 
  filter(igg=="positivo") 

out0203 #%>% write_xlsx("table/tab03-sarscov2-edad_etapas_vida-5c.xlsx")


# 03_edad: decenio ----------------------------------------------------------------

out0204 <- design_02 %>%
  group_by(edad_decenios,igg) %>% #group_by
  summarize(proportion = survey_mean(vartype = c("ci","cv")),
            total = survey_total(),
            n = unweighted(n())
  ) %>% 
  group_by(edad_decenios) %>% #group_by
  mutate(p = prop.table(n),
         t = sum(n)) %>% 
  ungroup() %>% 
  filter(igg=="positivo") 

out0204 #%>% write_xlsx("table/tab04-sarscov2-edad_decenios-10c.xlsx")

# 03_edad: quinquenio ----------------------------------------------------------------

out0205 <- design_02 %>%
  group_by(edad_quinquenal,igg) %>% #group_by
  summarize(proportion = survey_mean(vartype = c("ci","cv")),
            total = survey_total(),
            n = unweighted(n())
  ) %>% 
  group_by(edad_quinquenal) %>% #group_by
  mutate(p = prop.table(n),
         t = sum(n)) %>% 
  ungroup() %>% 
  filter(igg=="positivo")

out0205 #%>% write_xlsx("table/tab05-sarscov2-edad_quinquenal-20c.xlsx")



# 04_espacio: diris ---------------------------------------------------------------

out0206 <- design_02 %>%
  group_by(diris,igg) %>% #group_by
  summarize(proportion = survey_mean(vartype = c("ci","cv")),
            total = survey_total(),
            n = unweighted(n())
  ) %>% 
  group_by(diris) %>% #group_by
  mutate(p = prop.table(n),
         t = sum(n)) %>% 
  ungroup() %>% 
  filter(igg=="positivo")

out0206 #%>% write_xlsx("table/tab06-sarscov2-diris.xlsx")





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

out0301 <- design_03 %>%
  group_by(igm) %>% #group_by
  summarize(proportion = survey_mean(vartype = c("ci","cv")),
            total = survey_total(),
            n = unweighted(n())
  ) %>% 
  filter(igm=="positivo") 

out0301 #%>% write_xlsx("table/tab01-sarscov2-general.xlsx")

# 02_sexo ----------------------------------------------------------------

out0302 <- design_03 %>%
  group_by(sexo,igm) %>% #group_by
  summarize(proportion = survey_mean(vartype = c("ci","cv")),
            total = survey_total(),
            n = unweighted(n())
  ) %>% 
  group_by(sexo) %>% #group_by
  mutate(p = prop.table(n),
         t = sum(n)) %>% 
  ungroup() %>% 
  filter(igm=="positivo") 

out0302 #%>% write_xlsx("table/tab02-sarscov2-sexo.xlsx")


# 03_edad: etapas ----------------------------------------------------------------

out0303 <- design_03 %>%
  group_by(edad_etapas_de_vida_t,igm) %>% #group_by
  summarize(proportion = survey_mean(vartype = c("ci","cv")),
            total = survey_total(),
            n = unweighted(n())
  ) %>% 
  group_by(edad_etapas_de_vida_t) %>% #group_by
  mutate(p = prop.table(n),
         t = sum(n)) %>% 
  ungroup() %>% 
  filter(igm=="positivo") 

out0303 #%>% write_xlsx("table/tab03-sarscov2-edad_etapas_vida-5c.xlsx")


# 03_edad: decenio ----------------------------------------------------------------

out0304 <- design_03 %>%
  group_by(edad_decenios,igm) %>% #group_by
  summarize(proportion = survey_mean(vartype = c("ci","cv")),
            total = survey_total(),
            n = unweighted(n())
  ) %>% 
  group_by(edad_decenios) %>% #group_by
  mutate(p = prop.table(n),
         t = sum(n)) %>% 
  ungroup() %>% 
  filter(igm=="positivo") 

out0304 #%>% write_xlsx("table/tab04-sarscov2-edad_decenios-10c.xlsx")

# 03_edad: quinquenio ----------------------------------------------------------------

out0305 <- design_03 %>%
  group_by(edad_quinquenal,igm) %>% #group_by
  summarize(proportion = survey_mean(vartype = c("ci","cv")),
            total = survey_total(),
            n = unweighted(n())
  ) %>% 
  group_by(edad_quinquenal) %>% #group_by
  mutate(p = prop.table(n),
         t = sum(n)) %>% 
  ungroup() %>% 
  filter(igm=="positivo")

out0305 #%>% write_xlsx("table/tab05-sarscov2-edad_quinquenal-20c.xlsx")



# 04_espacio: diris ---------------------------------------------------------------

out0306 <- design_03 %>%
  group_by(diris,igm) %>% #group_by
  summarize(proportion = survey_mean(vartype = c("ci","cv")),
            total = survey_total(),
            n = unweighted(n())
  ) %>% 
  group_by(diris) %>% #group_by
  mutate(p = prop.table(n),
         t = sum(n)) %>% 
  ungroup() %>% 
  filter(igm=="positivo")

out0306 #%>% write_xlsx("table/tab06-sarscov2-diris.xlsx")





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

out0401 <- design_04 %>%
  group_by(positividad_peru) %>% #group_by
  summarize(proportion = survey_mean(vartype = c("ci","cv")),
            total = survey_total(),
            n = unweighted(n())
  ) %>% 
  mutate(p = prop.table(n),
         t = sum(n)) %>% 
  ungroup() %>% 
  filter(positividad_peru=="positivo") 

out0401 #%>% write_xlsx("table/tab01-sarscov2-general.xlsx")

# 02_sexo ----------------------------------------------------------------

out0402 <- design_04 %>%
  group_by(sexo,positividad_peru) %>% #group_by
  summarize(proportion = survey_mean(vartype = c("ci","cv")),
            total = survey_total(),
            n = unweighted(n())
  ) %>% 
  group_by(sexo) %>% #group_by
  mutate(p = prop.table(n),
         t = sum(n)) %>% 
  ungroup() %>% 
  filter(positividad_peru=="positivo") 

out0402 #%>% write_xlsx("table/tab02-sarscov2-sexo.xlsx")


# 03_edad: etapas ----------------------------------------------------------------

out0403 <- design_04 %>%
  group_by(edad_etapas_de_vida_t,positividad_peru) %>% #group_by
  summarize(proportion = survey_mean(vartype = c("ci","cv")),
            total = survey_total(),
            n = unweighted(n())
  ) %>% 
  group_by(edad_etapas_de_vida_t) %>% #group_by
  mutate(p = prop.table(n),
         t = sum(n)) %>% 
  ungroup() %>% 
  filter(positividad_peru=="positivo") 

out0403 #%>% write_xlsx("table/tab03-sarscov2-edad_etapas_vida-5c.xlsx")


# 03_edad: decenio ----------------------------------------------------------------

out0404 <- design_04 %>%
  group_by(edad_decenios,positividad_peru) %>% #group_by
  summarize(proportion = survey_mean(vartype = c("ci","cv")),
            total = survey_total(),
            n = unweighted(n())
  ) %>% 
  group_by(edad_decenios) %>% #group_by
  mutate(p = prop.table(n),
         t = sum(n)) %>% 
  ungroup() %>% 
  filter(positividad_peru=="positivo") 

out0404 #%>% write_xlsx("table/tab04-sarscov2-edad_decenios-10c.xlsx")

# 03_edad: quinquenio ----------------------------------------------------------------

out0405 <- design_04 %>%
  group_by(edad_quinquenal,positividad_peru) %>% #group_by
  summarize(proportion = survey_mean(vartype = c("ci","cv")),
            total = survey_total(),
            n = unweighted(n())
  ) %>% 
  group_by(edad_quinquenal) %>% #group_by
  mutate(p = prop.table(n),
         t = sum(n)) %>% 
  ungroup() %>% 
  filter(positividad_peru=="positivo")

out0405 #%>% write_xlsx("table/tab05-sarscov2-edad_quinquenal-20c.xlsx")



# 04_espacio: diris ---------------------------------------------------------------

out0406 <- design_04 %>%
  group_by(diris,positividad_peru) %>% #group_by
  summarize(proportion = survey_mean(vartype = c("ci","cv")),
            total = survey_total(),
            n = unweighted(n())
  ) %>% 
  group_by(diris) %>% #group_by
  mutate(p = prop.table(n),
         t = sum(n)) %>% 
  ungroup() %>% 
  filter(positividad_peru=="positivo")

out0406 #%>% write_xlsx("table/tab06-sarscov2-diris.xlsx")


# __________ --------------------------------------------------------------


# CORRECCION Sen/Spe ------------------------------------------------------

source("08-uncertainty_prev.R")

# ___________ -------------------------------------------------------------

# OUTPUTS -----------------------------------------------------------------

#' table with raw, population adjusted, test performance adjusted
#' figure 1 (overall, sex, age) showing the four outcomes!
#' figure 2 (age decenio) showing trend
#' figure 3 map per diris
#' 
#' more
#' performance of surveillance due to underreporting


# functions ---------------------------------------------------------------


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

# tables ------------------------------------------------------------------


outcome_01 <- out0101 %>% 
  union_all(out0102 %>% tidy_srvyr_tibble()) %>% 
  union_all(out0103 %>% tidy_srvyr_tibble()) %>% 
  union_all(out0106 %>% tidy_srvyr_tibble()) %>% 
  cdc_srvyr_tibble_02()

outcome_02 <- out0201 %>% 
  union_all(out0202 %>% tidy_srvyr_tibble()) %>% 
  union_all(out0203 %>% tidy_srvyr_tibble()) %>% 
  union_all(out0206 %>% tidy_srvyr_tibble()) %>% 
  cdc_srvyr_tibble_02()

outcome_03 <- out0301 %>% 
  union_all(out0302 %>% tidy_srvyr_tibble()) %>% 
  union_all(out0303 %>% tidy_srvyr_tibble()) %>% 
  union_all(out0306 %>% tidy_srvyr_tibble()) %>% 
  cdc_srvyr_tibble_02()

outcome_04 <- out0401 %>% 
  union_all(out0402 %>% tidy_srvyr_tibble()) %>% 
  union_all(out0403 %>% tidy_srvyr_tibble()) %>% 
  union_all(out0406 %>% tidy_srvyr_tibble()) %>% 
  cdc_srvyr_tibble_02()


# figure ------------------------------------------------------------------

# __fig01 -----------------------------------------------------------------

figura01 <- outcome_01 %>% 
  union_all(outcome_02) %>% 
  union_all(outcome_03) %>% 
  union_all(outcome_04) %>% 
  filter(covariate!="diris") %>% 
  mutate(covariate=if_else(category=="overall","overall",covariate)) %>% 
  mutate(covariate=fct_relevel(covariate,"overall","sexo")) %>% 
  mutate(covariate=fct_recode(covariate,
                              "Población General"="overall",
                              "Sexo Biológico"="sexo",
                              "Etapas de Vida"="edad_etapas_de_vida_t")) %>% 
  mutate(category=fct_relevel(category,"ninho","adolescente","joven","adulto")) %>% 
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

# figura01 %>% 
#   slice(1) %>% 
#   select(1:4,n:t) %>% 
#   mutate(fix=pmap(.l = select(.,n,t),
#                   .f = seroprevalence_posterior(positive_number_test = n,
#                                                  total_number_test = t,
#                                                  sensibility = 0.9694,
#                                                  specificity = 0.9574)))

figura01 %>% 
  cdc_srvyr_create_table(estim_digits = 2) %>% 
  # select(prevalence_tab)
  writexl::write_xlsx("table/33-seroprev-figura01.xlsx")

figura01 %>% 
  
  # filter(outcome!="IgM+") %>% 
  # filter(outcome!="IgG+") %>% 
  
  ggplot_prevalence() +
  # coord_flip() +
  facet_wrap(~covariate,scales = "free") +
  colorspace::scale_color_discrete_qualitative() +
  labs(title = "Prevalencia de SARS-CoV-2 por Prueba, Sexo y Edad",
       subtitle = "En Lima Metropolitana y Callao, Julio 2020",
       y = "Prevalencia",x = "",
       color = "Prueba"#,size = "CV%"
  )
ggsave("figure/33-seroprev-figure01.png",height = 4,width = 9,dpi = "retina")

# __fig02 -----------------------------------------------------------------


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
  
  # filter(outcome!="IgM+") %>% 
  # filter(outcome!="IgG+") %>% 
  
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



# __fig03 -----------------------------------------------------------------

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
