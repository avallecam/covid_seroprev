library(tidyverse)
library(survey)
library(srvyr)
library(writexl)

uu_raw_data <- read_rds("data/uu_raw_data.rds")
# uu_raw_data %>% glimpse()

# inputs ------------------------------------------------------------------

diccionario_conglomerado <- read_rds("data/inei-diccionario_conglomerado.rds") %>% 
  group_by(ubigeo,distrito,codccpp_bd,zona1_bd,conglomerado#,manzana,longitud,latitud
  ) %>% 
  summarise(totvivsel=sum(totvivsel)) %>% 
  ungroup() 

diccionario_ponderaciones <- readxl::read_excel("data-raw/expansion/FACTOR EXPANSION COVID19 AJUSTADO-enviado-14-7-20.xlsx") %>% 
  janitor::clean_names() %>% 
  select(ubigeo,conglomerado=conglomeradofinal,mviv:factorfinal)

# diccionario_ponderaciones %>% 
#   count(ubigeo)
# 
# diccionario_ponderaciones %>% 
#   count(ubigeo,factorfinal)

# revisar categorizaciones ------------------------------------------------

uu_raw_data %>% 
  count(presente_prueba,resultado_pr,resultado_pr2,tipo_muestra_pcr,
        ig_clasificacion,igg,igm,igg_igm)

uu_clean_data <- uu_raw_data %>% 
  
  filter(presente_prueba=="si") %>% 
  
  left_join(diccionario_conglomerado,by = c("conglomerado","cd_dist"="ubigeo")) %>% 
  
  #factor to be used
  mutate_at(.vars = vars(nm_dist,cd_dist,sexo),.funs = as.factor) %>% 
  
  filter(!is.na(totvivsel)) %>% 
  
  left_join(diccionario_ponderaciones,by = c("conglomerado","cd_dist"="ubigeo")) %>% 
  
  filter(ig_clasificacion!="missing") %>% 
  mutate(ig_clasificacion=as.factor(ig_clasificacion),
         diris=as.factor(diris),
         convResultado=as.factor(convResultado),
         sintomas_si_no=as.factor(sintomas_si_no)) %>% 
  
  # temporal
  mutate(CONGLOMERADO= conglomerado,
         ESTRATO = cd_dist, 
         PONDERACION = factorfinal) %>% 
  
  #creacion extra
  # select(convResultado,ig_clasificacion,sintomas_si_no,sintomas_previos) %>% 
  #count(sintomas_previos)
  mutate(positividad_peru=case_when(
    convResultado=="POSITIVO" | ig_clasificacion=="positivo" ~ "positivo",
    convResultado=="NEGATIVO" | ig_clasificacion=="negativo" ~ "negativo",
    TRUE~"missing"
  )) %>% 
  mutate(sintomas_cualquier_momento=case_when(
    sintomas_si_no=="si" | sintomas_previos=="si" ~ "si",
    sintomas_si_no=="no" | sintomas_previos=="no" ~ "no",
    TRUE ~ "missing"
  ))

# uu_clean_data %>% count(totvivsel)

uu_clean_data %>% 
  count(sintomas_cualquier_momento,sintomas_si_no,sintomas_previos)

uu_clean_data %>% 
  count(resultado_pr,resultado_pr2,ig_clasificacion,convResultado,positividad_peru)

# explorar ----------------------------------------------------------------

uu_clean_data %>% naniar::miss_var_summary()

uu_clean_data %>% 
  filter(cd_dist=="070103") %>% 
  count(nm_dist,conglomerado)

temporary_just_1_psu<- uu_clean_data %>% 
  count(cd_dist,conglomerado,sort = T) %>% 
  count(cd_dist,sort = T) %>% 
  filter(n==1)

uu_clean_data %>% 
  janitor::tabyl(ig_clasificacion)

uu_clean_data %>% 
  janitor::tabyl(sexo,ig_clasificacion) %>% 
  avallecam::adorn_ame()

uu_clean_data %>% 
  select(ig_clasificacion,sexo,
         edad_etapas_de_vida_c,edad_decenios,edad_quinquenal,
         diris,convResultado,sintomas_si_no)

# descripción poblacional -------------------------------------------------


uu_raw_data %>% 
  naniar::miss_var_summary() %>% 
  avallecam::print_inf()

uu_raw_data %>%
  # select(edad,ig_clasificacion) %>%
  # mutate(edad=as.numeric(edad)) %>%
  # naniar::miss_var_summary()
  # cdcper::cdc_edades_peru(edad) %>%
  filter(ig_clasificacion!="missing") %>% 
  compareGroups::compareGroups(ig_clasificacion~.,data = .,max.xlev = 20,
                               chisq.test.perm = TRUE,byrow = T) %>%
  compareGroups::createTable(digits = 1,sd.type = 2,show.ratio = T,show.n = T) %>% 
  compareGroups::export2xls("table/01-compareGroups-output-01.xls")


# diseño muestral de la encuesta ---------------------------------

design <- uu_clean_data %>% 
  filter(!magrittr::is_in(cd_dist,temporary_just_1_psu$cd_dist)) %>%
  
  # as_factor() %>% #importar variable+value labels
  
  filter(!is.na(ig_clasificacion)) %>% #CRITICAL! ON OUTCOME
  
  as_survey_design(id = CONGLOMERADO, 
                   strata = ESTRATO, 
                   weights= PONDERACION)

# tablas de prevalencia ------


# 01_general ----------------------------------------------------------------

design %>%
  group_by(ig_clasificacion) %>%
  summarize(proportion = survey_mean(vartype = c("ci","cv")),
            # total = survey_total(),
            # n = unweighted(n())
  ) %>% 
  filter(ig_clasificacion=="positivo") %>% 
  write_xlsx("table/tab01-sarscov2-general.xlsx")

# 01_general ----------------------------------------------------------------

design %>%
  group_by(sexo,ig_clasificacion) %>%
  summarize(proportion = survey_mean(vartype = c("ci","cv")),
            # total = survey_total(),
            # n = unweighted(n())
  ) %>% 
  filter(ig_clasificacion=="positivo") %>% 
  write_xlsx("table/tab02-sarscov2-sexo.xlsx")


# 01_general ----------------------------------------------------------------

design %>%
  group_by(edad_etapas_de_vida_t,ig_clasificacion) %>%
  summarize(proportion = survey_mean(vartype = c("ci","cv")),
            # total = survey_total(),
            # n = unweighted(n())
  ) %>% 
  filter(ig_clasificacion=="positivo") %>% 
  write_xlsx("table/tab03-sarscov2-edad_etapas_vida-5c.xlsx")


# 01_general ----------------------------------------------------------------

design %>%
  group_by(edad_decenios,ig_clasificacion) %>%
  summarize(proportion = survey_mean(vartype = c("ci","cv")),
            # total = survey_total(),
            # n = unweighted(n())
  ) %>% 
  filter(ig_clasificacion=="positivo") %>% 
  write_xlsx("table/tab04-sarscov2-edad_decenios-10c.xlsx")

# 01_general ----------------------------------------------------------------

design %>%
  group_by(edad_quinquenal,ig_clasificacion) %>%
  summarize(proportion = survey_mean(vartype = c("ci","cv")),
            # total = survey_total(),
            # n = unweighted(n())
  ) %>% 
  filter(ig_clasificacion=="positivo") %>% 
  write_xlsx("table/tab05-sarscov2-edad_quinquenal-20c.xlsx")



# por diris ---------------------------------------------------------------

design %>%
  group_by(diris,ig_clasificacion) %>%
  summarize(proportion = survey_mean(vartype = c("ci","cv")),
            # total = survey_total(),
            # n = unweighted(n())
  ) %>% 
  filter(ig_clasificacion=="positivo") %>% 
  write_xlsx("table/tab06-sarscov2-diris.xlsx")


# 
# 
# 
# 
# # 01_general ----------------------------------------------------------------
# 
# design %>%
#   group_by(ig_clasificacion,nm_dist) %>%
#   summarize(proportion = survey_mean(vartype = c("ci","cv")),
#             # total = survey_total(),
#             # n = unweighted(n())
#   ) %>% 
#   filter(ig_clasificacion=="positivo") %>% 
#   arrange(desc(proportion)) %>% 
#   avallecam::print_inf()
# #write_xlsx("table/tab01-anemia.xlsx")
# 




# PCR ---------------------------------------------------------------------

# diseño muestral de la encuesta ---------------------------------

design2 <- uu_clean_data %>% 
  filter(!magrittr::is_in(cd_dist,temporary_just_1_psu$cd_dist)) %>%
  
  # as_factor() %>% #importar variable+value labels
  
  filter(!is.na(convResultado)) %>% #CRITICAL! ON OUTCOME
  
  as_survey_design(id = CONGLOMERADO, 
                   strata = ESTRATO, 
                   weights= PONDERACION)

# tablas de prevalencia ------


# 01_general ----------------------------------------------------------------

design2 %>%
  group_by(convResultado) %>%
  summarize(proportion = survey_mean(vartype = c("ci","cv")),
            # total = survey_total(),
            # n = unweighted(n())
  ) %>% 
  filter(convResultado=="POSITIVO") %>% 
  write_xlsx("table/tab07-sarscov2-general_pcr.xlsx")

# 01_general ----------------------------------------------------------------

design2 %>%
  group_by(sintomas_si_no,convResultado) %>%
  summarize(proportion = survey_mean(vartype = c("ci","cv")),
            # total = survey_total(),
            # n = unweighted(n())
  ) %>% 
  filter(convResultado=="POSITIVO") %>% 
  write_xlsx("table/tab08-sarscov2-general_pcr_sintomatico.xlsx")


# CUALQUIER PRUEBA CUALQUIER SINTOMA --------------------------------------

# diseño muestral de la encuesta ---------------------------------

design3 <- uu_clean_data %>% 
  filter(!magrittr::is_in(cd_dist,temporary_just_1_psu$cd_dist)) %>%
  
  # as_factor() %>% #importar variable+value labels
  
  filter(!is.na(positividad_peru)) %>% #CRITICAL! ON OUTCOME
  filter(!is.na(edad_decenios)) %>% 
  
  as_survey_design(id = CONGLOMERADO, 
                   strata = ESTRATO, 
                   weights= PONDERACION)

# tablas de prevalencia ------

uu_clean_data %>%
  janitor::tabyl(positividad_peru)

uu_clean_data %>%
  janitor::tabyl(sintomas_cualquier_momento,positividad_peru) %>% 
  avallecam::adorn_ame()

# 01_general ----------------------------------------------------------------

design3 %>%
  group_by(positividad_peru) %>%
  summarize(proportion = survey_mean(vartype = c("ci","cv")),
            # total = survey_total(),
            # n = unweighted(n())
  ) %>% 
  filter(positividad_peru=="positivo") %>% 
  write_xlsx("table/tab09-sarscov2-general_cualquier_prueba.xlsx")

# 01_general ----------------------------------------------------------------

design3 %>%
  group_by(sintomas_cualquier_momento,positividad_peru) %>%
  summarize(proportion = survey_mean(vartype = c("ci","cv")),
            # total = survey_total(),
            # n = unweighted(n())
  ) %>% 
  filter(positividad_peru=="positivo") %>% 
  write_xlsx("table/tab10-sarscov2-general_cualquier_prueba_sintomas_cualquier_momento.xlsx")



# nuevo -------------------------------------------------------------------

# 01_general ----------------------------------------------------------------

design3 %>%
  group_by(positividad_peru) %>%
  summarize(proportion = survey_mean(vartype = c("ci","cv")),
            # total = survey_total(),
            # n = unweighted(n())
  ) %>% 
  filter(positividad_peru=="positivo") %>% 
  write_xlsx("table/tabpp-01-sarscov2-general.xlsx")

# 01_general ----------------------------------------------------------------

design3 %>%
  group_by(sexo,positividad_peru) %>%
  summarize(proportion = survey_mean(vartype = c("ci","cv")),
            # total = survey_total(),
            # n = unweighted(n())
  ) %>% 
  filter(positividad_peru=="positivo") %>% 
  write_xlsx("table/tabpp-02-sarscov2-sexo.xlsx")


# 01_general ----------------------------------------------------------------

design3 %>%
  group_by(edad_etapas_de_vida_t,positividad_peru) %>%
  summarize(proportion = survey_mean(vartype = c("ci","cv")),
            # total = survey_total(),
            # n = unweighted(n())
  ) %>% 
  filter(positividad_peru=="positivo") %>% 
  write_xlsx("table/tabpp-03-sarscov2-edad_etapas_vida-5c.xlsx")


# 01_general ----------------------------------------------------------------

design3 %>%
  group_by(edad_decenios,positividad_peru) %>%
  summarize(proportion = survey_mean(vartype = c("ci","cv")),
            # total = survey_total(),
            # n = unweighted(n())
  ) %>% 
  filter(positividad_peru=="positivo") %>% 
  write_xlsx("table/tabpp-04-sarscov2-edad_decenios-10c.xlsx")

# 01_general ----------------------------------------------------------------

design3 %>%
  group_by(edad_quinquenal,positividad_peru) %>%
  summarize(proportion = survey_mean(vartype = c("ci","cv")),
            # total = survey_total(),
            # n = unweighted(n())
  ) %>% 
  filter(positividad_peru=="positivo") %>% 
  write_xlsx("table/tabpp-05-sarscov2-edad_quinquenal-20c.xlsx")

