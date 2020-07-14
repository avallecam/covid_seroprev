library(tidyverse)
library(survey)
library(srvyr)

uu_raw_data <- read_rds("data/uu_raw_data.rds")
# uu_raw_data %>% glimpse()

# inputs ------------------------------------------------------------------

diccionario_conglomerado <- read_rds("data/inei-diccionario_conglomerado.rds") %>% 
  group_by(ubigeo,distrito,codccpp_bd,zona1_bd,conglomerado#,manzana,longitud,latitud
  ) %>% 
  summarise(totvivsel=sum(totvivsel)) %>% 
  ungroup() 

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
  
  # temporal
  mutate(CONGLOMERADO= conglomerado,
         ESTRATO = cd_dist, 
         PONDERACION = 1)

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
  filter(ig_clasificacion=="positivo") #%>% 
#write_xlsx("table/tab01-anemia.xlsx")

# 01_general ----------------------------------------------------------------

design %>%
  group_by(ig_clasificacion,sexo) %>%
  summarize(proportion = survey_mean(vartype = c("ci","cv")),
            # total = survey_total(),
            # n = unweighted(n())
  ) %>% 
  filter(ig_clasificacion=="positivo") #%>% 
#write_xlsx("table/tab01-anemia.xlsx")

# 01_general ----------------------------------------------------------------

design %>%
  group_by(ig_clasificacion,edad_decenios) %>%
  summarize(proportion = survey_mean(vartype = c("ci","cv")),
            # total = survey_total(),
            # n = unweighted(n())
  ) %>% 
  filter(ig_clasificacion=="positivo") #%>% 
#write_xlsx("table/tab01-anemia.xlsx")

# 01_general ----------------------------------------------------------------

design %>%
  group_by(ig_clasificacion,edad_quinquenal) %>%
  summarize(proportion = survey_mean(vartype = c("ci","cv")),
            # total = survey_total(),
            # n = unweighted(n())
  ) %>% 
  filter(ig_clasificacion=="positivo") #%>% 
#write_xlsx("table/tab01-anemia.xlsx")

# 01_general ----------------------------------------------------------------

design %>%
  group_by(ig_clasificacion,nm_dist) %>%
  summarize(proportion = survey_mean(vartype = c("ci","cv")),
            # total = survey_total(),
            # n = unweighted(n())
  ) %>% 
  filter(ig_clasificacion=="positivo") %>% 
  arrange(desc(proportion)) %>% 
  avallecam::print_inf()
#write_xlsx("table/tab01-anemia.xlsx")



# descripción poblacional -------------------------------------------------


uu_raw_data %>%
  # select(edad,ig_clasificacion) %>%
  # mutate(edad=as.numeric(edad)) %>%
  # naniar::miss_var_summary()
  # cdcper::cdc_edades_peru(edad) %>%
  filter(ig_clasificacion!="missing") %>% 
  compareGroups::compareGroups(ig_clasificacion~.,data = .,max.xlev = 20,
                               chisq.test.perm = TRUE,byrow = T) %>%
  compareGroups::createTable(digits = 1,sd.type = 2,show.ratio = T) %>% 
  compareGroups::export2xls("table/01-compareGroups-output-01.xls")
