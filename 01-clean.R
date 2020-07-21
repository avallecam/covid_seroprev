#' 
#' OBJETIVOS:
#' - make preliminary cleaning previous to QC evaluation
#' - make oficial cleaned db prior to export
#' write_csv(str_c("data/seroprev-hh-",my_timestap,".csv"))
#' write_csv(str_c("data/seroprev-pp-",my_timestap,".csv"))
#' write_rds("data/uu_raw_data.rds")

# packages ----------------------------------------------------------------

library(tidyverse)
library(avallecam)
library(magrittr)

# input -------------------------------------------------------------------

file_name <- avallecam::read_lastfile(path = "data-raw/",pattern = ".xlsx")

my_timestap <- str_replace(file_name,".+(...................)\\.xlsx","\\1") %>% 
  janitor::make_clean_names()
# my_timestap <- janitor::make_clean_names(Sys.Date())
readxl::excel_sheets(file_name)

retorno_ins <-  read_rds("data/retorno_ins.rds")


# ubigeo-diris-district-reunis ---------------------------------------------------

reunis_rute <- "../asis_repositorio/denom_r/data-raw/Poblacion Peru 2020 Dpto Prov Dist Final INEI-actualizado.xlsx"
diris_diccionario <- readxl::read_excel(reunis_rute,sheet = 4,skip = 6) %>% 
  janitor::clean_names() %>% 
  select(ubigeo:distrito)

ubigeo_diccionario <- 
  read_rds("../asis_repositorio/shape_r/data/per4-shp_distritos_janitor.rds") %>% 
  as_tibble() %>% 
  select(starts_with("cd_"),starts_with("nm_"),-ends_with("_t")) %>% 
  # crear columna de union a kobo
  mutate(peru_dist=cd_dist) %>% 
  # identificar pertenencia a diris
  left_join(diris_diccionario,by = c("peru_dist"="ubigeo")) %>% 
  mutate(diris=if_else(nm_prov=="callao","CALLAO",diris)) %>% 
  # asignar ipress a cada diris
  mutate(ipress=case_when(
    diris == "DIRIS CENTRO" ~ "00006204",
    diris == "DIRIS SUR" ~ "00005991",
    diris == "DIRIS NORTE" ~ "00005791",
    diris == "DIRIS ESTE" ~ "00013937",
    diris == "CALLAO" ~ "00013972",
    TRUE~NA_character_
  )) %>% 
  mutate(ipress_name=case_when(
    diris == "DIRIS CENTRO" ~ "LABORATORIO DE REFERENCIA DE SALUD PUBLICA",
    diris == "DIRIS SUR" ~ "CENTRO MATERNO INFANTIL DE SALUD VIRGEN DEL CARMEN",
    diris == "DIRIS NORTE" ~ "CENTRO MATERNO INFANTIL TAHUANTINSUYO BAJO",
    diris == "DIRIS ESTE" ~ "LABORATORIO DE REFERENCIA EN SALUD PUBLICA",
    diris == "CALLAO" ~ "DIRECCION DE LABORATORIO DE SALUD PUBLICA",
    TRUE~NA_character_
  ))
# mutate(nm_depa=if_else(nm_prov=="lima","lima_metropolitana",nm_depa))

ubigeo_diccionario %>% 
  filter(nm_prov=="lima") %>%
  # filter(nm_prov=="callao") %>% 
  avallecam::print_inf()

ubigeo_diccionario %>% 
  count(diris,ipress,ipress_name)

# csv household -----------------------------------------------------------

hh_raw_data <- readxl::read_excel(file_name,sheet = 1) %>% 
  mutate_at(.vars = vars(starts_with("peru_")),.funs = as.factor)

hh_raw_data %>% 
  write_csv(str_c("data/seroprev-hh-",my_timestap,".csv"))

hh_raw_data %>% glimpse()

# csv subject -------------------------------------------------------------

pp_raw_data <- readxl::read_excel(file_name,sheet = 2) %>% 
  select(-3)

pp_raw_data %>% 
  # colnames()
  write_csv(str_c("data/seroprev-pp-",my_timestap,".csv"))

pp_raw_data %>% glimpse()


# missings ----------------------------------------------------------------

hh_raw_data %>% 
  naniar::miss_var_summary() %>% 
  avallecam::print_inf()

# _______________ ---------------------------------------------------------


# INFO VIVIENDA seccion 2 -------------------------------------------------


# __ evaluacion de criterio para retirar replcia a nivel de jefe d --------


# hh_raw_data %>% 
#   filter(`_index`=="637") %>% 
#   glimpse()
# pp_raw_data %>% 
#   filter(`_parent_index`=="637") %>% 
#   filter(jefe_si_no=="si") %>% 
#   glimpse()

# __ creación ----------------------------------------------------------------


vv_raw_data <- pp_raw_data %>% #HECHO: AGREGAR NÚMERO DE HOGAR y verificar diferencia!
  # glimpse()
  # OJO: retiramos duplicado en jefe de vivienda
  # conservamos según criterio de edad y nivel educativo
  filter(!(`_parent_index`=="637" & `_index`=="2437")) %>% 
  select(`_parent_index`,numero_hogar,
          tipo_vivienda,
         agua,
         desague,
         electricidad,
         nro_dormitorios,
         nro_convivientes) %>% 
  group_by(`_parent_index`) %>% 
  filter(!is.na(nro_convivientes)) %>% 
  ungroup() %>% 
  distinct()

# ver duplicados - NO
vv_raw_data %>% 
  group_by(`_parent_index`,numero_hogar) %>% 
  filter(n()>1) %>% 
  avallecam::print_inf()
# para recuperar valores perdidos por nro docrmitorios y nro convivientes
# colocar personas entrevistadas por vivienda

# pp_raw_data %>% glimpse()
#   select(starts_with("material_"),starts_with("hogar"),starts_with("cocina"))

# __________ --------------------------------------------------------------


# UNION VIVIENDA - SUJETO -------------------------------------------------


# OJO ---------------------------------------------------------------------


# hh_raw_data %>% 
#   count(`_index`) %>% 
#   avallecam::print_inf()
# pp_raw_data %>% 
#   count(`_parent_index`) %>% 
#   avallecam::print_inf()

anti_join(hh_raw_data, #813
          pp_raw_data, #3117
          by=c("_index"="_parent_index")) %>% 
  select(-peru_depa,-peru_prov,-(direccion:longitud),
         -(id:`_uuid`),-`_validation_status`) %>% 
  arrange(conglomerado) %>% 
  avallecam::print_inf()

# hh_raw_data %>% select(id) %>% naniar::miss_var_summary()

inner_join(hh_raw_data, #813
          pp_raw_data, #3117
          by=c("_index"="_parent_index")) %>% 
  # filter(conglomerado=="4724602") %>% 
  filter(conglomerado=="17940") %>%
  select(-peru_depa,-peru_prov,-(direccion:longitud),
         -(id:`_uuid`),-`_validation_status`) %>% 
  count(conglomerado,numero_vivienda)

# union all ---------------------------------------------------------------

uu_raw_data <- left_join(pp_raw_data, #3117
                         hh_raw_data, #813
                         by=c("_parent_index"="_index")) %>% #3117 -> HECHO: ver si personas = 0 o son duplicados
  # naniar::vis_miss()
  select(#`_parent_index`,
         -tipo_vivienda,
         -agua,
         -desague,
         -electricidad,
         -nro_dormitorios,
         -nro_convivientes) %>% 
  full_join(vv_raw_data) %>% #746 -> #conserva observaciones
  #recuperacion de datos de vivienda
  # filter(nro_convivientes=="999") %>% 
  # select(participante,nro_convivientes) %>% 
  mutate(nro_convivientes=if_else(condition = nro_convivientes=="999",
                                  true = participante,
                                  false = nro_convivientes)) %>% 
  # fix ubigeo-inei
  mutate(peru_dist=as.character(peru_dist)) %>% 
  mutate(peru_dist=case_when(
    str_length(peru_dist)==5~str_replace(peru_dist,"(.+)","0\\1"),
    TRUE~peru_dist
  )) %>% 
  left_join(ubigeo_diccionario) %>% #conserva observaciones
  select(-peru_depa,-peru_prov) %>% 
  
  mutate(conglomerado=str_replace_all(conglomerado,"\n","")) %>% 
  mutate(conglomerado=str_replace_all(conglomerado," ","")) %>% 
  
  # clasificacion global
  mutate(ig_clasificacion=case_when(
    is_in(resultado_pr,c("igg","igm_igg","igm")) ~ "positivo",
    is_in(resultado_pr2,c("igg","igm_igg","igm")) ~ "positivo",
    resultado_pr=="negativo" ~ "negativo",
    resultado_pr2=="negativo" ~ "negativo",
    # resultado_pr=="indeterminado" ~ "indeterminado",
    is.na(resultado_pr) ~ "missing"
  )) %>% 
  
  # clasificacion individual
  mutate(
    igg=case_when(
      resultado_pr=="igg"~"positivo",
      resultado_pr=="igm_igg"~"positivo",
      resultado_pr2=="igg"~"positivo",
      resultado_pr2=="igm_igg"~"positivo",
      ig_clasificacion=="missing"~"missing",
      TRUE~"negativo"),
    igg_igm=case_when(
      resultado_pr=="igm_igg"~"positivo",
      resultado_pr2=="igm_igg"~"positivo",
      ig_clasificacion=="missing"~"missing",
      TRUE~"negativo"),
    igm=case_when(
      resultado_pr=="igm"~"positivo",
      resultado_pr2=="igm"~"positivo",
      resultado_pr=="igm_igg"~"positivo",
      resultado_pr2=="igm_igg"~"positivo",
      ig_clasificacion=="missing"~"missing",
      TRUE~"negativo")
         # ind=if_else(resultado_pr=="indeterminado",
         #             "indeterminado",NA_character_),
         # neg=if_else(resultado_pr=="negativo",
         #             "negativo",NA_character_)
  ) %>% 
  
  #create age categories
  mutate(edad=as.numeric(edad)) %>% 
  cdcper::cdc_edades_peru(edad) %>% 
  
  # resultados laboratorio
  left_join(retorno_ins) #conserva observaciones

# PRUEBAS POST ------------------------------------------------------------


# [*] conteo ESP vs OBS --------------------------------------------

# __ nivel vivienda -------------------------------------------------------


uu_raw_data %>% 
  # glimpse()
  # filter(is_in(conglomerado,si_vinculado$conglomerado)) %>% 
  select(nm_dist,cd_dist,conglomerado,numero_vivienda,
         #numero_hogar,
         participante) %>% 
  group_by(nm_dist,cd_dist,conglomerado,numero_vivienda,participante) %>% 
  summarise(n_registros=n()) %>% 
  ungroup() %>% 
  mutate(diferencia_E_O=n_registros-as.integer(participante)) %>% 
  # # left_join(vv_raw_data)
  # filter(diferencia_E_O<0) %>% 
  # arrange(diferencia_E_O) %>% 
  # avallecam::print_inf()
  # # skimr::skim(diferencia_E_O)
  write_rds("data/uu_raw_data-summary_vivienda-diferencia_esperado_observado.rds")


# __ nivel hogar -------------------------------------------------------------

uu_raw_data %>% 
  # glimpse()
  # filter(is_in(conglomerado,si_vinculado$conglomerado)) %>% 
  select(nm_dist,cd_dist,conglomerado,numero_vivienda,
         numero_hogar,
         participante,`_parent_index`,
         nro_dormitorios,nro_convivientes) %>% 
  group_by(`_parent_index`,nm_dist,cd_dist,conglomerado,
           numero_vivienda,numero_hogar,participante,
           nro_dormitorios,nro_convivientes) %>% 
  summarise(n_registros=n()) %>% 
  ungroup() %>% 
# recuperación lograda en nro de convivientes
  # # left_join(vv_raw_data) %>% 
  # select(`_parent_index`:n_registros,nro_dormitorios,nro_convivientes) %>% 
  # mutate_at(.vars = vars(participante:nro_convivientes),.funs = as.integer) %>% 
  # select(participante:nro_convivientes) %>% 
  # filter(nro_dormitorios>50)
  # # skimr::skim()
  write_rds("data/uu_raw_data-summary_hogar-registros_por_hogar.rds")
  
# cruce pruebas -----------------------------------------------------------


uu_raw_data %>% 
  count(presente_prueba,resultado_pr,resultado_pr2,
        ig_clasificacion,
        #igg,igm,igg_igm
        tipo_muestra_pcr,convResultado
        ) %>% 
  avallecam::print_inf()

uu_raw_data %>% 
  naniar::miss_var_summary() %>% 
  avallecam::print_inf()

# uu_raw_data %>% 
#   select(`_id`,`_uuid`,`_submission__id`) %>% 
#   skimr::skim()
# 
# uu_raw_data %>% 
#   select(`_submission__id`) %>% 
#   mutate(`_submission__id`=as.character(`_submission__id`)) %>% 
#   count(`_submission__id`,sort = T)
#   skimr::skim()

# uu_raw_data %>% 
#   select(convResultado,sintomas_si_no,sintomas) %>% 
#   naniar::miss_var_summary()

uu_raw_data %>%
  count(sintomas_si_no,ig_clasificacion,tipo_muestra_pcr,convResultado)
  # janitor::tabyl(sintomas_si_no,convResultado) %>% 
  # avallecam::adorn_ame()

# RAW: write to data -----------------------------------------------------------

uu_raw_data %>% 
  rownames_to_column() %>% 
  write_rds("data/uu_raw_data.rds")


# _________________ -------------------------------------------------------


# CLEAN: create new covariates --------------------------------------------


uu_raw_data <- read_rds("data/uu_raw_data.rds") #3117
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

uu_raw_data %>% 
  count(cd_dist,conglomerado,numero_vivienda) %>% 
  count(cd_dist,conglomerado)

uu_clean_data <- uu_raw_data %>% #3117 
  
  # justificación: sin ningún resultado de prueba no hay outcome
  # va al flujograma de numero de participantes
  # se van 17 observaciones
  filter(presente_prueba=="si") %>% #3100
  
  left_join(diccionario_conglomerado,by = c("conglomerado","cd_dist"="ubigeo")) %>% 
  
  #factor to be used
  mutate_at(.vars = vars(nm_dist,cd_dist,sexo),.funs = as.factor) %>% 
  
  # justificación: debe tener conglomerado
  filter(!is.na(totvivsel)) %>% #conserva observaciones
  
  left_join(diccionario_ponderaciones,by = c("conglomerado","cd_dist"="ubigeo")) %>% 
  
  # PENDIENTE: VERIFICAR SI ES UN MISSING NO RECUPERABLE
  # justificación: estuvo en prueba pero no se colectó ningún resultado
  # se va 1
  filter(ig_clasificacion!="missing") %>% #3099
  
  mutate(ig_clasificacion=as.factor(ig_clasificacion),
         diris=as.factor(diris),
         convResultado=as.factor(convResultado),
         sintomas_si_no=as.factor(sintomas_si_no)) %>% 
  
  # identificación de elementos del diseño
  mutate(CONGLOMERADO= conglomerado, #PSU
         VIVIENDA= numero_vivienda, #SSU
         ESTRATO = cd_dist, #ESTRATO C PSU C SSU 
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
  )) %>% 
  
  # creacion de variables y recategorizacion
  
  # hacinamiento: redatam
  mutate(nro_dormitorios = as.numeric(nro_dormitorios),
         nro_convivientes = as.numeric(nro_convivientes)) %>% 
  mutate(ind_hacin = (nro_convivientes/nro_dormitorios)) %>% 
  # mutate(ind_hacin=if_else(condition = ind_hacin==Inf,
  #                          true = NA_real_,
  #                          false = ind_hacin)) %>%
  mutate(hacinamiento=case_when(ind_hacin>=0.1 & ind_hacin<=2.4~"Sin Hacinmaniento",
                                ind_hacin>=2.5 & ind_hacin<=20 ~"Con Hacinamiento")) %>% 
  mutate(hacinamiento=fct_relevel(hacinamiento,"Sin Hacinmaniento")) %>% 
  
  # pobreza: mef
  mutate(nbi_haci=case_when(ind_hacin>=0.1 & ind_hacin<=3.4~0,
                            ind_hacin>=3.5 & ind_hacin<=20 ~1),
         nbi_electricidad=case_when(electricidad=="si"~0,
                                    electricidad=="no"~1),
         nbi_agua=case_when(agua=="cisterna"~1,
                            agua=="otro"~1,
                            agua=="pilon_comun"~1,
                            agua=="pozo_subterraneo"~1,
                            agua=="red_publica_dentro"~0,
                            agua=="red_publica_fuera"~1),
         nbi_desague=case_when(desague=="letrina"~0,
                               desague=="otro"~1,
                               desague=="pozo_negro"~0,
                               desague=="pozo_septico"~0,
                               desague=="red_publica_dentro"~0,
                               desague=="red_publica_fuera"~1),
         nbi_ninos_sin_cole=case_when(!is.na(nivel_academico) & nivel_academico!="sin_nivel" ~0,
                                      edad>=6 & edad<=12 & nivel_academico!="sin_nivel"~1,
                                      edad>=6 & edad<=12 & nivel_academico=="sin_nivel"~1)) %>%  
  mutate(pre_educac=case_when(jefe_si_no=="no" & !is.na(nivel_academico)~0,
                              jefe_si_no=="si" & nivel_academico=="inicial"~1,
                              jefe_si_no=="si" & nivel_academico=="primaria_incomp"~1,
                              jefe_si_no=="si" & nivel_academico=="sin_nivel"~1)) %>%
  # submission_id es equivalente a parent_index
  # count(`_submission__id`)
  # count(`_parent_index`,`_submission__id`) %>% 
  # count(`_parent_index`,`_submission__id`,sort = T)
  group_by(`_submission__id`) %>% 
  mutate(nbi_educacion=sum(pre_educac,na.rm = T)) %>% 
  #do(mutate(.,nbi_educacion=length(pre_educac[!is.na(pre_educac)]))) %>% 
  ungroup() %>% 
  mutate(ind_pobreza=nbi_educacion+nbi_haci+nbi_ninos_sin_cole+nbi_electricidad+nbi_agua+nbi_desague) %>% 
  mutate(pobreza=case_when(ind_pobreza==0~"No pobre",
                           ind_pobreza==1~"Pobre",
                           ind_pobreza>=1~"Pobre extremo"))

# uu_clean_data %>% count(totvivsel)

# WRITE clean data --------------------------------------------------------


uu_clean_data %>% 
  write_rds("data/uu_clean_data.rds")

# outcomes! ---------------------------------------------------------------


uu_clean_data %>% 
  count(sintomas_cualquier_momento,sintomas_si_no,sintomas_previos)

uu_clean_data %>% 
  count(resultado_pr,resultado_pr2,ig_clasificacion,convResultado,positividad_peru)

# explorar ----------------------------------------------------------------

uu_clean_data %>% naniar::miss_var_summary()


# _________________ -------------------------------------------------------



# issues? -----------------------------------------------------------------

uu_raw_data %>% 
  filter(is.na(`_index`)) %>% 
  glimpse()

uu_raw_data %>% 
  filter(is.na(`_index`)) %>% 
  # count(agregar)
  filter(agregar=="si") %>% 
  # glimpse()
  count(username,`_submission_time`)

pp_raw_data %>% 
  filter(`_parent_index`==23)

# additional summaries ----------------------------------------------------

# hh_raw_data %>% 
uu_raw_data %>% 
  # glimpse()
  count(peru_dist,
        cd_depa,cd_prov,cd_dist,
        nm_depa,nm_prov,nm_dist,sort = T) %>%
  # select(cd_dist,starts_with("nm_"),n) %>% 
  avallecam::print_inf()

# hh_raw_data %>% 
#   count(conglomerado,sort = T) %>% 
#   avallecam::print_inf()


# rescatar geolocalizacion ------------------------------------------------

uu_raw_data %>% 
  select(contains("gps"),latitud,longitud) %>% 
  # naniar::vis_miss()
  filter(gps_dispo=="no") %>% 
  filter(!is.na(latitud)) %>% 
  avallecam::print_inf()
naniar::miss_var_summary()
# mutate()
# print(n=100)


# identificador unico? ----------------------------------------------------

# uu_raw_data %>% count(submission_id,id,sort = T)
# uu_raw_data %>% naniar::miss_var_summary() %>% avallecam::print_inf()
# uu_raw_data %>% count(id,uuid,submission_id,submission_uuid,sort = T)
# uu_raw_data %>% 
# select(nombres,apellido_materno,apellido_paterno,edad,sexo) %>% 
# mutate(hash=pmap(.l = select(.,nombres,apellido_materno,apellido_paterno,edad,sexo),
#                  .f = epitrix::hash_names)) %>% 
# unnest(cols = c(hash))


