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

# inputs ------------------------------------------------------------------

# _ CONGLOMERADOS -----------------------------------------------------------

diccionario_conglomerado <- read_rds("data/inei-diccionario_conglomerado.rds") %>% 
  group_by(ubigeo,distrito,codccpp_bd,zona1_bd,conglomerado#,manzana,longitud,latitud
  ) %>% 
  summarise(totvivsel=sum(totvivsel)) %>% 
  ungroup() 

diccionario_conglomerado %>% 
  filter(distrito=="ANCON")

# _ PONDERACIONES -----------------------------------------------------------


diccionario_ponderaciones <- 
  # readxl::read_excel("data-raw/expansion/FACTOR EXPANSION COVID19 AJUSTADO-enviado-14-7-20.xlsx") %>%
  # readxl::read_excel("data-raw/expansion/FACTOR EXPANSION COVID19-enviado-22-7-20.xlsx") %>%
  readxl::read_excel("data-raw/expansion/FACTOR EXPANSION COVID19-enviado-25-7-20.xlsx") %>% 
  janitor::clean_names() %>% 
  select(ubigeo,conglomerado=conglomeradofinal,mviv:factorfinal)

diccionario_ponderaciones %>% 
  filter(conglomerado=="22970")

diccionario_ponderaciones %>% 
  filter(ubigeo=="150102")

# diccionario_ponderaciones %>% 
#   count(ubigeo)
# 
# diccionario_ponderaciones %>% 
#   count(ubigeo,factorfinal)

# ___________ -------------------------------------------------------------



# ubigeo-diris-district-reunis ---------------------------------------------------

reunis_rute <- "../asis_repositorio/denom_r/data-raw/Poblacion Peru 2020 Dpto Prov Dist Final INEI-actualizado.xlsx"
diris_diccionario <- readxl::read_excel(reunis_rute,sheet = 4,skip = 6) %>% 
  janitor::clean_names() %>% 
  select(ubigeo:distrito)

ubigeo_diccionario <- 
  read_rds("../asis_repositorio/shape_r/data/per4-shp_distritos_janitor.rds") %>% 
  as_tibble() %>% 
  select(starts_with("cd_"),starts_with("nm_"),-ends_with("_t"),geometry) %>% 
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
  filter(nm_prov=="lima"|nm_prov=="callao") %>% 
  write_rds("data/per4-shp_distritos_janitor_diris.rds")

ubigeo_diccionario %>% 
  count(diris,ipress,ipress_name)

# csv subject -------------------------------------------------------------

pp_raw_data <- readxl::read_excel(file_name,sheet = 2) %>% 
  select(-3) %>% 
  mutate(apellido_paterno=str_replace_all(apellido_paterno," ,",""),
         apellido_materno=str_replace_all(apellido_materno," ,",""),
         nombres=str_replace_all(nombres," ,",""),
         apellido_paterno=str_replace_all(apellido_paterno,",",""),
         apellido_materno=str_replace_all(apellido_materno,",",""),
         nombres=str_replace_all(nombres,",",""),
         apellido_paterno=if_else(is.na(apellido_paterno),"",apellido_paterno),
         apellido_materno=if_else(is.na(apellido_materno),"",apellido_materno),
         nombres=if_else(is.na(nombres),"",nombres)
  ) %>% 
  mutate(nombre_completo=str_c(apellido_paterno," ",apellido_materno,", ",nombres),
         nombre_completo=str_to_upper(nombre_completo))

# pp_raw_data %>% #3202x175
#   union_all(
#     recovered_observations %>% #38x5
#       select(-conglomerado,-numero_vivienda)
#     ) %>% #3240x176
#   glimpse()

pp_raw_data %>% 
  # colnames()
  write_csv(str_c("data/seroprev-pp-",my_timestap,".csv"))

pp_raw_data %>% glimpse()

# pp_raw_data %>% 
#   select(dni,nombre_completo,nombres,apellido_paterno,apellido_materno,tipo_muestra_pcr) %>%
#   filter(str_detect(nombre_completo,"______")) %>%
#   avallecam::print_inf()

registros_por_hh <- pp_raw_data %>% 
  select(`_parent_index`,`_index`,numero_hogar) %>% 
  # count(`_parent_index`,`_index`,numero_hogar) %>% 
  # avallecam::print_inf()
  group_by(`_parent_index`,numero_hogar) %>% 
  summarise(n_registros_pp=n()) %>% 
  ungroup() #%>% 
# mutate(participante=as.integer(participante),
#        diferencia_E_O=n_registros-as.integer(participante))


# csv household -----------------------------------------------------------

hh_raw_data <- readxl::read_excel(file_name,sheet = 1) %>% 
  mutate_at(.vars = vars(starts_with("peru_")),.funs = as.factor) #%>% 
  # left_join(registros_por_hh,
  #           by=c("_index"="_parent_index"))

hh_raw_data %>% 
  write_csv(str_c("data/seroprev-hh-",my_timestap,".csv"))

hh_raw_data %>% glimpse()

# xlsx recovered ----------------------------------------------------------

#need to add
recovered_observations <- 
  readxl::read_excel("table/05-ausentes-en_consolidado-con_conglomerado_recuperado-n45.xlsx") %>% 
  select(diris_2,codigo_del_tubo,conglomerado,
         numero_vivienda,accion_pendiente,vivienda_estatus,
         consentimiento,numero_dni,
         nombres_y_apellidos_del_participante,everything()) %>% 
  # glimpse()
  mutate(conglomerado=str_replace(conglomerado,"'","")) %>% 
  filter(accion_pendiente!="digitado") %>% 
  filter(accion_pendiente!="ya_en_base") %>% 
  select(conglomerado,numero_vivienda,numero_dni,
         #nombres_y_apellidos_del_participante,
         everything()) %>% 
  # count(accion_pendiente)
  select(-codigo_del_tubo,-diris_2,-accion_pendiente,-consentimiento,
         -doc_identidad,-dni_2,-vivienda_estatus,
         -nombres_y_apellidos_del_participante,
         -(estatus_resultado:codigo_orden),
         -(estado:diris)) %>% 
  mutate(fecha_nacimiento=janitor::excel_numeric_to_date(as.numeric(fecha_nacimiento)),
         fecha_nacimiento=as.character(fecha_nacimiento)) %>% 
  rename(dni=numero_dni) %>% 
  mutate(
    presente_prueba="si", # EXCLUSION!!!! 
    ig_clasificacion="negativo" # EXCLUSION!!!!
  ) %>% 
  left_join(diccionario_conglomerado %>% select(peru_dist=ubigeo,conglomerado))
# recovered_observations
# naniar::miss_var_summary() %>%
# avallecam::print_inf()
# glimpse()

# missings ----------------------------------------------------------------

hh_raw_data %>% 
  naniar::miss_var_summary() %>% 
  avallecam::print_inf()

# hh_raw_data %>% 
#   filter(is.na(n_registros_pp))

# _______________ ---------------------------------------------------------


# INFO VIVIENDA seccion 2 -------------------------------------------------


# __ evaluacion de criterio para retirar replcia a nivel de jefe d --------

issue <- "624"

# hh_raw_data %>%
#   filter(`_index`==issue) %>%
#   glimpse()
# pp_raw_data %>%
#   filter(`_parent_index`==issue) %>%
#   filter(jefe_si_no=="si") %>%
#   glimpse()

issue_decide <- "2425"

# __ creación ----------------------------------------------------------------


vv_raw_data <- pp_raw_data %>% #HECHO: AGREGAR NÚMERO DE HOGAR y verificar diferencia!
  # glimpse()
  # OJO: retiramos duplicado en jefe de vivienda
  # conservamos según criterio de edad y nivel educativo
  filter(!(`_parent_index`==issue & `_index`==issue_decide)) %>% 
  select(`_parent_index`,numero_hogar,
          tipo_vivienda,
         agua,
         desague,
         electricidad,
         nro_dormitorios,
         nro_convivientes) %>% 
  left_join(registros_por_hh %>% 
              rename(n_registros_vv=n_registros_pp)) %>% 
  # select()
  # naniar::miss_var_summary() %>% 
  # avallecam::print_inf()
  group_by(`_parent_index`) %>% 
  filter(!is.na(nro_convivientes)) %>% #menos cantidad de valores perdidos
  ungroup() %>% 
  distinct() %>% 
  #corregir valores perdidos con observado
  mutate(
    nro_dormitorios = as.numeric(nro_dormitorios),
    nro_dormitorios = if_else(condition = nro_dormitorios==999,
                              true = NA_real_,
                              false = nro_dormitorios),
    nro_convivientes = as.numeric(nro_convivientes),
    nro_convivientes = if_else(condition = nro_convivientes==999,
                               true = as.double(n_registros_vv),
                               false = nro_convivientes)) #%>% 
  # filter(nro_convivientes==999) %>% 
  # filter(nro_dormitorios==999) %>% 

# ver duplicados - NO
# corregir en issue e issue_decide
vv_raw_data %>% 
  group_by(`_parent_index`,numero_hogar) %>% 
  filter(n()>1) %>% 
  avallecam::print_inf()
# para recuperar valores perdidos por nro docrmitorios y nro convivientes
# colocar personas entrevistadas por vivienda

vv_raw_data %>% dim()

# pp_raw_data %>% glimpse()
#   select(starts_with("material_"),starts_with("hogar"),starts_with("cocina"))
# vv_raw_data %>% 
#   avallecam::print_inf()
  # skimr::skim()

# __________ --------------------------------------------------------------


# UNION VIVIENDA - SUJETO -------------------------------------------------


# OJO ---------------------------------------------------------------------


# hh_raw_data %>% 
#   count(`_index`) %>% 
#   avallecam::print_inf()
# pp_raw_data %>% 
#   count(`_parent_index`) %>% 
#   avallecam::print_inf()

# viviendas sin información de participantes
anti_join(hh_raw_data, #813
          pp_raw_data, #3117
          by=c("_index"="_parent_index")) %>% 
  dim()
  # select(-peru_depa,-peru_prov,-(direccion:longitud),
  #        -(id:`_uuid`),-`_validation_status`) %>% 
  # arrange(conglomerado) %>% 
  # avallecam::print_inf()
anti_join(hh_raw_data, #813
          pp_raw_data, #3117
          by=c("_index"="_parent_index")) %>% 
  left_join(diccionario_conglomerado) %>% 
  # glimpse()
  writexl::write_xlsx("table/05-20200724-viviendas_sin_participante.xlsx")


# hh_raw_data %>% select(id) %>% naniar::miss_var_summary()

# sujetos con información de vivienda
inner_join(hh_raw_data, #813
          pp_raw_data, #3117
          by=c("_index"="_parent_index")) %>% 
  dim()
  # filter(conglomerado=="4724602") %>% 
  # filter(conglomerado=="17940") %>%
  # select(-peru_depa,-peru_prov,-(direccion:longitud),
  #        -(id:`_uuid`),-`_validation_status`) %>% 
  # count(conglomerado,numero_vivienda)

anti_join(hh_raw_data, #813
          vv_raw_data, #756
          by=c("_index"="_parent_index")) %>% 
  avallecam::print_inf()

# hh_raw_data %>% 
#   count(`_index`,sort = T)
# pp_raw_data %>% 
#   count(`_parent_index`) %>% 
#   avallecam::print_inf()

# union all ---------------------------------------------------------------

#' sujeto
#' vivienda
#' vivienda dado por jefe de hogar
#' diccionario de ubigeo
#' retorno ins resultado pm

uu_raw_data_prelab <- left_join(pp_raw_data, #3202 X 175
                         hh_raw_data, #816 x 26
                         by=c("_parent_index"="_index")) %>% #3202 x 200 -> HECHO: ver si personas = 0 o son duplicados
  # join all recovered fron consolidado_pm x retorno_pm
  union_all(recovered_observations) %>% #38x5 -> 3240 x 200
  # recover and fix age issue with missings
  # select(edad,fecha_nacimiento) %>%
  mutate(edad=as.numeric(edad)) %>% 
  # filter(as.numeric(edad)>200|is.na(edad)) %>% 
  mutate(edad=case_when(
    is.na(edad)~(lubridate::interval(lubridate::ymd(fecha_nacimiento),
                                     Sys.Date())/lubridate::years(1)) %>% floor(),
    edad==999~NA_real_,
    TRUE~edad)) %>% 
  # avallecam::print_inf()
  # unir info de vivienda por jefe de hogar
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
  mutate(nro_convivientes=case_when(
    nro_convivientes==0 ~ as.numeric(participante),
    is.na(nro_convivientes) ~ as.numeric(participante),
    # nro_convivientes==0 ~ as.numeric(n_registros_vv),
    TRUE~nro_convivientes)) %>%
  # fix ubigeo-inei
  mutate(peru_dist=as.character(peru_dist)) %>% 
  mutate(peru_dist=case_when(
    str_length(peru_dist)==5~str_replace(peru_dist,"(.+)","0\\1"),
    TRUE~peru_dist
  )) %>% 
  left_join(ubigeo_diccionario %>% select(-geometry)) %>% #conserva observaciones
  # naniar::miss_var_summary() %>% avallecam::print_inf()
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
  cdcper::cdc_edades_peru(edad) #%>% 
  
  # # last logical correction
  # mutate(tipo_muestra_pcr=case_when(
  #   (ig_clasificacion=="negativo" | is.na(ig_clasificacion)) & 
  #     (tipo_muestra_pcr!="nasal" | is.na(tipo_muestra_pcr)) ~ "nasal",
  #   TRUE~tipo_muestra_pcr
  # )) 

uu_raw_data_prelab %>% 
  skimr::skim(edad)

# ________ ----------------------------------------------------------------


# UNION: LAB RESULTS -------------------------------------------------------

uu_raw_data <- uu_raw_data_prelab %>% 
  
  # resultados laboratorio
  left_join(
    retorno_ins %>% 
      rename_all(.funs=str_replace,"(.+)","left_01_\\1") %>%
      rename(dni=left_01_dni,
             convResultado_01=left_01_convResultado) %>% 
      mutate(left_01=TRUE)
    ) %>% #conserva observaciones
  # count(left_01)
  left_join(
    retorno_ins %>% #select(-celular1) %>%
      rename_all(.funs=str_replace,"(.+)","left_02_\\1") %>%
      rename(nombre_completo=left_02_nombrePaciente,
             convResultado_02=left_02_convResultado) %>%
      mutate(left_02=TRUE)
  ) %>%
  # count(left_01,left_02) #2186-(1098+932+90)
  # select(dni,nombre_completo,convResultado_01,convResultado_02,left_01,left_02) %>%
  mutate(convResultado=coalesce(convResultado_01,convResultado_02),
         EstatusResultado=coalesce(left_01_EstatusResultado,left_02_EstatusResultado),
         CodigoOrden=coalesce(left_01_CodigoOrden,left_02_CodigoOrden),
         fechaNacimiento=coalesce(left_01_fechaNacimiento,left_02_fechaNacimiento),
         DocIdentidad=coalesce(left_01_DocIdentidad,left_02_DocIdentidad),
         left_lab=coalesce(left_01,left_02)) %>%
  select(-convResultado_01,-convResultado_02,
         -left_01_EstatusResultado,-left_02_EstatusResultado,
         -left_01_CodigoOrden,-left_02_CodigoOrden,
         -left_01_fechaNacimiento,-left_02_fechaNacimiento,
         -left_01_DocIdentidad,-left_02_DocIdentidad,
         -left_01,-left_02,
         -left_01_nombrePaciente,-left_02_dni)
  # count(convResultado_01,convResultado_02,convResultado,left_01,left_02,left_lab) #2186-(1097+933+89) = 67

# __ reporte --------------------------------------------------------------

hh_raw_data %>% dim()
pp_raw_data %>% dim()
vv_raw_data %>% dim()
uu_raw_data %>% dim()

uu_raw_data %>% 
  count(tipo_muestra_pcr,left_lab)

# pendientes, rechazoz e inconsistencias
uu_raw_data %>% 
  count(ig_clasificacion,tipo_muestra_pcr,EstatusResultado)

# pendiente
uu_raw_data %>% 
  filter(tipo_muestra_pcr=="nasal") %>% 
  filter(is.na(EstatusResultado)) %>%
  writexl::write_xlsx("table/04-20200723-pendiente-in_kobo-presente_prueba-pendiente_retorno.xlsx")

# rechazo
uu_raw_data %>% 
  filter(tipo_muestra_pcr=="nasal") %>% 
  filter(EstatusResultado=="Rechazo Rom") %>%
  writexl::write_xlsx("table/04-20200723-rechazos-in_kobo-presente_prueba-rechazo_rom.xlsx")

# inconsistente: con resultado pero si reporte de envio de muestra
uu_raw_data %>% 
  filter(tipo_muestra_pcr!="nasal"|is.na(tipo_muestra_pcr)) %>% 
  # count(tipo_muestra_pcr,EstatusResultado)
  filter(EstatusResultado=="Resultado Verificado") %>% 
  writexl::write_xlsx("table/04-20200723-inconsistente-in_kobo-sin_prueba-con_resultado.xlsx")

#ausentes
retorno_ins %>%
  anti_join(uu_raw_data) %>% 
  rename(nombre_completo=nombrePaciente,
         dni_2=dni) %>% 
  anti_join(uu_raw_data) %>% 
  # avallecam::print_inf()
  writexl::write_xlsx("table/04-20200723-ausentes-retorno_ins-anti_join-base_nominal.xlsx")

consolidados <- read_rds("data/cdc-consolidados_a_ins.rds") %>% 
  filter(!is.na(n_final)) %>% 
  rename(dni=n_final) %>% 
  rownames_to_column()

# en retorno pero no en consolidados
retorno_ins %>%
  anti_join(uu_raw_data) %>% 
  rename(nombre_completo=nombrePaciente,
         dni_2=dni) %>% 
  anti_join(uu_raw_data) %>% 
  rename(dni=dni_2) %>% #64
  anti_join(consolidados) %>% #22
  mutate(nombres_y_apellidos_del_paciente=
           str_replace(nombre_completo,"(.+), (.+)","\\2 \\1")) %>% 
  rename(dni_3=dni) %>% 
  anti_join(consolidados) 
#' 19, entonces
#' 45 sí presentes en consolidados
#' pendiente: corregir nombres o dni para 
#' limpiar vinculación o rescatar lo que se pueda 

uu_raw_data %>% 
  filter(conglomerado=="2783801") %>% 
  select(conglomerado,numero_vivienda,participante,nombre_completo) %>% 
  avallecam::print_inf()

diccionario_ponderaciones %>% 
  filter(conglomerado=="2783801")

uu_raw_data %>% 
  select(dni,nombre_completo,EstatusResultado,conglomerado,numero_vivienda) %>% 
  filter(str_detect(nombre_completo,"GALINDO"))

# uu_raw_data %>% #glimpse()
#   select(dni,nombre_completo,nombres,apellido_paterno,apellido_materno,
#          sexo,edad,`_submission__id`,left_lab,convResultado) %>% 
#   filter(str_detect(nombre_completo,"______")) %>%
#   avallecam::print_inf()

  
uu_raw_data %>% 
  janitor::tabyl(convResultado) %>% 
  janitor::adorn_pct_formatting()

uu_raw_data %>% 
  janitor::tabyl(EstatusResultado,convResultado)

# __________ --------------------------------------------------------------


# PRUEBAS POST ------------------------------------------------------------

# uu_raw_data %>% glimpse()  

# __ identificador por persona -----------------------------------------------

uu_raw_data %>% 
  select(dni,nombre_completo,`_index`) %>% 
  filter(is.na(dni))

pp_raw_data %>% 
  select(nombres,apellido_paterno,apellido_materno,`_index`,`_parent_index`) %>% 
  filter(`_index`==1467)

hh_raw_data %>% 
  select(`_index`,conglomerado,numero_vivienda) %>% 
  filter(`_index`==372)

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
  mutate(participante=as.integer(participante),
         diferencia_E_O=n_registros-as.integer(participante)) %>% 
  # filter(n_registros==0)
  # summarise_if(.predicate = is.integer,.funs = sum,na.rm=T)
  # # left_join(vv_raw_data)
  # filter(diferencia_E_O>0) %>%
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

# uu_clean_data %>% 
#   select(dni,nombre_completo,left_ins_EstatusResultado) %>% 
#   naniar::miss_var_summary()

# RAW: write to data -----------------------------------------------------------

uu_raw_data %>% 
  rownames_to_column() %>% 
  write_rds("data/uu_raw_data.rds")


# _________________ -------------------------------------------------------


# CLEAN: create new covariates --------------------------------------------


uu_raw_data <- read_rds("data/uu_raw_data.rds") #3117
# uu_raw_data %>% glimpse()


# REVISION: missings ------------------------------------------------------


# __ conglome | viviendas | sujetos ------------------------------------------

distrito_conglomerado_nro_viviendas <- uu_raw_data %>% 
  count(cd_dist,conglomerado,numero_vivienda) %>% 
  count(cd_dist,conglomerado)

distrito_conglomerado_nro_viviendas %>% 
  filter(conglomerado=="27714")


# ___ save counting -------------------------------------------------------

distrito_conglomerado_nro_viviendas %>% 
  write_rds()

# ___ continuacion --------------------------------------------------------



# conglomerados no muestreados
diccionario_conglomerado %>% 
  filter(!is_in(conglomerado,distrito_conglomerado_nro_viviendas$conglomerado))

# conglomerados no presentes en la ponderacion
diccionario_ponderaciones %>% 
  filter(!is_in(conglomerado,distrito_conglomerado_nro_viviendas$conglomerado))

uu_raw_data %>% 
  filter(cd_dist=="150102") %>% 
  count(nm_dist,cd_dist,conglomerado)

diccionario_ponderaciones %>% 
  filter(ubigeo=="150102")

hh_raw_data %>% 
  filter(peru_dist=="150102") %>% 
  count(conglomerado,numero_vivienda,participante,`_index`,agregar)

# pp_raw_data %>% 
#   filter(is_in(`_parent_index`,c(118,117))) %>% 
#   dim()


# __ prueba rapida -------------------------------------------------------

# ___ categorizaciones ------------------------------------------------

uu_raw_data %>% 
  count(presente_prueba,resultado_pr,resultado_pr2,tipo_muestra_pcr,
        ig_clasificacion,igg,igm,igg_igm)

uu_raw_data %>% 
  filter(presente_prueba=="si") %>% 
  filter(ig_clasificacion=="missing") %>% 
  writexl::write_xlsx("table/04-20200723-pr_pendiente-in_kobo-presente_prueba-sin_resultado_pr.xlsx")

# __________ --------------------------------------------------------------


# CREAR: base --------------------------------------------------------------


uu_clean_data <- uu_raw_data %>% #3117 
  # filter(conglomerado=="2783801") %>% filter(numero_vivienda=="99") %>%
  
  # justificación: sin ningún resultado de prueba no hay outcome
  # va al flujograma de numero de participantes
  # se van 17 observaciones
  filter(presente_prueba=="si") %>% # EXCLUSION!!!!
  # glimpse()
  
  left_join(diccionario_conglomerado,by = c("conglomerado","cd_dist"="ubigeo")) %>% 
  
  #factor to be used
  mutate_at(.vars = vars(nm_dist,cd_dist,sexo),.funs = as.factor) %>% 
  
  # justificación: debe tener conglomerado
  # filter(!is.na(totvivsel)) %>% #conserva observaciones
  
  left_join(diccionario_ponderaciones,by = c("conglomerado","cd_dist"="ubigeo")) %>% 
  
  # PENDIENTE: VERIFICAR SI ES UN MISSING NO RECUPERABLE
  # justificación: estuvo en prueba pero no se colectó ningún resultado
  # se va 1
  filter(ig_clasificacion!="missing"|!is.na(ig_clasificacion)) %>% # EXCLUSION!!!!
  
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
  # mutate(ind_pobreza=nbi_educacion+nbi_haci+nbi_ninos_sin_cole+nbi_electricidad+nbi_agua+nbi_desague) %>% 
  mutate(ind_pobreza=pmap_dbl(.l = select(.,nbi_educacion,nbi_haci,nbi_ninos_sin_cole,
                                 nbi_electricidad,nbi_agua,nbi_desague),
                          .f = sum,na.rm=TRUE)) %>% 
  mutate(pobreza=case_when(ind_pobreza==0~"No pobre",
                           ind_pobreza==1~"Pobre",
                           ind_pobreza>=1~"Pobre extremo"),
         pobreza_dico=case_when(ind_pobreza==0~"No pobre",
                                ind_pobreza>=1~"Pobre"))

# uu_clean_data %>% count(totvivsel)

# _ WRITE clean data --------------------------------------------------------


uu_clean_data %>% 
  write_rds("data/uu_clean_data.rds")

# outcomes! ---------------------------------------------------------------

uu_clean_data %>% 
  count(sintomas_cualquier_momento,sintomas_si_no,sintomas_previos)

uu_clean_data %>% 
  count(presente_prueba,resultado_pr,resultado_pr2,ig_clasificacion,convResultado,positividad_peru)

# CALCULAR: nro viv ------------------------------------------------------


# __ conglome | viviendas | sujetos ------------------------------------------

# número total de viviendas 
uu_clean_data %>% 
  count(cd_dist,conglomerado,numero_vivienda)

distrito_conglomerado_nro_viviendas <- uu_clean_data %>% 
  count(cd_dist,conglomerado,numero_vivienda) %>% 
  # filter(conglomerado=="2783801")
  count(cd_dist,conglomerado)

distrito_conglomerado_nro_viviendas %>% 
  writexl::write_xlsx("table/06-ubigeo-conglomerado_numero_de_viviendas.xlsx")


distrito_conglomerado_nro_viviendas %>% 
  filter(conglomerado=="27378")

# uu_raw_data %>%
uu_clean_data %>%
  filter(conglomerado=="4724602") %>% 
  select(conglomerado,numero_vivienda,participante,nombre_completo) %>% 
  avallecam::print_inf()

# explorar ----------------------------------------------------------------

uu_clean_data %>% 
  naniar::miss_var_summary() %>% 
  avallecam::print_inf()


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


