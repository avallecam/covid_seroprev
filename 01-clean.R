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

retorno_ins %>% naniar::miss_var_summary()
# retorno_ins %>% 
#   filter(is.na(nombrePaciente))

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

file_name_ponderaciones <- avallecam::read_lastfile(path = "data-raw/expansion/",pattern = ".xlsx")

diccionario_ponderaciones <- 
  readxl::read_excel(file_name_ponderaciones) %>% 
  janitor::clean_names() %>% 
  select(ubigeo,conglomerado=conglomeradofinal,mviv:factorfinal)

# diccionario_ponderaciones %>% 
#   filter(conglomerado=="_______")

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

# csv household -----------------------------------------------------------

hh_raw_data <- readxl::read_excel(file_name,sheet = 1) %>% 
  mutate_at(.vars = vars(starts_with("peru_")),.funs = as.factor) #%>% 
# left_join(registros_por_hh,
#           by=c("_index"="_parent_index"))

hh_raw_data %>% 
  write_csv(str_c("data/seroprev-hh-",my_timestap,".csv"))

hh_raw_data %>% glimpse()

hh_raw_data %>%
  naniar::miss_var_summary() %>%
  avallecam::print_inf()

# hh_raw_data %>% 
#   filter(is.na(n_registros_pp))

# xlsx recovered ----------------------------------------------------------

#need to add
recovered_observations <- 
  readxl::read_excel("table/previo/05-ausentes-en_consolidado-con_conglomerado_recuperado-n45.xlsx") %>% 
  select(diris_2,codigo_del_tubo,conglomerado,
         numero_vivienda,numero_hogar,accion_pendiente,vivienda_estatus,
         consentimiento,numero_dni,
         nombres_y_apellidos_del_participante,everything()) %>% 
  # glimpse()
  mutate(conglomerado=str_replace(conglomerado,"'","")) %>% 
  filter(accion_pendiente!="digitado") %>% 
  filter(accion_pendiente!="ya_en_base") %>% 
  select(conglomerado,numero_vivienda,numero_dni,
         #nombres_y_apellidos_del_participante,
         everything()) %>% 
  # filter(vivienda_estatus=="vacia")
  # count(accion_pendiente)
  select(-codigo_del_tubo,-diris_2,-accion_pendiente,-consentimiento,
         -doc_identidad,-dni_2,-vivienda_estatus,
         -nombres_y_apellidos_del_participante,
         -(estatus_resultado:codigo_orden),
         -(estado:diris)) %>% 
  mutate(fecha_nacimiento=janitor::excel_numeric_to_date(as.numeric(fecha_nacimiento)),
         fecha_nacimiento=as.character(fecha_nacimiento)) %>% 
  rename(dni=numero_dni) %>% 
  #motivo: determinacion en base a protocolo
  mutate(
    presente_prueba="si", # EXCLUSION!!!! 
    resultado_pr="negativo" # EXCLUSION!!!!
  ) %>% 
  left_join(diccionario_conglomerado %>% select(peru_dist=ubigeo,conglomerado)) %>% 
  # asociar con un index para posibilitar union con base de sujetos
  left_join(
    hh_raw_data %>% 
      select(conglomerado,numero_vivienda,`_index`)
  ) %>% 
  # crear nuevos index para viviendas no rgistradas en kobo
  mutate(numero_hogar=as.character(numero_hogar),
         `_index`=if_else(is.na(`_index`),as.double(conglomerado),`_index`)) #index eq conglomerado

recovered_observations %>% glimpse()

recovered_observations %>% 
  naniar::miss_var_summary() %>%
  avallecam::print_inf()

# recovered_observations %>%
#   avallecam::print_inf()

# csv subject -------------------------------------------------------------

pp_raw_data_pre <- readxl::read_excel(file_name,sheet = 2) %>% 
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
         nombre_completo=str_to_upper(nombre_completo)) %>% 
  mutate(numero_hogar=str_replace(numero_hogar,",","")) %>% #3225 x 175
  #filter out replicates
  filter(!(`_parent_index`==256 & numero_hogar=="5 reemplazo 58")) %>% #3224 
  distinct(nombre_completo,.keep_all = T)

# only recover if not registered in kobo 
pp_recovered_observations <- recovered_observations %>% 
  anti_join(
    pp_raw_data_pre %>% select(nombre_completo)
  ) %>%
  left_join(
    retorno_ins %>% 
      select(dni,sexo_ins)
  ) %>%
  # recover sex
  mutate(sexo=sexo_ins)

# pp_recovered_observations %>% count(sexo)

pp_raw_data <- pp_raw_data_pre %>% #3222 x 175
  union_all(
    pp_recovered_observations %>% 
      rename(`_parent_index`=`_index`) %>%
      select(-conglomerado,-numero_vivienda,-peru_dist) #17x6
    ) %>% #3239 x 175
  rownames_to_column() %>%
  # en pp_ index son numeros consecutivos por numero de fila
  mutate(`_index`=if_else(is.na(`_index`),as.double(rowname),`_index`)) %>%
  select(-rowname)

# pp_raw_data_pre %>% count(sexo)
# pp_raw_data %>% count(sexo)

pp_raw_data %>% 
  # colnames()
  write_csv(str_c("data/seroprev-pp-",my_timestap,".csv"))

pp_raw_data %>% glimpse()

# pp_raw_data %>% 
#   select(dni,nombre_completo,nombres,apellido_paterno,apellido_materno,tipo_muestra_pcr) %>%
#   filter(str_detect(nombre_completo,"______")) %>%
#   avallecam::print_inf()

# __________ --------------------------------------------------------------


# summary: ind at hh ------------------------------------------------------


# crear base con números de registros ingresados (miembros) por vivienda y hogar
registros_por_hh <- pp_raw_data %>% 
  select(`_parent_index`,`_index`,numero_hogar) %>% 
  # count(`_parent_index`,`_index`,numero_hogar) %>% 
  # avallecam::print_inf()
  group_by(`_parent_index`,numero_hogar) %>% 
  summarise(n_registros_pp=n()) %>% 
  ungroup() #%>% 
# mutate(participante=as.integer(participante),
#        diferencia_E_O=n_registros-as.integer(participante))

# dentro del grupo de recuperados
# que viviendas no estan consideradas en la lista de viviendas en kobo
hh_recovered_observations <- pp_recovered_observations %>% 
  count(conglomerado,numero_vivienda,numero_hogar,`_index`) %>% 
  anti_join(
    hh_raw_data %>% 
      select(conglomerado,numero_vivienda,`_index`)
  ) %>% 
  rename(n_registros_vv=n)

# todas incluidas
registros_por_hh %>% 
  filter(is_in(`_parent_index`,hh_recovered_observations$`_index`))

# # una presente en ponderaciones ya no esta incluida
# registros_por_hh %>%
#   filter(`_parent_index`=="24474")
#   # filter(`_parent_index`=="20787")
diccionario_ponderaciones %>%
  # filter(conglomerado=="24474")
  # filter(conglomerado=="20787")
  # filter(conglomerado=="2655502")
  filter(conglomerado=="2783801")
# 
# hh_raw_data %>% 
#   filter(conglomerado=="24474") %>% 
#   select(1:5,`_index`)
# 
# pp_raw_data %>% 
#   filter(`_parent_index`==532|`_parent_index`==630) %>% 
#   select(jefe_si_no)
# 


# duplicates? --------------------------------------------------------------------

hh_raw_data %>% dim()
pp_raw_data %>% dim()
hh_raw_data %>% 
  group_by_all() %>% 
  filter(n()>1) %>% 
  ungroup() %>% 
  dim()
pp_raw_data %>% 
  group_by_all() %>% 
  filter(n()>1) %>% 
  ungroup() %>% 
  dim()

# _______________ ---------------------------------------------------------


# INFO VIVIENDA seccion 2 -------------------------------------------------


# __ evaluacion de criterio para unica retirar replica a nivel de jefe d --------

issue <- 618

# hh_raw_data %>%
#   filter(`_index`==issue) %>%
#   glimpse()
# pp_raw_data %>%
#   filter(`_parent_index`==issue) %>%
#   filter(jefe_si_no=="si") %>%
#   glimpse()

issue_decide <- 2410

# __ creación ----------------------------------------------------------------


vv_raw_data <- pp_raw_data %>% #3239
  # glimpse()
  # OJO: retiramos duplicado en jefe de vivienda
  # conservamos según criterio de edad y nivel educativo
  filter(!(`_parent_index`==issue & `_index`==issue_decide)) %>% #32309
  select(`_parent_index`,numero_hogar,
          tipo_vivienda,
         agua,
         desague,
         electricidad,
         nro_dormitorios,
         nro_convivientes) %>% 
  left_join(
    registros_por_hh %>% #1014
      rename(n_registros_vv=n_registros_pp)
  ) %>% 
  # ## usamos nro_convivientes como variable con menor 
  # naniar::miss_var_summary() %>%
  # avallecam::print_inf()
  
  # conservar solo la información de jefes de hogar
  # retirar replicas con valores perdidos
  # group_by(`_parent_index`) %>% #3201 -> 745 grupos
  group_by(`_parent_index`,numero_hogar) %>% #3239 -> 1014 grupos
  filter(!is.na(nro_convivientes)) %>% # 763
  ungroup() %>% 
  # group_by_all() %>% 
  # filter(n()>1)
  distinct() %>% # 759 (replicas de misma info de vivienda)
  
  mutate(
    nro_dormitorios = as.numeric(nro_dormitorios),
    nro_convivientes = as.numeric(nro_convivientes)
  ) %>% 
  # naniar::miss_var_summary()
  # select(n_registros_vv,nro_convivientes,nro_dormitorios) %>% 
  # skimr::skim()
  
  #corregir valores perdidos con observado
  mutate(
    nro_dormitorios = if_else(condition = nro_dormitorios==999,
                              true = NA_real_,
                              false = nro_dormitorios),
    nro_convivientes = if_else(condition = nro_convivientes==999,
                               true = as.double(n_registros_vv),
                               false = nro_convivientes)) %>% 
  # filter(nro_convivientes==999) %>% 
  # filter(nro_dormitorios==999) %>% 
  # naniar::miss_var_summary()
  # select(n_registros_vv,nro_convivientes,nro_dormitorios) %>% # STATUS: OK
  # skimr::skim()
  
  left_join(
    hh_raw_data %>% #817 - conglomerado e index son equivalentes
      select(conglomerado,`_index`,numero_vivienda), 
            by=c("_parent_index"="_index")
    ) %>% # 759
  # filter(conglomerado=="20787")
  # filter(conglomerado=="2655502")
  # filter(conglomerado=="2783801") #STATUS OK
  union_all(hh_recovered_observations %>% # 2
              rename(`_parent_index`=`_index`)) # 761

# ver duplicados - NO
# corregir en issue e issue_decide
vv_raw_data %>% 
  group_by(`_parent_index`,numero_hogar) %>% 
  filter(n()>1) %>% 
  avallecam::print_inf()
# para recuperar valores perdidos por nro docrmitorios y nro convivientes
# colocar personas entrevistadas por vivienda

vv_raw_data %>% dim() #760 
vv_raw_data %>% count(`_parent_index`,numero_vivienda,sort = T) 
#' 750 viviendas con información brindada por jefe de hogar completas
#' 8 con dos hogares por vivienda
#' 1 con tres hogares por vivienda
vv_raw_data %>% naniar::miss_var_summary()
vv_raw_data %>% 
  filter(is.na(nro_convivientes)) # missing in last addition

# vv_raw_data %>% 
#   union_all(hh_recovered_observations)
#   # filter(`_parent_index`=="762")

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
  writexl::write_xlsx("table/previo/05-20200724-viviendas_sin_participante.xlsx")


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

# right_join(hh_raw_data, #813
#           vv_raw_data, #756
#           by=c("_index"="_parent_index")) %>% 
#   avallecam::print_inf()

# anti_join(hh_raw_data, #813
#           vv_raw_data, #756
#           by=c("_index"="_parent_index")) %>% 
#   avallecam::print_inf()

# hh_raw_data %>% 
#   count(`_index`,sort = T)
# pp_raw_data %>% 
#   count(`_parent_index`) %>% 
#   avallecam::print_inf()


# filter(conglomerado=="20787")
# filter(conglomerado=="2655502")
# filter(conglomerado=="2783801")
left_join(pp_raw_data, #3202 X 175
          hh_raw_data, #816 x 26
          by=c("_parent_index"="_index")) %>% 
  # filter(str_detect(nombre_completo,"ORTEGA")) %>%
  # filter(conglomerado=="2655502") %>%
  filter(conglomerado=="2783801") %>%
  count(numero_vivienda)
  # select(nombre_completo,conglomerado,numero_vivienda,numero_hogar)

diccionario_ponderaciones %>% filter(conglomerado=="2655502")
diccionario_ponderaciones %>% filter(conglomerado=="2783801") # STATUS OK
pp_recovered_observations

hh_recovered_observations
hh_raw_data %>%
  filter(conglomerado=="2783801") %>% 
  select(conglomerado,numero_vivienda,peru_dist,`_index`)
vv_raw_data %>%
  filter(conglomerado=="2783801")
  
# union all ---------------------------------------------------------------

#' sujeto
#' vivienda
#' vivienda dado por jefe de hogar
#' diccionario de ubigeo
#' retorno ins resultado pm

uu_raw_data_prelab <- left_join(pp_raw_data, #3239 X 175
                                hh_raw_data, #817 x 26
                                by=c("_parent_index"="_index")) %>% #3239 x 200 -> HECHO: ver si personas = 0 o son duplicados
  
  # recover and fix age issue with missings
  # select(edad,fecha_nacimiento) %>%
  mutate(edad=as.numeric(edad)) %>% 
  # filter(as.numeric(edad)>200|is.na(edad)) %>% 
  mutate(edad=case_when(
    is.na(edad)~(lubridate::interval(lubridate::ymd(fecha_nacimiento),
                                     Sys.Date())/lubridate::years(1)) %>% floor(),
    edad==999~NA_real_,
    TRUE~edad)) %>% 
  # skimr::skim(edad)
  # avallecam::print_inf()
  
  # unir info de vivienda por jefe de hogar
  select(#`_parent_index`,
    -tipo_vivienda,
    -agua,
    -desague,
    -electricidad,
    -nro_dormitorios,
    -nro_convivientes) %>% #3240 x 194
  # count(`_parent_index`,numero_hogar) %>% count(`_parent_index`,sort = T) #812 parent_index
  left_join(vv_raw_data %>% 
              select(-conglomerado,-numero_vivienda)) %>% #760 x 9 -> 3239 x 201 #conserva observaciones
  # #verificar
  # select(conglomerado,nro_dormitorios,nro_convivientes,participante,n_registros_vv) %>%
  # naniar::miss_var_summary()
  # filter(is.na(nro_convivientes)) # 567 - 17.5% missings
  
  # recuperar conglomerado
  mutate(conglomerado=case_when(
    is.na(conglomerado) ~ as.character(`_parent_index`),
    TRUE~conglomerado)) %>% 
  mutate(numero_vivienda=if_else(is.na(numero_vivienda),"99",numero_vivienda)) %>%
  # select(conglomerado,nro_dormitorios,nro_convivientes,participante,n_registros_vv) %>%
  # naniar::miss_var_summary()
  
  # recuperar proto ubigeo
  arrange(conglomerado) %>% 
  # select(conglomerado,numero_vivienda,peru_dist) %>% 
  fill(peru_dist) %>% 
  
  #recuperacion de datos de vivienda
  mutate(nro_convivientes=case_when(
    is.na(participante) & is.na(nro_convivientes) ~ as.numeric(n_registros_vv),
    !is.na(participante) & is.na(nro_convivientes) ~ as.numeric(participante),
    TRUE~nro_convivientes)
  ) %>%
  # #verificar
  # select(conglomerado,`_parent_index`,numero_vivienda,numero_hogar,
  #        nro_dormitorios,nro_convivientes,participante,n_registros_vv,nombre_completo) %>%
  # naniar::miss_var_summary()
  # filter(is.na(nro_convivientes)) # 0 - no missings
  
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
  cdcper::cdc_edades_peru(edad) %>% 
  # collapse the last two categories 
  # due to low number of observations
  # original distribution:
  # [80,90)          96
  # [90,100)         21
  mutate(edad_decenios=fct_collapse(edad_decenios,
                                    "[80,100)"=c("[80,90)","[90,100)")))


uu_raw_data_prelab %>% 
  skimr::skim(edad)

uu_raw_data_prelab %>%
  filter(is.na(conglomerado)) %>% dim()

uu_raw_data_prelab %>% 
  # skimr::skim(edad)
  # select(starts_with("edad"))
  # count(edad_decenios)
  select(edad,edad_decenios) %>% 
  # filter(is.na(edad_decenios))
  count(edad_decenios)

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
  count(ig_clasificacion,tipo_muestra_pcr,EstatusResultado,
        convResultado) %>% 
  avallecam::print_inf()

# pendiente
uu_raw_data %>% 
  # filter(tipo_muestra_pcr=="nasal") %>% 
  filter(ig_clasificacion=="negativo") %>% 
  # filter(tipo_muestra_pcr=="nasal" | is.na(tipo_muestra_pcr)) %>% 
  filter(tipo_muestra_pcr!="no_posible" | is.na(tipo_muestra_pcr)) %>%
  filter(is.na(EstatusResultado) | is.na(convResultado)) %>%
  filter(is.na(convResultado)) %>%
  # count(ig_clasificacion,tipo_muestra_pcr,EstatusResultado,convResultado) #15+15+3+8
  select(dni,nombre_completo,diris,conglomerado,numero_vivienda,
         ig_clasificacion,tipo_muestra_pcr,EstatusResultado,
         convResultado) %>% 
  writexl::write_xlsx("table/previo/04-20200723-pendiente-in_kobo-presente_prueba-pendiente_retorno.xlsx")

# rechazo
uu_raw_data %>% 
  filter(tipo_muestra_pcr=="nasal") %>% 
  filter(EstatusResultado=="Rechazo Rom") %>% #2
  writexl::write_xlsx("table/previo/04-20200723-rechazos-in_kobo-presente_prueba-rechazo_rom.xlsx")

# inconsistente: con resultado pero si reporte de envio de muestra
uu_raw_data %>% 
  filter(tipo_muestra_pcr!="nasal"|is.na(tipo_muestra_pcr)) %>% 
  filter(ig_clasificacion!="negativo") %>% 
  filter(EstatusResultado=="Resultado Verificado") %>% #36+9
  # count(ig_clasificacion,tipo_muestra_pcr,EstatusResultado)
  writexl::write_xlsx("table/previo/04-20200723-inconsistente-in_kobo-sin_prueba-con_resultado.xlsx")

#ausentes
retorno_ins %>%
  rename_all(.funs=str_replace,"(.+)","anti_01_\\1") %>%
  rename(dni=anti_01_dni) %>% 
  anti_join(uu_raw_data) %>% 
  rename(nombre_completo=anti_01_nombrePaciente,
         dni_2=dni) %>% 
  anti_join(uu_raw_data) %>% 
  # slice(2) %>% select(1:3)
  # avallecam::print_inf()
  writexl::write_xlsx("table/previo/04-20200723-ausentes-retorno_ins-anti_join-base_nominal.xlsx")

consolidados <- read_rds("data/cdc-consolidados_a_ins.rds") %>% 
  filter(!is.na(n_final)) %>% 
  rename(dni=n_final) %>% 
  rownames_to_column()

# en retorno pero no en consolidados
retorno_ins %>%
  rename_all(.funs=str_replace,"(.+)","anti_01_\\1") %>%
  rename(dni=anti_01_dni) %>% 
  anti_join(uu_raw_data) %>% 
  rename(nombre_completo=anti_01_nombrePaciente,
         dni_2=dni) %>% 
  anti_join(uu_raw_data) %>% 
  rename(dni=dni_2) %>% #64 -> 22
  anti_join(consolidados) %>% #22 -> 16
  mutate(nombres_y_apellidos_del_paciente=
           str_replace(nombre_completo,"(.+), (.+)","\\2 \\1")) %>% 
  rename(dni_3=dni) %>% 
  anti_join(consolidados) #15 
#' 19, entonces
#' 45 sí presentes en consolidados
#' pendiente: corregir nombres o dni para 
#' limpiar vinculación o rescatar lo que se pueda 

# retorno_ins %>% 
#   filter(str_detect(nombrePaciente,"________")) %>% 
#   select(dni,nombrePaciente)
# uu_raw_data %>% 
#   filter(str_detect(nombre_completo,"________")) %>% 
#   select(dni,nombre_completo)


# uu_raw_data %>% 
#   filter(conglomerado=="_______") %>% 
#   select(conglomerado,numero_vivienda,participante,nombre_completo) %>% 
#   avallecam::print_inf()
# 
# diccionario_ponderaciones %>% 
#   filter(conglomerado=="_______")

# uu_raw_data %>% 
#   select(dni,nombre_completo,EstatusResultado,conglomerado,numero_vivienda) %>% 
#   filter(str_detect(nombre_completo,"________"))

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

# pp_raw_data %>% 
#   select(nombres,apellido_paterno,apellido_materno,`_index`,`_parent_index`) %>% 
#   filter(`_index`==____)

# hh_raw_data %>% 
#   select(`_index`,conglomerado,numero_vivienda) %>% 
#   filter(`_index`==___)

# [*] conteo ESP vs OBS --------------------------------------------

# __ nivel vivienda -------------------------------------------------------


uu_raw_data %>% 
  # glimpse()
  # filter(is_in(conglomerado,si_vinculado$conglomerado)) %>% 
  select(nm_dist,cd_dist,conglomerado,numero_vivienda,
         #numero_hogar,
         participante,nro_convivientes,n_registros_vv) %>% 
  # naniar::miss_var_summary()
  # group_by(nm_dist,cd_dist,conglomerado,numero_vivienda,participante) %>% 
  group_by(nm_dist,cd_dist,conglomerado,numero_vivienda,
           participante,nro_convivientes) %>% 
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


uu_raw_data <- read_rds("data/uu_raw_data.rds") #3239
# uu_raw_data %>% glimpse()


# REVISION: missings ------------------------------------------------------


# __ conglome | viviendas | sujetos ------------------------------------------

distrito_conglomerado_nro_viviendas <- uu_raw_data %>% 
  count(cd_dist,conglomerado,numero_vivienda) %>% 
  count(cd_dist,conglomerado)

distrito_conglomerado_nro_viviendas %>% 
  filter(conglomerado=="27714")


# ___ save counting -------------------------------------------------------

# distrito_conglomerado_nro_viviendas %>% 
#   write_rds()

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
  writexl::write_xlsx("table/previo/04-20200723-pr_pendiente-in_kobo-presente_prueba-sin_resultado_pr.xlsx")

# __________ --------------------------------------------------------------


# CREAR: base --------------------------------------------------------------


uu_clean_data_pre <- uu_raw_data %>% #3239 
  
  # filter(is.na(conglomerado)) %>%
  # select(conglomerado,numero_vivienda,`_parent_index`,cd_dist)
  
  # filter(is.na(conglomerado))
  
  # filter(conglomerado=="2783801") %>% filter(numero_vivienda=="99") %>%
  
  #' [EXCLUSION]
  # justificación: sin ningún resultado de prueba no hay outcome
  # va al flujograma de numero de participantes
  # se van 17 observaciones
  filter(presente_prueba=="si") %>% # 3224 # EXCLUSION!!!!
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
  # filter(ig_clasificacion!="missing"|!is.na(ig_clasificacion)) %>% # EXCLUSION!!!!
  
  mutate(ig_clasificacion=as.factor(ig_clasificacion),
         diris=as.factor(diris),
         convResultado=as.factor(convResultado),
         sintomas_si_no=as.factor(sintomas_si_no)) %>% 
  
  #' [DISEÑO]
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
  
  # creacion de variables y recategorizacion
  
  #' [HACINAMIENTO]
  
  # hacinamiento: redatam
  mutate(nro_dormitorios = as.numeric(nro_dormitorios),
         nro_convivientes = as.numeric(nro_convivientes)) %>% 
  mutate(ind_hacin = (nro_convivientes/nro_dormitorios)) %>% 
  mutate(ind_hacin_qrt=cut(ind_hacin,breaks = 4,include.lowest = T)) %>% 
  # mutate(ind_hacin=if_else(condition = ind_hacin==Inf,
  #                          true = NA_real_,
  #                          false = ind_hacin)) %>%
  mutate(hacinamiento=case_when(ind_hacin>=0.1 & ind_hacin<=2.4~"Sin Hacinmaniento",
                                ind_hacin>=3 & ind_hacin<=20 ~"Con Hacinamiento")) %>% 
  mutate(hacinamiento=fct_relevel(hacinamiento,"Sin Hacinmaniento")) %>% 
  
  #' [CATEGORIZAR]
  
  cdcper::cdc_cut_integer(variable = nro_convivientes) %>% 
  cdcper::cdc_cut_integer(variable = nro_dormitorios,number_cuts = 3) %>% 
  cdcper::cdc_cut_integer(variable = ind_hacin) %>% 
  
  #' [POBREZA MEF]
  
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
                                ind_pobreza>=1~"Pobre")) %>% 
  
  # sympt + contact dates
  # no genera nuevos missings
  mutate_at(.vars = vars(fecha_last_contacto,fecha_inicio_sintomas,fecha_inicio_sintomas_previo),
            .funs = lubridate::ymd) %>% 
  
  # sintomas categoria
  rename_at(.vars = vars(contains("sinto")),
            .funs = janitor::make_clean_names) %>% 
  
  #'
  #'  [sintomas actuales]
  #'  
  
  #' suma horizontal total de sintomas
  mutate_at(.vars = vars(sintomas_fiebre:sintomas_anosmia),
            .funs = as.numeric) %>% 
  mutate(sintomas_actual_total = pmap(select(.,
                                             sintomas_fiebre:sintomas_anosmia),
                                      sum,na.rm=T)) %>%
  unnest(cols = c(sintomas_actual_total)) %>%
  
  #' aplicar definicion de oligosintomatico
  mutate(sintomas_actual_oligo=if_else(
    condition = sintomas_actual_total==1 & sintomas_anosmia==0,
    true = "si",
    false = "no"
  )) %>% 
  # aplicar definicion de sintomatico covid
  mutate(sintomas_actual_covid=case_when(
    sintomas_actual_total>=2 | sintomas_anosmia==1 ~ "si",
    TRUE~"no"
  )) %>% 
  # select(sintomas_actual_covid,sintomas_actual_oligo,sintomas_si_no) %>% 
  # filter(is.na(sintomas_si_no))
  #' aplicar consistencia de registros
  #' 
  #' valor perdido en reporte de sintomas actuales, 
  #' también debe de trasladarse a las variables sinto y oligo 
  #' 
  mutate(
    sintomas_actual_covid=if_else(is.na(sintomas_si_no),NA_character_,sintomas_actual_covid),
    sintomas_actual_oligo=if_else(is.na(sintomas_si_no),NA_character_,sintomas_actual_oligo)
  ) %>% 
  
  #' aplicar mismo protocolo para el reporte del otro grupo de sintomas
  #' 
  #'
  #'  [sintomas previos]
  #'  
  #' 
  #' 
  # sintomas previos | anterior
  mutate_at(.vars = vars(sintomas_anterior_fiebre:sintomas_anterior_anosmia),
            .funs = as.numeric) %>% 
  mutate(sintomas_anterior_total = pmap(select(.,
                                               sintomas_anterior_fiebre:sintomas_anterior_anosmia),
                                        sum,na.rm=T)) %>%
  unnest(cols = c(sintomas_anterior_total)) %>%
  mutate(sintomas_anterior_oligo=if_else(
    condition = sintomas_anterior_total==1 & sintomas_anterior_anosmia==0,
    true = "si",
    false = "no"
  )) %>% 
  mutate(sintomas_anterior_covid=case_when(
    sintomas_anterior_total>=2 | sintomas_anterior_anosmia==1 ~ "si",
    TRUE~"no"
  )) %>% 
  mutate(
    sintomas_anterior_covid=if_else(is.na(sintomas_previos),NA_character_,sintomas_anterior_covid),
    sintomas_anterior_oligo=if_else(is.na(sintomas_previos),NA_character_,sintomas_anterior_oligo)
  ) %>% 
  
  # filter(sintomas_anterior_total==0,sintomas_previos=="si") %>% #dim() # 1 obs
  # select(observacion_sintomas) # missing
  #' 
  #' corregir valor incoherente
  #' 
  #' 01 observación no reportó síntomas pero sí fecha. 
  #' acción: fecha se cambió a valor perdido.
  #' 
  mutate(sintomas_previos=if_else(sintomas_anterior_total==0 & sintomas_previos=="si",
                                  "no",
                                  sintomas_previos),
         
         fecha_inicio_sintomas_previo=case_when(
           sintomas_anterior_total==0 & sintomas_previos=="si"~NA_character_,
           # 01 fecha inconsistente al pasado 
           # - incorrecto: justo esta variable registra info del pasado
           # - maximo restringir antes de introducción de sars-cov-2
           fecha_inicio_sintomas_previo<lubridate::ymd(20200301)~NA_character_,
           TRUE~as.character(fecha_inicio_sintomas_previo))) %>%
  
  mutate(fecha_inicio_sintomas_previo=lubridate::ymd(fecha_inicio_sintomas_previo)) %>% 
  # select(fecha_inicio_sintomas_previo) %>%
  # skimr::skim()
  # naniar::miss_var_summary()
  
  #'
  #'  [sintomas cualquier momento]
  #'  
  
  # reintegrar valor corregido
  mutate(sintomas_cualquier_momento=case_when(
    sintomas_si_no=="si" | sintomas_previos=="si" ~ "si",
    sintomas_si_no=="no" | sintomas_previos=="no" ~ "no",
    TRUE ~ "missing"
  )) %>% 
  
  #'
  #'  [fecha prueba]
  #'  
  #'  corrección del registro de esta fecha
  #'  se usó como punto de referencia para la 
  #'  categorizacion de inicio de sintomas a 
  #'  14 días de la visita del estudio
  #'  
  mutate(fecha_prueba=lubridate::ymd(fecha_prueba)) %>%
  # skimr::skim(fecha_prueba) #17 missing del 01/07/1970 al 13/07/2020
  #'  
  #'  [CORRECCIÓN]
  #'  
  #'  MISSING en fecha de prueba
  #'  - si diris norte -> today
  #'  - si diris sur -> today
  #'  - si diris callao -> today
  #'  - si diris centro -> left_01_fecha_coleccion
  #'
  # CORRECCION DE MISSINGS
  # filter(is.na(fecha_prueba)) %>%
  mutate(fecha_prueba=case_when(
    is.na(fecha_prueba) & diris=="DIRIS NORTE" ~ lubridate::ymd(today),
    is.na(fecha_prueba) & diris=="DIRIS SUR" & is.na(today) ~ left_01_fecha_coleccion,
    is.na(fecha_prueba) & diris=="DIRIS SUR" ~ lubridate::ymd(today),
    is.na(fecha_prueba) & diris=="CALLAO" ~ lubridate::ymd(today),
    is.na(fecha_prueba) & diris=="DIRIS CENTRO" ~ left_01_fecha_coleccion,
    TRUE~fecha_prueba
  )) %>% 
  
  # filter(is.na(fecha_prueba)) %>%
  # count(today,`_submission__submission_time`,`_submission_time`,
  #       left_01_fecha_coleccion,fecha_prueba,diris,nm_dist) %>%
  # avallecam::print_inf()
  #
  #'  [CORRECCIÓN]
  #'  
  #'  OUTLIERS en fecha de prueba
  #'  - si diris norte -> today
  #'  - si diris centro -> left_01_fecha_coleccion 
  #'  - si diris callao -> 20200701 #POR error de digitación
  #'  - si diris sur -> left_01_fecha_coleccion
  #'  
  # CORRECCION DE OUTLIERS
  # filter(fecha_prueba<lubridate::ymd(20200628) | fecha_prueba>lubridate::ymd(20200710)) %>%
  mutate(fecha_prueba=case_when(
    (fecha_prueba<lubridate::ymd(20200628) | fecha_prueba>lubridate::ymd(20200710)) & diris=="DIRIS NORTE" & lubridate::ymd(today)>lubridate::ymd(20200710) ~ left_01_fecha_coleccion,
    (fecha_prueba<lubridate::ymd(20200628) | fecha_prueba>lubridate::ymd(20200710)) & diris=="DIRIS NORTE" ~ lubridate::ymd(today),
    (fecha_prueba<lubridate::ymd(20200628) | fecha_prueba>lubridate::ymd(20200710)) & diris=="DIRIS CENTRO" ~ left_01_fecha_coleccion,
    (fecha_prueba<lubridate::ymd(20200628) | fecha_prueba>lubridate::ymd(20200710)) & diris=="DIRIS SUR" ~ left_01_fecha_coleccion,
    (fecha_prueba<lubridate::ymd(20200628) | fecha_prueba>lubridate::ymd(20200710)) & diris=="CALLAO" ~ lubridate::ymd(20200701),
    TRUE~fecha_prueba)) %>% 
  
  # filter(fecha_prueba<lubridate::ymd(20200628) | fecha_prueba>lubridate::ymd(20200710)) %>%
  # count(today,`_submission__submission_time`,`_submission_time`,left_01_fecha_coleccion,fecha_prueba,diris,nm_dist) %>%
  # avallecam::print_inf()
  # glimpse()
  
  # select(contains("fecha")) %>% 
  # naniar::miss_var_summary()
  
  #'
  #' 
  #' [sintomas]
  #' 
  # select(contains("sintoma")) %>% glimpse()
  
  # select(sintomas_si_no,sintomas_actual_total,
  #        sintomas_previos,sintomas_anterior_total,
  #        fecha_inicio_sintomas,fecha_inicio_sintomas_previo,
  #        fecha_prueba,
  #        presente_prueba,ig_clasificacion,convResultado) %>% 
  # filter(sintomas_previos=="si") %>% # N=2
  # filter(sintomas_si_no=="si") %>% N=5
  # filter(is.na(sintomas_si_no)) %>% 
  # filter(is.na(fecha_inicio_sintomas_previo)) %>% 
  # avallecam::print_inf()
  
  #'
  #' [fecha sintomas actual]
  #'
  
  # sintomas actual
  # si fecha prueba is NA -> missgins & outliers fixed
  # si fecha sintomas in NA then fecha sintomas is NA
  
  mutate(fecha_prueba_actual = fecha_prueba) %>% 
  
  # naniar::miss_var_summary()
  #' 
  #' [TIEMPO DE VISITA A INICIO DE SINTOMAS]
  #' el tiempo de los síntomas se calculó 
  #' al restar la fecha de prueba corregida con 
  #' la fecha de inicio de sintomas corregida
  #' 
  mutate(fecha_prueba_inicio_sintomas_diff = fecha_prueba_actual - fecha_inicio_sintomas) %>%
  
  mutate(fecha_prueba_inicio_sintomas_diff_cat=case_when(
    fecha_prueba_inicio_sintomas_diff<=14~"[00-14]d",
    fecha_prueba_inicio_sintomas_diff<=60~"[15-60]d",
    fecha_prueba_inicio_sintomas_diff>60~"[61-Inf]d"
  )) %>% 
  mutate(fecha_inicio_sintomas_14d=case_when(
    fecha_prueba_inicio_sintomas_diff<=14~"si",
    fecha_prueba_inicio_sintomas_diff>14~"no"#,
    # TRUE~"no"
  )) %>% 
  mutate(fecha_inicio_sintomas_60d=case_when(
    fecha_prueba_inicio_sintomas_diff<=60~"si",
    fecha_prueba_inicio_sintomas_diff>60~"no"#,
    # TRUE~"no"
  )) %>% 
  
  #'
  #' [fecha sintomas previo]
  #'
  #'# sintomas previo
  mutate(fecha_prueba_previo = fecha_prueba) %>%
  # naniar::miss_var_summary()
  # 
  mutate(fecha_prueba_inicio_sintomas_previo_diff = fecha_prueba_previo - fecha_inicio_sintomas_previo) %>%
  
  mutate(fecha_prueba_inicio_sintomas_previo_diff_cat=case_when(
    fecha_prueba_inicio_sintomas_previo_diff<=14~"[00-14]d",
    fecha_prueba_inicio_sintomas_previo_diff<=60~"[15-60]d",
    fecha_prueba_inicio_sintomas_previo_diff>60~"[61-Inf]d"
  )) %>% 
  mutate(fecha_inicio_sintomas_previo_14d=case_when(
    fecha_prueba_inicio_sintomas_previo_diff<=14~"si",
    fecha_prueba_inicio_sintomas_previo_diff>14~"no"#,
    # TRUE~"no"
  )) %>% 
  mutate(fecha_inicio_sintomas_previo_60d=case_when(
    fecha_prueba_inicio_sintomas_previo_diff<=60~"si",
    fecha_prueba_inicio_sintomas_previo_diff>60~"no"#,
    # TRUE~"no"
  )) %>% 
  
  #sintomas cualquier momento - coalesce
  mutate(
    sintomas_actual_cat=case_when(
      sintomas_actual_oligo=="no" & sintomas_actual_covid=="no" ~ "sinto_asint",
      sintomas_actual_oligo=="si" ~ "sinto_oligo",
      sintomas_actual_covid=="si" ~ "sinto_covid"
    ),
    sintomas_anterior_cat=case_when(
      sintomas_anterior_oligo=="no" & sintomas_anterior_covid=="no" ~ "sinto_asint",
      sintomas_anterior_oligo=="si" ~ "sinto_oligo",
      sintomas_anterior_covid=="si" ~ "sinto_covid"
    ),
    sintomas_cualquier_momento_cat = coalesce(sintomas_anterior_cat, sintomas_actual_cat),
    fecha_prueba_inicio_sintomas_cualquier_momento_cat = coalesce(fecha_prueba_inicio_sintomas_previo_diff_cat, fecha_prueba_inicio_sintomas_diff_cat)
  ) %>% 
  # cualquier momento dentro de los 14 dias
  mutate(fecha_prueba_inicio_sintomas_cualquier_momento_cat_14d=case_when(
    is.na(fecha_prueba_inicio_sintomas_cualquier_momento_cat)~NA_character_,
    fecha_prueba_inicio_sintomas_cualquier_momento_cat=="[00-14]d"~"si",
    fecha_prueba_inicio_sintomas_cualquier_momento_cat!="[00-14]d"~"no"
  )) %>% 
  
  #' [RIESGO]
  
  # condiciones de riesgo
  rename_at(.vars = vars(contains("riesgo")),
            .funs = janitor::make_clean_names) %>% 
  
  mutate(condicion_riesgo_mayor_60a=case_when(
    edad_etapas_de_vida_c=="60a_mas"~"1",
    TRUE~condicion_riesgo_mayor_60a
  )) %>% 
  
  #' [HABITACIONES]
  
  # categorizar
  mutate(nro_dormitorios_cat=case_when(
    #1, 2, 3-5, >=6
    nro_dormitorios==1~"01",
    nro_dormitorios==2~"02",
    nro_dormitorios>=3 & nro_dormitorios<=5~"03-05",
    nro_dormitorios>=6~"06+",
    TRUE~NA_character_
  )) %>% 
  
  #' [etnias]
  
  mutate(etnia_cat=case_when(
    magrittr::is_in(etnia,c("otros",#"afro",
                            "nativo_selva","aimara",
                            "indigena_originario")) ~ "otros",
    TRUE~etnia
  )) %>% 
  
  #' [contacto]
  
  select(-contacto_gripal) %>% 
  
  #' [error por exceso de restricción en instrumento]
  
  mutate(prueba_previa=case_when(
    telefono=="999999999" & str_starts(codigo_err,"MAR|kev") ~ NA_character_,
    TRUE ~ prueba_previa
  )) %>% 
  
  #' 
  #' [ULTIMAS POST-CATEGORIZACIONES]
  #' 
  
  # pre-fusion: 
  # - colapsar categorias tipo 3 y 4
  # - valor perdido - se conservan
  mutate(contacto_tipo_collapse=case_when(
    # contacto_tipo=="contacto_tipo3"~"contacto_tipo34",
    contacto_tipo=="contacto_tipo4"~"contacto_tipo45",
    contacto_tipo=="contacto_tipo5"~"contacto_tipo45",
    TRUE ~ contacto_tipo
    # is.na(contacto_tipo)~"contacto_tipo5",
  )) %>% 
  # fusion: post-categorization of contacts with tipes
  mutate(contacto_covid_tipo=case_when(
    contacto_covid=="si" ~ str_c(contacto_covid,"_",contacto_tipo_collapse),
    # contacto_covid=="si" & !is.na(contacto_tipo) ~str_c(contacto_covid,"_",contacto_tipo),
    # contacto_covid=="si" & is.na(contacto_tipo) ~str_c(contacto_covid,"_","missing"),
    TRUE~contacto_covid
  )) %>% 
  
  
  # fusion: solo sintomaticos covid
  mutate(sintomas_cualquier_momento_cat_fecha_14d_v1=case_when(
    sintomas_cualquier_momento_cat=="sinto_covid" & fecha_prueba_inicio_sintomas_cualquier_momento_cat_14d=="si" ~ "sinto_covid_onset_u14d_si",
    sintomas_cualquier_momento_cat=="sinto_covid" & fecha_prueba_inicio_sintomas_cualquier_momento_cat_14d=="no" ~ "sinto_covid_onset_u14d_no",
    sintomas_cualquier_momento_cat=="sinto_covid" & is.na(fecha_prueba_inicio_sintomas_cualquier_momento_cat_14d) ~ NA_character_,
    TRUE ~ sintomas_cualquier_momento_cat
  )) %>% 
  
  # fusion: tiempo estratificado
  mutate(sintomas_cualquier_momento_cat_fecha_rangos=case_when(
    sintomas_cualquier_momento_cat=="sinto_covid" ~ str_c(sintomas_cualquier_momento_cat,"_",fecha_prueba_inicio_sintomas_cualquier_momento_cat),
    # sintomas_cualquier_momento_cat=="sinto_covid" & !is.na(fecha_prueba_inicio_sintomas_cualquier_momento_cat) ~ str_c(sintomas_cualquier_momento_cat,"_",fecha_prueba_inicio_sintomas_cualquier_momento_cat)
    # sintomas_cualquier_momento_cat=="sinto_covid" & is.na(fecha_prueba_inicio_sintomas_cualquier_momento_cat_14d) ~ "sinto_covid_onset_u14d_missing",
    TRUE ~ sintomas_cualquier_momento_cat
  )) %>% 
  
  #' 
  #' [PRUEBAS PREVIAS POSITIVAS]
  #' 
  
  mutate(across(c(tipo_prueba_previa),
                str_replace_na,replacement = "")) %>% 
  mutate(prueba_previa_cat=str_c(prueba_previa,"_",tipo_prueba_previa,"_"#,
                                 #resultado_pr_previa,"_",resultado_pcr_previa
  )) #%>% 
  # mutate(prueba_previa_cat=if_else(prueba_previa_cat=="si_pr__",NA_character_,prueba_previa_cat)) #%>% 
  # mutate(prueba_previa_rec=if_else(tipo_prueba_previa=="pcr","si_pcr",prueba_previa_cat))
  
  # # fusion: también oligosintomaticos
  # mutate(sintomas_cualquier_momento_cat_fecha_14d_v2=case_when(
  #   
  #   sintomas_cualquier_momento_cat=="sinto_covid" & fecha_prueba_inicio_sintomas_cualquier_momento_cat_14d=="si" ~ "sinto_covid_onset_u14d_si",
  #   sintomas_cualquier_momento_cat=="sinto_covid" & fecha_prueba_inicio_sintomas_cualquier_momento_cat_14d=="no" ~ "sinto_covid_onset_u14d_no",
  #   # sintomas_cualquier_momento_cat=="sinto_covid" & is.na(fecha_prueba_inicio_sintomas_cualquier_momento_cat_14d) ~ "sinto_covid_onset_u14d_missing",
  #   
  #   sintomas_cualquier_momento_cat=="sinto_oligo" & fecha_prueba_inicio_sintomas_cualquier_momento_cat_14d=="si" ~ "sinto_oligo_onset_u14d_si",
  #   sintomas_cualquier_momento_cat=="sinto_oligo" & fecha_prueba_inicio_sintomas_cualquier_momento_cat_14d=="no" ~ "sinto_oligo_onset_u14d_no",
  #   # sintomas_cualquier_momento_cat=="sinto_oligo" & is.na(fecha_prueba_inicio_sintomas_cualquier_momento_cat_14d) ~ "sinto_oligo_onset_u14d_missing",
  #   TRUE ~ sintomas_cualquier_momento_cat
  # ))


# ___ qc contact and symptoms classification

uu_clean_data_pre %>%
  count(contacto_covid,
        #contacto_tipo,contacto_tipo_collapse,
        contacto_covid_tipo)


uu_clean_data_pre %>%
  # select(contains("sintomas"),contains("inicio")) %>% 
  select(rowname,
         dni,
         sintomas_cualquier_momento_cat,
         sintomas_si_no,
         fecha_inicio_sintomas,
         sintomas_previos, # problema
         fecha_inicio_sintomas_previo, # problema
         fecha_prueba_inicio_sintomas_diff_cat,
         fecha_prueba_inicio_sintomas_previo_diff_cat,
         fecha_prueba_inicio_sintomas_cualquier_momento_cat,
         fecha_prueba_inicio_sintomas_cualquier_momento_cat_14d,
         fecha_inicio_sintomas_14d) %>%
  filter(sintomas_cualquier_momento_cat=="sinto_covid") %>% 
  filter(sintomas_si_no=="si") %>%
  # filter(sintomas_previos=="si") %>%
  naniar::miss_var_summary()
  # view()
  # glimpse()
  # avallecam::print_inf()

uu_clean_data_pre %>%
  count(sintomas_cualquier_momento_cat,
        sintomas_cualquier_momento_cat_fecha_14d_v1,
        sintomas_cualquier_momento_cat_fecha_rangos,
        # sintomas_cualquier_momento_cat_fecha_14d_v2,
        fecha_prueba_inicio_sintomas_cualquier_momento_cat)

uu_clean_data_pre %>% 
  count(prueba_previa,tipo_prueba_previa,resultado_pr_previa,resultado_pcr_previa,
        prueba_previa_cat)


# _ pre step --------------------------------------------------------------



# uu_clean_data %>% count(totvivsel)

uu_clean_data_pre %>% 
  count(ig_clasificacion,positividad_peru)

uu_clean_data <- uu_clean_data_pre %>% #3224
  # filter by PR availability - last data analysis requirement
  mutate(ig_clasificacion=as.character(ig_clasificacion)) %>% 
  filter(ig_clasificacion!="missing") %>% #3212
  # filter by age availability - clean estimations
  filter(!is.na(edad)) #%>% 
  # filter(edad_decenios!="[100,Inf]") #3212

# _ WRITE clean data --------------------------------------------------------


uu_clean_data %>% 
  write_rds("data/uu_clean_data.rds")

# read_rds("data/uu_clean_data.rds") %>%
#   writexl::write_xlsx("data/uu_clean_data.xlsx")

# uu_clean_data %>% glimpse()

# __ reporte: flowchart --------------------------------------------------------------

hh_raw_data %>% dim()
pp_raw_data_pre %>% dim()
pp_raw_data %>% dim()
vv_raw_data %>% dim()
uu_raw_data_prelab %>% dim()
uu_raw_data %>% dim()
uu_clean_data_pre %>% dim()
uu_clean_data %>% dim()


# numero de conglomerados
# uu_raw_data_prelab %>% count(conglomerado)
# numero de viviendas
# uu_raw_data_prelab %>% 
#   count(cd_dist,conglomerado,numero_vivienda) %>% 
#   naniar::miss_var_summary()

# outcomes! ---------------------------------------------------------------

uu_clean_data %>% count(edad_quinquenal_raw) %>% avallecam::print_inf()

uu_clean_data %>% 
  count(sintomas_cualquier_momento,sintomas_si_no,sintomas_previos)

uu_clean_data %>% 
  count(presente_prueba,resultado_pr,resultado_pr2,ig_clasificacion,convResultado,positividad_peru)

uu_clean_data %>% 
  count(convResultado)

# _SINTOMAS!!! -------------------------------------------------------------

uu_clean_data %>% 
  select(contains("sinto")) %>% 
  glimpse()

#' 
#' [INCONSISTENCIAS]
#' 

# N=2 inconsistencias al futuro
uu_clean_data %>% 
  select(fecha_inicio_sintomas,fecha_prueba,fecha_prueba_inicio_sintomas_diff) %>% 
  arrange(desc(fecha_inicio_sintomas)) %>% 
  filter(fecha_prueba_inicio_sintomas_diff<0)

# N=1 inconsistencias al futuro
uu_clean_data %>% 
  select(fecha_inicio_sintomas_previo,fecha_prueba,fecha_prueba_inicio_sintomas_previo_diff) %>% 
  arrange(desc(fecha_inicio_sintomas_previo)) %>% 
  filter(fecha_prueba_inicio_sintomas_previo_diff<0)


#' 
#' [consulta] 
#' en columnas de observaciones
#' ¿se recuperará alguna información de ellas?
#' 
# uu_clean_data %>% 
#   select(observacion_sintomas) %>% 
#   filter(!is.na(observacion_sintomas)) %>% 
#   # avallecam::print_inf()
#   writexl::write_xlsx("table/previo/09-uu_clean_data-observacion_sintomas-revisar.xlsx")
# 
# uu_clean_data %>% 
#   select(observacion) %>% 
#   filter(!is.na(observacion)) %>% 
#   # avallecam::print_inf()
#   writexl::write_xlsx("table/previo/09-uu_clean_data-observacion-revisar.xlsx")

#' 
#' [nota]
#' sintomas cualquier momento integra las respuestas
#' en sintomas actual y sintomas previos
#' 
uu_clean_data %>% 
  select(contains("sinto")) %>% 
  count(sintomas_si_no,sintomas_previos,sintomas_cualquier_momento)
# # glimpse()
# naniar::miss_var_summary() %>% 
# avallecam::print_inf()

#' [nota]
#' con las correcciones ejecutadas, 
#' ya no se poseen valores perdidos en fechas
#' 
#' N=5 en actuales
#' N=3 en previos
#' 
uu_raw_data %>% 
  select(sintomas_si_no,fecha_inicio_sintomas) %>% 
  filter(sintomas_si_no=="si") %>% 
  naniar::miss_var_summary()
uu_clean_data %>% 
  select(sintomas_si_no,fecha_inicio_sintomas) %>% 
  filter(sintomas_si_no=="si") %>% 
  naniar::miss_var_summary()
# skimr::skim()
uu_raw_data %>% 
  select(sintomas_previos,fecha_inicio_sintomas_previo) %>% 
  filter(sintomas_previos=="si") %>% 
  naniar::miss_var_summary()
uu_clean_data %>% 
  select(sintomas_previos,fecha_inicio_sintomas_previo) %>% 
  filter(sintomas_previos=="si") %>% 
  naniar::miss_var_summary()

#' 
#' [notas]
#' fechas no se sobrelapan
#' fechas tienen registros inconsistentes
#' solucion:
#' no fueron modificadas, pero en la categorización se corrigieron 
#' segun las fechas de la toma de prueba o fin de estudio [ver codigo] 
#' 
# # complementos: fechas no se sobrelapan
uu_clean_data %>%
  # count(fecha_inicio_sintomas,fecha_inicio_sintomas_previo) %>%
  # avallecam::print_inf()
  select(fecha_inicio_sintomas,fecha_inicio_sintomas_previo) %>% 
  skimr::skim()

#' 
#' [nota]
#' clasificación en sinto, oligo o asinto de forma independiente 
#' en reportes actuales o previos
#' 
#' definicion
#' asintomático: sin síntoma cualquiera
#' oligosintomático: 1 síntoma sin anosmia (ageusia)
#' sintomático: solo anosmia (ageusia) OR al menos 2+ síntomas covid
#' 
#verificacion
uu_clean_data %>% 
  count(sintomas_si_no,sintomas_actual_total,
        sintomas_actual_covid,sintomas_actual_oligo,sintomas_anosmia)
#' un caso sin sintomas [ver codigo]
uu_clean_data %>% 
  count(sintomas_previos,sintomas_anterior_total,
        sintomas_anterior_covid,sintomas_anterior_oligo,sintomas_anterior_anosmia) 


#'
#' [nota]
#' verificar cruce de registros actuales, previs, cualquier momento
#' y su categorización en sinto, oligo, asinto y en intervalos de tiempo 
#' según fecha de inicio de síntomas en tres rangos
#' y otra variable con solo 14 dias
#' 
# caso mencionado
uu_clean_data %>% 
  count(sintomas_si_no,sintomas_previos,
        sintomas_cualquier_momento,
        sintomas_cualquier_momento_cat,
        fecha_prueba_inicio_sintomas_cualquier_momento_cat)
  # filter(sintomas_previos=="si",sintomas_anterior_total==0) %>% 
  # glimpse()
uu_clean_data %>% 
  count(sintomas_si_no,sintomas_previos,
        sintomas_cualquier_momento,
        sintomas_cualquier_momento_cat,
        fecha_prueba_inicio_sintomas_cualquier_momento_cat,
        fecha_prueba_inicio_sintomas_cualquier_momento_cat_14d)

# uu_clean_data %>% 
#   # select(contains("fecha")) %>% 
#   # glimpse()
#   # select(fecha_prueba,) %>% 
#   # naniar::miss_var_summary()
#   mutate(fecha_prueba=lubridate::ymd(fecha_prueba)) %>% 
#   count(fecha_prueba) %>% 
#   avallecam::print_inf()

#' 
#' [nota]
#' verificación independiente de la categorización de 
#' rangos de tiempo por fecha de inicio de sintomas
#' 
uu_clean_data %>% 
  count(sintomas_previos,fecha_inicio_sintomas_previo_14d)
uu_clean_data %>% 
  count(sintomas_previos,fecha_inicio_sintomas_previo_14d,
        fecha_inicio_sintomas_previo_60d,fecha_prueba_inicio_sintomas_previo_diff_cat)
uu_clean_data %>% 
  count(sintomas_si_no,fecha_inicio_sintomas_14d)
uu_clean_data %>% 
  count(sintomas_si_no,fecha_inicio_sintomas_14d,fecha_inicio_sintomas_60d,fecha_prueba_inicio_sintomas_diff_cat)
# group_by(fecha_inicio_sintomas_14d) %>%
# skimr::skim(fecha_prueba_inicio_sintomas_diff)


#' 
#' [nota]
#' clasificacion integrada en la variable cualquier momento
#' 
uu_clean_data %>% 
  count(#sintomas_si_no,sintomas_previos,
        sintomas_cualquier_momento,
        sintomas_cualquier_momento_cat,
        fecha_prueba_inicio_sintomas_cualquier_momento_cat,
        fecha_prueba_inicio_sintomas_cualquier_momento_cat_14d)

#' 
#' [nota]
#' lista extendida con numero de síntomas total
#'
# uu_clean_data %>% 
#   count(sintomas_si_no,sintomas_previos,sintomas_cualquier_momento,sintomas_actual_total,sintomas_anterior_total,
#         sintomas_cualquier_momento_cat,fecha_prueba_inicio_sintomas_cualquier_momento_cat) %>% 
#   avallecam::print_inf()

#' 
#' [nota]
#' las dos variables sintomas cualquier momento
#' fueron creadas con coalesce
#' 
uu_clean_data %>% 
  count(sintomas_si_no,sintomas_previos,
        sintomas_cualquier_momento,
        sintomas_actual_cat,
        sintomas_anterior_cat,
        sintomas_cualquier_momento_cat)
uu_clean_data %>% 
  count(sintomas_si_no,sintomas_previos,
        sintomas_cualquier_momento,
        fecha_prueba_inicio_sintomas_diff_cat,
        fecha_prueba_inicio_sintomas_previo_diff_cat,
        fecha_prueba_inicio_sintomas_cualquier_momento_cat)

#' 
#' [nota]
#' repetida verificacion de categorias y tiempo
#' 
uu_clean_data %>% 
  count(sintomas_si_no,
        sintomas_anosmia,
        sintomas_actual_total,
        sintomas_actual_oligo,
        sintomas_actual_covid,
        sintomas_actual_cat)
uu_clean_data %>% 
  count(sintomas_si_no,
        fecha_prueba_inicio_sintomas_diff_cat,
        fecha_inicio_sintomas_14d,
        fecha_inicio_sintomas_60d)
uu_clean_data %>% 
  count(sintomas_previos,
        sintomas_anterior_oligo,
        sintomas_anterior_covid,
        sintomas_anterior_cat)
uu_clean_data %>% 
  count(sintomas_previos,
        fecha_prueba_inicio_sintomas_previo_diff_cat,
        fecha_inicio_sintomas_previo_14d,
        fecha_inicio_sintomas_previo_60d)

#' 
#' [nota]
#' repetida verificacion de categorias y tiempo
#' 
uu_clean_data %>% 
  count(sintomas_si_no,sintomas_previos,
        sintomas_cualquier_momento,
        sintomas_cualquier_momento_cat,
        fecha_prueba_inicio_sintomas_cualquier_momento_cat,
        fecha_prueba_inicio_sintomas_cualquier_momento_cat_14d)

uu_clean_data %>% 
  count(#sintomas_si_no,
        #sintomas_previos,
        sintomas_cualquier_momento,
        sintomas_cualquier_momento_cat,
        # fecha_prueba_inicio_sintomas_cualquier_momento_cat,
        fecha_prueba_inicio_sintomas_cualquier_momento_cat_14d)

# _COVARIABLES!!! ---------------------------------------------------------

#' [contacto]

uu_clean_data %>% 
  select(contains("contac"),cuarentena) %>% 
  # skimr::skim()
  naniar::miss_var_summary()

uu_clean_data %>% 
  select(contains("contac"),cuarentena) %>% 
  filter(contacto_covid=="si") %>%
  # skimr::skim()
  naniar::miss_var_summary()

uu_clean_data %>% 
  select(contains("contac"),cuarentena) %>% 
  filter(contacto_covid=="si") %>% 
  janitor::tabyl(cuarentena,contacto_covid) %>% 
  avallecam::adorn_ame(denominator = "col")

uu_clean_data %>% 
  select(contains("contac"),cuarentena) %>% 
  filter(contacto_covid=="si") %>% 
  janitor::tabyl(contacto_tipo,contacto_covid) %>% 
  avallecam::adorn_ame(denominator = "col")

#' 
#' [vivienda]
#' 
uu_clean_data %>% 
  count(nro_dormitorios_cat)

#' 
#' [etnias]
#' 

uu_clean_data %>% 
  select(contains("etni"))%>% 
  janitor::tabyl(etnia,sort = T) %>% 
  janitor::adorn_pct_formatting()

uu_clean_data %>% 
  select(contains("etni"))%>% 
  count(etnia_cat,sort = T)

uu_clean_data %>% 
  filter(etnia_cat=="otros") %>% 
  count(etnia_cat,etnia_otros)


#' 
#' [TRABAJO]
#' 

uu_clean_data %>% 
  count(rubro)

uu_clean_data %>% 
  select(contains("trab"),ocupacion, contains("labo"), contains("rubr"),prof_salud) %>% 
  naniar::miss_var_summary()

uu_clean_data %>% 
  select(contains("trab"),ocupacion, contains("labo"), contains("rubr"),prof_salud) %>% 
  filter(trabajo_reciente=="si") %>% 
  naniar::miss_var_summary()

#' [RECATEGORIZABLE] - [OCUPACION]
# uu_clean_data %>%
#   select(contains("trab"),ocupacion, contains("labo"), contains("rubr"),prof_salud) %>%
#   filter(trabajo_reciente=="si") %>%
#   count(rubro,ocupacion,sort = T) %>%
#   # avallecam::print_inf()
#   writexl::write_xlsx("table/previo/10-20200810-ocupacion-pendiente_recategorizar.xlsx")

uu_clean_data %>% 
  select(contains("trab"),ocupacion, contains("labo"), contains("rubr"),prof_salud) %>% 
  filter(trabajo_reciente=="si") %>% 
  count(rubro,prof_salud,sort = T)


#' 
#' [SINTOMAS]
#' 

uu_clean_data %>% 
  select(contains("sinto"),fecha_prueba) %>% 
  select(contains("fecha")) %>% 
  # glimpse()
  skimr::skim()

#' 
#' [ATENCION EN SALUD]
#' 

uu_clean_data %>% 
  select(atencion,sintomas_si_no,sintomas_previos,atencion_sintomas,falto_labores,situa_hospitalizado) %>% 
  naniar::miss_var_summary()

uu_clean_data %>% 
  select(atencion,sintomas_si_no,sintomas_previos,atencion_sintomas,falto_labores,situa_hospitalizado) %>% 
  count(atencion)

uu_clean_data %>% 
  select(atencion,sintomas_si_no,sintomas_previos,atencion_sintomas,falto_labores,situa_hospitalizado) %>% 
  filter(sintomas_si_no=="si"|sintomas_previos=="si") %>% 
  naniar::miss_var_summary()

uu_clean_data %>% 
  select(atencion,sintomas_si_no,sintomas_previos,atencion_sintomas,falto_labores,situa_hospitalizado) %>% 
  filter(sintomas_si_no=="si"|sintomas_previos=="si") %>% 
  count(atencion_sintomas)

uu_clean_data %>% 
  select(atencion,sintomas_si_no,sintomas_previos,atencion_sintomas,falto_labores,situa_hospitalizado) %>% 
  filter(sintomas_si_no=="si"|sintomas_previos=="si") %>% 
  count(falto_labores)

uu_clean_data %>% 
  select(atencion,sintomas_si_no,sintomas_previos,atencion_sintomas,falto_labores,situa_hospitalizado) %>% 
  filter(sintomas_si_no=="si"|sintomas_previos=="si") %>% 
  count(situa_hospitalizado)

#' 
#' [RIESGO]
#' 


uu_clean_data %>% 
  select(edad,edad_etapas_de_vida_c,edad_etapas_de_vida_t,condicion_riesgo_mayor_60a) %>% 
  filter(edad_etapas_de_vida_c=="60a_mas") %>% 
  group_by(condicion_riesgo_mayor_60a) %>% 
  skimr::skim(edad)

# MISSINGS subset ---------------------------------------------------------

uu_clean_data %>% 
  mutate(edad_miss=case_when(is.na(edad)~"miss",edad>100~"cien",TRUE~"not")) %>% 
  # select(edad_miss,convResultado) %>% filter(edad_miss=="miss")
  count(edad_miss,presente_prueba,igg,igm,ig_clasificacion,positividad_peru)

uu_clean_data %>% 
  mutate(edad_miss=case_when(is.na(edad)~"miss",edad>100~"cien",TRUE~"not")) %>% 
  select(dni,nombre_completo,
         edad_miss,convResultado,ig_clasificacion,positividad_peru) %>% 
  filter(edad_miss=="not" & positividad_peru!="missing" & ig_clasificacion=="missing") %>% 
  writexl::write_xlsx("table/previo/08-20200719-missings-pcr_presente-pr_perdido-n38.xlsx")

uu_clean_data %>% 
  count(presente_prueba,igg,igm,ig_clasificacion,convResultado,positividad_peru)
# select(igg,igm,ig_clasificacion,positividad_peru) %>% 
# # naniar::miss_var_summary()
# skimr::skim()

# EDUCATION subset --------------------------------------------------------

uu_clean_data %>% 
  janitor::tabyl(nivel_academico) %>% 
  janitor::adorn_pct_formatting()

uu_clean_data %>% 
  filter(is.na(nivel_academico)) %>% 
  select(dni,tipo_doc,nombre_completo,#nombres,apellido_paterno,apellido_materno,
         edad,nivel_academico) %>% 
  # naniar::miss_var_summary() %>% 
  # avallecam::print_inf()
  # avallecam::print_inf()
  writexl::write_xlsx("table/previo/08-20200728-missings-nivel_educativo.xlsx")

# CALCULAR: nro viv ------------------------------------------------------


# __ conglome | viviendas | sujetos ------------------------------------------

uu_clean_data %>% 
  filter(conglomerado=="20787") %>% 
  count(numero_vivienda)

uu_clean_data %>% 
  select(cd_dist,conglomerado,numero_vivienda,numero_hogar,nro_convivientes,
         nro_dormitorios,participante,n_registros_vv,ind_hacin) %>% 
  # skimr::skim(ind_hacin)
  naniar::miss_var_summary()

# número total de viviendas 
uu_clean_data %>% 
  count(cd_dist,conglomerado,numero_vivienda)

distrito_conglomerado_nro_viviendas <- uu_clean_data %>% 
  count(cd_dist,nm_dist,conglomerado,numero_vivienda) %>% 
  # filter(conglomerado=="2783801")
  count(cd_dist,nm_dist,conglomerado)

distrito_conglomerado_nro_viviendas %>% 
  avallecam::print_inf()
# hh_recovered_observations
# uu_clean_data %>% 
#   # filter(is.na(conglomerado)) %>% 
#   filter(is.na(numero_vivienda)) %>% 
#   # glimpse()
#   select(dni,nombre_completo,conglomerado,numero_vivienda,
#          numero_hogar,`_index`,`_parent_index`) %>% 
  

distrito_conglomerado_nro_viviendas %>% 
  writexl::write_xlsx("table/previo/06-ubigeo-conglomerado_numero_de_viviendas.xlsx")

# diccionario_ponderaciones %>% 
#   filter(conglomerado=="24474")
# distrito_conglomerado_nro_viviendas %>% 
#   filter(conglomerado=="24474")
# uu_clean_data %>% 
#   filter(str_detect(nombre_completo,"______")) %>% 
#   select(conglomerado,numero_vivienda,numero_hogar,nombre_completo,participante,nro_convivientes,n_registros_vv)

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


