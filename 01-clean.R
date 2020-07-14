#' aim:
#' make preliminary cleaning previous to QC evaluation
#' make oficial cleaned db prior to export

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

pp_raw_data %>% 
  naniar::miss_var_summary() %>% 
  avallecam::print_inf()

# hh_raw_data %>% 
#   count(`_index`) %>% 
#   avallecam::print_inf()
# pp_raw_data %>% 
#   count(`_parent_index`) %>% 
#   avallecam::print_inf()

# union all ---------------------------------------------------------------

uu_raw_data <- full_join(pp_raw_data,hh_raw_data,
                         by=c("_parent_index"="_index")) %>% 
  # fix ubigeo-inei
  mutate(peru_dist=as.character(peru_dist)) %>% 
  mutate(peru_dist=case_when(
    str_length(peru_dist)==5~str_replace(peru_dist,"(.+)","0\\1"),
    TRUE~peru_dist
  )) %>% 
  left_join(ubigeo_diccionario) %>% 
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
  cdcper::cdc_edades_peru(edad)


uu_raw_data %>% 
  count(presente_prueba,resultado_pr,resultado_pr2,tipo_muestra_pcr,
        ig_clasificacion,igg,igm,igg_igm)

uu_raw_data %>% naniar::miss_var_summary() %>% 
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

# write to data -----------------------------------------------------------

uu_raw_data %>% 
  rownames_to_column() %>% 
  write_rds("data/uu_raw_data.rds")

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


