
# packages ----------------------------------------------------------------

library(tidyverse)
library(avallecam)
library(lubridate)
library(magrittr)

uu_raw_data_pre <- read_rds("data/uu_raw_data.rds") %>% 
  janitor::clean_names()

uu_raw_data <- uu_raw_data_pre %>% 
  select(numero_hogar:dni,username:ipress_name,parent_index)

uu_raw_data %>% glimpse()


# inputs ------------------------------------------------------------------

diccionario_conglomerado <- read_rds("data/inei-diccionario_conglomerado.rds") %>% 
  group_by(ubigeo,distrito,codccpp_bd,zona1_bd,conglomerado#,manzana,longitud,latitud
  ) %>% 
  summarise(totvivsel=sum(totvivsel)) %>% 
  ungroup() 

# verificar conglomerado --------------------------------------------------

a_evaluar <- uu_raw_data %>% 
  count(cd_depa,cd_prov,cd_dist,
        #nm_depa,nm_prov,
        nm_dist,
        conglomerado,
        # direccion,gps_latitude,gps_longitude
        sort = T) %>% 
  left_join(diccionario_conglomerado,by = c("conglomerado","cd_dist"="ubigeo"))

# dividir -----------------------------------------------------------------


si_vinculado <- a_evaluar %>% 
  # naniar::miss_var_summary()
  filter(!is.na(totvivsel))

no_vinculado <- a_evaluar %>% 
  # naniar::miss_var_summary()
  filter(is.na(totvivsel))

# evaluar -----------------------------------------------------------------

si_vinculado %>% 
  avallecam::print_inf()

no_vinculado %>% 
  avallecam::print_inf()

inner_join(
  si_vinculado %>% select(cd_dist,conglomerado),
  no_vinculado %>% select(cd_dist,conglomerado)
)

no_vinculado_problematic <- no_vinculado %>% 
  select(cd_dist,conglomerado) %>% 
  mutate(problematic="yes")

# rescatar ----------------------------------------------------------------

detect_conglomerado <- function(string) {
  diccionario_conglomerado %>% 
    filter(str_detect(conglomerado,string))
}

# diccionario_conglomerado %>% 
#   # filter(ubigeo=="150110") %>%
#   filter(str_detect(conglomerado,"20437")) %>% #otro distrito
#   # filter(conglomerado=="21010")
#   avallecam::print_inf()
# 
# no_vinculado %>% 
#   filter(magrittr::is_in(conglomerado,c("22","1")))

no_vinculado %>% 
  select(cd_dist:n) %>% 
  mutate(evaluate=map(.x = conglomerado,.f = ~detect_conglomerado(string = .x))) %>% 
  mutate(filter=map_int(.x = evaluate,.f = nrow)) %>% 
  filter(filter==0) %>% 
  avallecam::print_inf()

no_vinculado %>% 
  select(cd_dist:n) %>% 
  mutate(evaluate=map(.x = conglomerado,.f = ~detect_conglomerado(string = .x))) %>% 
  mutate(filter=map_int(.x = evaluate,.f = nrow)) %>% 
  filter(filter==1) %>% 
  rename(conglomerado_enkobo=conglomerado) %>%
  unnest(evaluate) %>%
  avallecam::print_inf()

# problemas ---------------------------------------------------------------

#' ubigeo-conglomerados mal digitados
#' 150140-3556
#' 150105-22

uu_raw_data_pre %>% 
  filter(conglomerado=="22") %>% 
  count(username) %>% 
  glimpse()

# soluiton ----------------------------------------------------------------

no_vinculado %>% summarise(sum(n))

recovery_output <- uu_raw_data %>% 
  left_join(no_vinculado_problematic) %>% 
  filter(problematic=="yes")

recovery_output %>% glimpse()
recovery_output %>% count(username)

recovery_output #%>% 
  # writexl::write_xlsx("table/00-20200713-conglomerado-no_vinculado.xlsx")


# ______ ------------------------------------------------------


# viviendas por conglomerado? ---------------------------------------------


uu_raw_data %>%
  filter(is_in(conglomerado,si_vinculado$conglomerado)) %>% 
  count(nm_dist,cd_dist,conglomerado,numero_vivienda) %>% 
  arrange(cd_dist,conglomerado) %>% 
  # count(cd_dist,conglomerado)
  # avallecam::print_inf()
  writexl::write_xlsx("table/00-20200713-numeracion-viviendas_por_conglomerado.xlsx")

uu_raw_data %>%
  filter(is_in(conglomerado,si_vinculado$conglomerado)) %>% 
  # filter(conglomerado=="25738") %>% 
  # filter(str_detect(numero_vivienda,"azar"))
  mutate(numero_vivienda=if_else(
    condition = numero_vivienda=="azar",
    true = str_c(numero_vivienda,"-",parent_index),
    false = numero_vivienda)) %>%
  count(nm_dist,cd_dist,conglomerado,numero_vivienda,direccion,parent_index) %>% 
  # filter(conglomerado=="25738")
  # avallecam::print_inf()
  arrange(cd_dist,conglomerado) %>% 
  select(-n) %>% 
  count(nm_dist,cd_dist,conglomerado) %>% 
  arrange(cd_dist,conglomerado) %>% 
  rename(cantidad_viviendas=n) %>% 
  # avallecam::print_inf()
  writexl::write_xlsx("table/00-20200713-cantidad-viviendas_por_conglomerado.xlsx")


# registros observados por vivienda (esperados) ---------------------------



uu_raw_data %>% 
  # glimpse()
  filter(is_in(conglomerado,si_vinculado$conglomerado)) %>% 
  select(nm_dist,cd_dist,conglomerado,numero_vivienda,participante) %>% 
  group_by(nm_dist,cd_dist,conglomerado,numero_vivienda,participante) %>% 
  summarise(n_registros=n()) %>% 
  ungroup() %>% 
  # filter(conglomerado=="23820")
  # filter((participante!=n_registros) | (participante=="0")) %>% 
  # avallecam::print_inf()
  writexl::write_xlsx("table/00-20200713-registros_por_vivienda-total.xlsx")


uu_raw_data %>% 
  # glimpse()
  filter(is_in(conglomerado,si_vinculado$conglomerado)) %>% 
  select(nm_dist,cd_dist,conglomerado,numero_vivienda,participante) %>% 
  group_by(nm_dist,cd_dist,conglomerado,numero_vivienda,participante) %>% 
  summarise(n_registros=n()) %>% 
  ungroup() %>% 
  # filter(conglomerado=="23820")
  filter((participante!=n_registros) | (participante=="0")) %>% 
  # avallecam::print_inf()
  writexl::write_xlsx("table/00-20200713-registros_por_vivienda-solo_diferentes.xlsx")


# test parent index to solve azar -----------------------------------------

uu_raw_data_pre %>% 
  select(conglomerado,id,id_2,uuid,parent_index,submission_id,
         submission_uuid,numero_vivienda,direccion) %>% 
  filter(conglomerado=="25738") %>% view()


# missings ----------------------------------------------------------------

consolidados <- read_rds("data/cdc-consolidados_a_ins.rds") %>% 
  filter(!is.na(n_final))

consolidados %>% 
  writexl::write_xlsx("table/00-20200714-consolidados_a_ins.xlsx")

uu_raw_data %>% 
  filter(magrittr::is_in(dni,consolidados %>% pull(n_final))) %>% 
  writexl::write_xlsx("table/00-20200714-nominal-en_consolidadoss.xlsx")

uu_raw_data %>% 
  # filter(!is.na(presente_prueba)) %>% 
  filter(is.na(resultado_pr)) %>% 
  filter(magrittr::is_in(dni,consolidados %>% pull(n_final))) %>% 
  writexl::write_xlsx("table/00-20200714-nominal_sin_resultado_pr-en_consolidadoss.xlsx")


# RETORNO ins -------------------------------------------------------------

retorno_ins <- readxl::read_excel("data-raw/retorno_ins/RESULTADOS AL 13.07.20.xlsx") %>% 
  # glimpse()
  select(DocIdentidad,nombrePaciente,edad_retorno=edad,SexoPaciente,
         EstatusResultado,convResultado) %>% 
  # count(EstatusResultado,convResultado)
  separate(DocIdentidad,into = c("doc","dni"),remove = F) %>% 
  filter(EstatusResultado=="Resultado Verificado"| convResultado=="NEGATIVO")

retorno_ins %>% 
  count(dni,sort = T)
  # dplyr::filter(num=="sin") %>% 
  # count(EstatusResultado,convResultado)
  
retorno_ins %>% 
  write_rds("data/retorno_ins.rds")
  

uu_raw_data_pre %>% 
  left_join(retorno_ins) %>% 
  count(convResultado)
