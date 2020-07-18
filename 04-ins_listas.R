#' 
#' OBJETIVO
#' - crear consolidado
#' write_rds("data/cdc-consolidados_a_ins.rds")

library(tidyverse)
library(magrittr)

# extraer nombre de archivos ----------------------------------------------


path <- "data-raw/muestras/"
pattern <- ".xlsx$"
file_name <- list.files(path = path,
           pattern = pattern,
           full.names = T) %>%
  enframe(name = NULL) %>%
  bind_cols(pmap_df(., file.info)) %>% 
  select(value)

# explorar patron ---------------------------------------------------------


n <- 8

readxl::read_excel(path = file_name %>% slice(n) %>% pull(value),skip = 5) %>% 
  janitor::clean_names() %>% 
  filter(!is.na(n_del_tubo))

readxl::read_excel(path = file_name %>% slice(n) %>% pull(value)) %>% 
  slice(1) %>% 
  pull(1)

# definir acciones --------------------------------------------------------


read_ins <- function(value,n_skip) {
  readxl::read_excel(path = value,skip = n_skip) %>% 
    janitor::clean_names() %>% 
    filter(!is.na(n_del_tubo)) %>% 
    select(n_del_tubo:nombre_y_apellido_del_tomador_de_muestra) %>% 
    set_colnames(c("n_del_tubo", "n_dni", "otro_documento","n_documento",
                   "nombres_y_apellidos_del_paciente",
                   "nombre_y_apellido_del_tomador_de_muestra"))
}

# leer en masa ------------------------------------------------------------


consolidados <- file_name %>% 
  mutate(n_skip=c(rep(4,10),rep(5,2))) %>% 
  # slice(1:8) %>%
  mutate(read_all=pmap(.l = select(.,value,n_skip),.f = read_ins)) %>% 
  mutate(value=str_replace(value,"data-raw/muestras/","")) %>% 
  select(-n_skip) %>% 
  unnest(cols = c(read_all)) %>% 
  mutate(n_final=if_else(is.na(n_dni),n_documento,n_dni))

# grabar ------------------------------------------------------------------


consolidados %>% 
  filter(!is.na(n_final)) %>% 
  # naniar::miss_var_summary()
  write_rds("data/cdc-consolidados_a_ins.rds")

# perdidos ----------------------------------------------------------------


# consolidados %>% naniar::miss_var_summary()
consolidados %>% 
  filter(is.na(n_dni)) %>% 
  # naniar::miss_var_summary()
  filter(is.na(n_documento)) #%>% 
# naniar::miss_var_summary()

# duplicados --------------------------------------------------------------


consolidados %>% 
  distinct()


# consolidados %>% 
#   count(n_del_tubo,sort = T) %>% 
#   filter(n>1) %>% 
#   avallecam::print_inf()

consolidados %>% 
  group_by(n_final) %>% 
  filter(n()>1) #%>% 
  #view()

consolidados %>% 
  group_by(value,
           n_del_tubo) %>% 
  filter(n()>1) #%>% 
  # ungroup() %>% 
  # arrange(n_del_tubo) %>% 
  # select(value:n_dni)


# digitacion de codigo de tubo ---------------------------------------------

consolidados %>% 
  count(str_length(n_del_tubo))

consolidados %>% 
  select(n_del_tubo) %>% 
  filter(str_length(n_del_tubo)>14)

consolidados %>% 
  select(n_del_tubo) %>% 
  filter(str_length(n_del_tubo)==12)
