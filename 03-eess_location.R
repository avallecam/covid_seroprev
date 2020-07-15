#' 
#' OBJETIVO:
#' - crear un diccionario de conglomerado y eess
#' write_rds("data/inei-diccionario_conglomerado.rds")

library(tidyverse)

diccionario_conglomerado <- readxl::read_excel("data-raw/BD MZ DE VIV SELECCIONADAS COVID.XLSX") %>% 
  mutate(ubigeo=str_c(CCDD_BD,CCPP_BD,CCDI_BD)) %>% 
  janitor::clean_names() %>% 
  select(ubigeo,distrito:latitud)

diccionario_conglomerado %>% 
  count(
    ubigeo,
    codccpp_bd,
    # zona1_bd,
    # conglomerado,
    # sort = T
  )

diccionario_conglomerado %>% count(totvivsel)

diccionario_conglomerado %>% 
  write_rds("data/inei-diccionario_conglomerado.rds")

diccionario_conglomerado %>% 
  ggplot(aes(x = longitud,y = latitud)) +
  geom_point(size=5,alpha=0.1) +
  coord_fixed()
