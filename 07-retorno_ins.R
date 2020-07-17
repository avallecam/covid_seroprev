#' 
#' OBJETIVO
#' - crear input de con resultados de px moleculares para ingresar a base final
#' write_rds("data/retorno_ins.rds")

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

consolidados <- read_rds("data/cdc-consolidados_a_ins.rds") %>% 
  filter(!is.na(n_final)) %>% 
  rename(dni=n_dni)

# ____________ ------------------------------------------------------------


# RETORNO ins -------------------------------------------------------------

# retorno_name <- "data-raw/retorno_ins/RESULTADOS AL 13.07.20.xlsx"
retorno_name <- "data-raw/retorno_ins/BASE_ESTUDIO_PREVALENCIA_16-07-10 AM.xlsx"

readxl::excel_sheets(retorno_name)

readxl::read_excel(retorno_name) %>% 
  count(EstatusResultado,convResultado)


readxl::read_excel(retorno_name) %>% 
  separate(DocIdentidad,into = c("doc","dni"),remove = F) %>% 
  count(doc)
  # filter(doc!="DNI") %>% 
  # count(convResultado)

# __RECHAZOS --------------------------------------------------------------



rechazo_rom <- readxl::read_excel(retorno_name) %>% 
  # glimpse()
  select(DocIdentidad,nombrePaciente,edad_retorno=edad,SexoPaciente,
         EstatusResultado,convResultado) %>% 
  # count(EstatusResultado,convResultado)
  separate(DocIdentidad,into = c("doc","dni"),remove = F) %>% 
  filter(EstatusResultado=="Rechazo Rom") %>% 
  select(-doc,-edad_retorno)

# uu_raw_data_pre
rechazo_rom

rechazo_rom_base_presente <- rechazo_rom %>% 
  inner_join(uu_raw_data_pre) 

rechazo_rom_base_presente %>% 
  count(convResultado,presente_prueba,ig_clasificacion,
        tipo_muestra_pcr,diris,nm_dist,conglomerado,sort = T) %>% 
  avallecam::print_inf()

rechazo_rom_base_presente %>% 
  writexl::write_xlsx("table/02-rechazo_rom_base_presente.xlsx")

rechazo_rom_base_ausente <- rechazo_rom %>% 
  anti_join(uu_raw_data_pre) #%>% 
  #count(convResultado)

rechazo_rom_base_ausente %>% 
  writexl::write_xlsx("table/02-rechazo_rom_base_ausente.xlsx")


# ___ cruzar con consolidado ----------------------------------------------

rechazo_rom %>% 
  inner_join(consolidados) %>% 
  glimpse()

rechazo_rom %>% 
  anti_join(consolidados)

# __ RETORNOS -------------------------------------------------------------


retorno_ins <- readxl::read_excel(retorno_name) %>% 
  # glimpse()
  select(DocIdentidad,nombrePaciente,edad_retorno=edad,SexoPaciente,
         EstatusResultado,convResultado) %>% 
  # count(EstatusResultado,convResultado)
  separate(DocIdentidad,into = c("doc","dni"),sep = " - ",remove = F) %>% 
  filter(EstatusResultado=="Resultado Verificado"| convResultado=="NEGATIVO") %>% 
  select(-doc,-edad_retorno)

retorno_ins %>% 
  count(convResultado)

retorno_ins %>% 
  count(dni,sort = T)
# dplyr::filter(num=="sin") %>% 
# count(EstatusResultado,convResultado)

retorno_ins %>% 
  filter(str_length(dni)<8) %>% 
  avallecam::print_inf()

retorno_ins %>% 
  write_rds("data/retorno_ins.rds")

# verificar ---------------------------------------------------------------

# retorno_ins %>% 
#   select(dni) %>% 
#   distinct()

retorno_ins %>% 
  inner_join(uu_raw_data_pre)

retorno_ins %>% 
  anti_join(uu_raw_data_pre) %>% 
  # avallecam::print_inf()
  writexl::write_xlsx("table/03-20200716-retorno_ins-anti_join-base_nominal.xlsx")


uu_raw_data_pre %>% 
  left_join(retorno_ins) %>% 
  count(convResultado)


# solucion ----------------------------------------------------------------


# por nombre --------------------------------------------------------------

retorno_ins %>% select(dni,contains("nombre"),contains("apelli"))
uu_raw_data_pre %>% select(dni,contains("nombre"),contains("apelli"))
