library(tidyverse)
library(magrittr)
dict_name <- "notrack/Encuesta ESPI_fisico.xlsx"
survey <- readxl::read_excel(dict_name,sheet = 1)
choices <- readxl::read_excel(dict_name,sheet = 2)
share_p01_v01 <- survey %>% 
  select(name,label) %>% 
  filter(!is.na(name)) %>% 
  rownames_to_column() %>% 
  mutate(rowname=as.numeric(rowname)) %>% 
  # keep for ins
  mutate(va=case_when(
    is_in(rowname,c(4:7,18:27,31:33,36:41,56:58,60:65,81:84))~"x",
    TRUE~NA_character_
  )) %>% 
  # remove
  filter(!is_in(rowname,c(16,1:3,8:17,28:30,34:35,42:48,49,51:55,62,66:67,71:74,76:80))) %>% 
  avallecam::print_inf()

share_p02_v01 <- choices %>% 
  select(1:3) %>% 
  filter(is_in(list_name,c("sintomas","condicion_riesgo"))) %>% 
  avallecam::print_inf()

union_all(share_p01_v01,share_p02_v01) %>% 
  # avallecam::print_inf()
  writexl::write_xlsx("notrack/00-dictionario_ins.xlsx")
