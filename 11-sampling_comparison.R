#' 
#' OBJECTIVES:
#' - performance of survey comparing to expected (census 2017 and inei 2020)
#' write_xlsx("table/02-seroprev-supp-table02.xlsx")
#' ggsave("figure/33-seroprev-supp-figure01-a.png")
#' ggsave("figure/33-seroprev-supp-figure01-b.png")
#' 


library(tidyverse)
library(magrittr)

theme_set(theme_bw())

# reunis inei -------------------------------------------------------------

# source: 
# https://www.minsa.gob.pe/reunis/index.asp
# https://www.minsa.gob.pe/reunis/data/poblacion_estimada.asp
# https://cloud.minsa.gob.pe/s/XJ3NoG3WsxgF6H8
rute_reunis <- "data-raw/population/Poblacion Peru 2020 Dpto Prov Dist Final INEI-actualizado.xlsx"

data_reunis <- cdcper::read_reunis_edad_quinquenio(file = rute_reunis,
                                                   year = 2020)
popstr_reference <- data_reunis %>% 
  filter(provincia=="LIMA"|provincia=="CALLAO") %>% 
  group_by(ano,sex,age) %>% 
  summarise(ref_sum_value=sum(value)) %>% 
  ungroup() %>% 
  mutate(ref_pct_value=ref_sum_value/sum(ref_sum_value))
  # count(sex,age) %>% 
  # avallecam::print_inf()

# for 13-epicurve
popstr_reference %>% 
  group_by(ano) %>% 
  summarise(across(starts_with("ref_"),sum)) %>% 
  write_rds("data/local_population.rds")

# local sutdy -------------------------------------------------------------


uu_clean_data <- read_rds("data/uu_clean_data.rds")

popstr_study <- uu_clean_data %>% 
  mutate(edad_80=if_else(edad>=80,"[80,Inf)",as.character(edad_quinquenal_raw))) %>% 
  count(sexo,edad_quinquenal_raw,edad_80) %>%
  group_by(sexo,edad_80) %>% 
  summarise(loc_sum_value=sum(n)) %>% 
  ungroup()  %>% 
  mutate(loc_pct_value=loc_sum_value/sum(loc_sum_value)) %>% 
  mutate(sex=case_when(
    sexo=="femenino"~"m",
    sexo=="masculino"~"h"
  )) %>% 
  mutate(age_end=str_replace(edad_80,"\\[(.+),(.+)\\)","\\2")) %>% 
  mutate(age_end=as.numeric(age_end)-1) %>% 
  mutate(age_ini=str_replace(edad_80,"\\[(.+),(.+)\\)","\\1_")) %>% 
  mutate(age_all=str_c(age_ini,age_end,"a")) %>% 
  mutate(age=case_when(
    age_all=="0_0a" ~ "00_00a",
    age_all=="1_4a" ~ "01_04a",
    age_all=="5_9a" ~ "05_09a",
    age_all=="80_Infa" ~ "80_nna",
    TRUE~age_all
  )) %>% 
  select(-edad_80,-age_end,-age_ini,-age_all,-sexo)
  # count(edad_quinquenal_raw,edad_80) %>% 
  # count(sexo,edad_quinquenal_raw) %>% 
  # avallecam::print_inf()


# compare -----------------------------------------------------------------

popstr_reference %>% count(sex)
popstr_reference %>% count(age)
popstr_study %>% count(sex)
popstr_study %>% count(age)

data_to_pyramid <- popstr_reference %>% 
  left_join(popstr_study) %>% 
  mutate(sex_label=case_when(
    # sex=="m"~"Mujer con PR",
    # sex=="h"~"Hombre con PR"
    sex=="m"~"Female",
    sex=="h"~"Male"
  )) %>% 
  mutate(diff=loc_pct_value-ref_pct_value) %>% 
  mutate(age_label=str_replace(age,"_","-")) %>% 
  mutate(age_label=str_replace(age_label,"a","")) %>% 
  mutate(age_label=str_replace(age_label,"nn","+")) %>% 
  mutate(age_label=str_replace(age_label,"\\-\\+","+")) %>%
  mutate(age_label=str_replace(age_label,"00-00","<0"))

data_to_pyramid %>% 
  avallecam::print_inf()

# pyramid plot ------------------------------------------------------------

library(ggpol)

aplot <- data_to_pyramid %>% 
  mutate(ref_pct_value=ref_pct_value*100,
         loc_pct_value=loc_pct_value*100) %>% 
  mutate(ref_pct_value=if_else(sex=="m",-ref_pct_value,ref_pct_value),
         loc_pct_value=if_else(sex=="m",-loc_pct_value,loc_pct_value)) %>%
  mutate(sex=sex_label) %>% 
  mutate(age=age_label) %>% 
  # mutate(sex=fct_relevel(sex,"Mujer con PR")) %>% 
  mutate(sex=fct_relevel(sex)) %>% 
  ggplot() +
  geom_bar(aes(age,loc_pct_value,fill=sex),stat = "identity") +
  geom_bar(aes(age,ref_pct_value),color="black",alpha=0,stat = "identity") +
  facet_share(~sex, dir = "h", scales = "free", reverse_num = TRUE) +
  coord_flip() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5),
                     #labels = scales::percent_format(accuracy = 1)
                     ) +
  colorspace::scale_fill_discrete_qualitative(rev=T) +
  labs(
    # title = "Distribución de participantes por edad y sexo con Prueba Rápida (PR)",
    # subtitle = "Comparación con población en Lima y Callao para el 2020 (Fuente: REUNIS 2020)",
    # x="Edad (años)",y="Sujetos (%)",fill="Participantes"
    title = "Distribution of participants against population by age and sex",
    subtitle = "Reference: Lima Metropolitan Area estimates (Source: REUNIS 2020)",
    x="Age (years)",y="Participants (%)",
    fill="Participants with\na serological test\nresult"
  ) #+
  # scale_fill_manual(alapha)
  # coord_flip()
# aplot
ggsave("figure/33-seroprev-supp-figure01-a.png",height = 5,width = 9,dpi = "retina")

# % difference plot -------------------------------------------------------

data_to_pyramid %>% 
  mutate(across(c(contains("_pct_"),diff), ~ .*100)) %>% 
  writexl::write_xlsx("table/02-seroprev-supp-table02.xlsx")

# summary results
data_to_pyramid %>% 
  filter(sex=="h") %>% 
  filter(magrittr::is_in(age,
                         c("25_29a","30_34a","35_39a","40_44a",
                           "45_49a","50_54a","55_59a"#,"60_64a"
                           ))) %>% 
  summarise(across(diff,list(sum=sum,mean=mean,median=median)))

# ggplot
bplot <- data_to_pyramid %>% 
  mutate(sex=sex_label) %>% 
  mutate(age=age_label) %>% 
  ggplot(aes(age,diff,fill=sex)) +
  geom_col(position = position_dodge()) +
  geom_hline(aes(yintercept=0),lty=2) +
  colorspace::scale_fill_discrete_qualitative(rev=T) +
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = scales::breaks_pretty(10)) +
  coord_flip() +
  labs(
    # title = "Diferencia entre participantes y población por edad y sexo.",
    # subtitle = "Población en Lima y Callao para el 2020 (Fuente: REUNIS 2020)",
    # x="Edad (años)",
    # y="Diferencia (%)\n % participantes - % poblacion",
    # fill=""
    title = "Difference between participants and population by age and sex",
    subtitle = "Reference: Lima Metropolitan Area estimates (Source: REUNIS 2020)",
    x="Age (years)",
    y="Difference (%)\n % participants - % population",
    fill="Participants with\na serological test\nresult"
  )
ggsave("figure/33-seroprev-supp-figure01-b.png",height = 5,width = 8,dpi = "retina")
# aplot
# bplot
