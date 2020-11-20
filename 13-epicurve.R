#' 
#' OBJECTIVES:
#' - interventions + oficial reports plot and tables
#' writexl::write_xlsx("table/02-seroprev-supp-table05.xlsx")
#' writexl::write_xlsx("table/02-seroprev-supp-table06.xlsx")
#' ggsave("figure/03-seroprev-figure04.png")
#' ggsave("figure/03-seroprev-figure03.png")
#' 

library(tidyverse)
library(covidPeru)
library(readr)
library(covid19viz)
library(cdcper)
library(lubridate)
theme_set(theme_bw())


# analysis time limits --------------------------------------------------------------

min_analysis_date <- ymd(20200301)
max_analysis_date <- ymd(20201101)

# ______________ ----------------------------------------------------------


# covid19viz R package ----------------------------------------------------

# _import intervention data ------------------------------------------------

unesco <- read_unesco_education()
# acaps <- read_acaps_governments()

# no data in ACAPS
# acaps %>% filter(ISO3=="PER")

unesco_peru <- unesco %>% 
  filter(ISO=="PER") %>% 
  mutate(Date=dmy(Date)) %>% 
  group_by(Status) %>% 
  summarise(date_min=min(Date),
            date_max=max(Date)) %>% 
  rename(intervention_label=Status) %>% 
  mutate(intervention=case_when(
    intervention_label=="Closed due to COVID-19" ~ "closed",
    intervention_label=="Partially open" ~ "partial"
  ))
unesco_peru

# _unite intervention data -------------------------------------------------

interventions <- tibble(
  date_min = ymd(20200628), 
  date_max = ymd(20200709),
  intervention_label = "Seroprevalence study",
  intervention = "seroprev"
) %>% 
  union_all(
    unesco_peru %>% 
      mutate(date_max=if_else(date_max==max(date_max),
                              max_analysis_date,
                              date_max))
  ) %>% 
  filter(!is.na(intervention))
interventions

interventions %>% 
  writexl::write_xlsx("table/02-seroprev-supp-table05.xlsx")

# ______________ ----------------------------------------------------------

# mobility data -----------------------------------------------------------

mobility <- read_google_region_country(country_iso = "PE")
# mobility %>% count(sub_region_1) %>% print(n=Inf)
# mobility %>% count(country_region,sub_region_1,sub_region_2,metro_area) %>% print(n=Inf)
mobility_lima <- mobility %>% 
  filter(date<max_analysis_date) %>% 
  filter(
    magrittr::is_in(sub_region_1,c("Metropolitan Municipality of Lima",
                                   "Callao Region"))) %>% 
  mutate(across(c(sub_region_1,sub_region_2,metro_area),
                str_replace_na,replacement = "")) %>% 
  mutate(subregion=str_c(sub_region_1,"\n",sub_region_2,"\n",metro_area)) %>% 
  mutate(subregion=str_trim(subregion)) %>% 
  mutate(subregion=if_else(sub_region_1=="Metropolitan Municipality of Lima",
                           "Metropolitan Municipality\nof Lima",subregion)) %>% 
  # count(country_region,subregion)
  filter(!(sub_region_1=="Metropolitan Municipality of Lima" &
             sub_region_2=="")) %>%
  # count(country_region,sub_region_1,sub_region_2,metro_area,subregion)
  # pivot_longer()
  pivot_longer(cols = -c(country_region_code:date,subregion),
               names_to = "field",
               names_pattern = "(.+)_percent_change_from_baseline",
               values_to = "percent_change_from_baseline")

# _ plot it ---------------------------------------------------------------

mobility_lima %>% 
  mutate(field=str_replace_all(field,"_"," "),
         field=str_to_sentence(field)) %>% 
  ggplot() +
  geom_rect(data = interventions,
            aes(xmin = date_min, xmax = date_max, 
                ymin = -Inf, ymax = Inf, 
                fill =intervention_label),
            alpha=0.2) +
  geom_hline(aes(yintercept=0),lty=2) +
  # geom_line(aes(x = date,percent_change_from_baseline, color=subregion)) +
  geom_smooth(aes(x = date,percent_change_from_baseline, color=subregion),
              span = 0.1) +
  geom_vline(data = interventions %>% 
               filter(intervention=="seroprev"),
             aes(xintercept = date_min), lty=3) +
  geom_vline(data = interventions %>% 
               filter(intervention=="seroprev"),
             aes(xintercept = date_max), lty=3) +
  facet_wrap(~field) +
  colorspace::scale_color_discrete_qualitative() +
  scale_x_date(date_breaks = "1 month",date_labels = "%b") +
  # theme(legend.position="bottom")
  labs(title = "Government interventions and Mobility reports",
       subtitle = "Lima Metropolitan Area and Callao Region, Peru 2020",
       x = "Date",
       y = "% change from baseline",
       fill = "Interventions",
       color = "Region")
ggsave("figure/03-seroprev-figure04.png",dpi = "retina",height = 3.5,width = 8)

# ______________ ----------------------------------------------------------


# covidPeru R package -----------------------------------------------------


# _import surveillance data ------------------------------------------------

positivos <- da_positivos()
fallecidos <- da_fallecidos()
sinadef <- da_sinadef() %>% 
  rename(PROVINCIA=`PROVINCIA DOMICILIO`)

positivos_age <- positivos %>% 
  as_tibble() %>% 
  cdcper::cdc_edades_peru(variable_edad = EDAD) %>% 
  select(fecha,year,semana,DEPARTAMENTO,PROVINCIA,EDAD,
         edad_etapas_de_vida_c)

fallecidos_age <- fallecidos %>% 
  as_tibble() %>% 
  rename(EDAD=EDAD_DECLARADA) %>% 
  # glimpse()
  cdcper::cdc_edades_peru(variable_edad = EDAD) %>% 
  select(fecha,year,semana,DEPARTAMENTO,PROVINCIA,EDAD,
         edad_etapas_de_vida_c)

# _count by epiweek + unite --------------------------------------------------------


# from 11-sampling_comparison
local_population <- read_rds("data/local_population_age.rds") %>% 
  rename(edad=age) %>% 
  mutate(edad=as.factor(edad)) %>% 
  mutate(edad=fct_relabel(.f = edad,.fun = str_replace,"_","-")) %>% 
  mutate(edad=fct_relabel(.f = edad,.fun = str_replace,"a","")) %>% 
  mutate(edad=fct_relabel(.f = edad,.fun = str_replace,"-mas","+")) %>% 
  select(-ano)

summarize_epiweek <- function(data,source,variable) {
  data %>% 
    filter(PROVINCIA=="LIMA"|PROVINCIA=="CALLAO") %>% 
    filter(fecha > min_analysis_date) %>% 
    filter(fecha < max_analysis_date) %>% 
    cdcper::cdc_yearweek_to_date(year_integer = year,week_integer = semana) %>% 
    count(epi_date,{{variable}}) %>% 
    mutate(source={{source}})
}

peru_sources_age <- positivos_age %>% 
  summarize_epiweek(variable = edad_etapas_de_vida_c,
                    source = "COVID-19 Cases") %>% 
  rename(edad=edad_etapas_de_vida_c) %>% 
  mutate(edad=fct_relabel(.f = edad,.fun = str_replace,"_","-")) %>% 
  mutate(edad=fct_relabel(.f = edad,.fun = str_replace,"a","")) %>% 
  mutate(edad=fct_relabel(.f = edad,.fun = str_replace,"-mas","+")) %>% 
  select(-source) %>% 
  rename(cases=n) %>% 
  # mutate(source=fct_relabel(.f = edad,
  #                           .fun = str_replace,
  #                           "(.+)",
  #                           "COVID-19 Cases [\\1]")) %>% 
  # select(-edad) %>% 
  full_join(
    fallecidos_age %>% 
      summarize_epiweek(variable = edad_etapas_de_vida_c,
                        source = "COVID-19 Deaths") %>% 
      rename(edad=edad_etapas_de_vida_c) %>% 
      mutate(edad=fct_relabel(.f = edad,.fun = str_replace,"_","-")) %>% 
      mutate(edad=fct_relabel(.f = edad,.fun = str_replace,"a","")) %>% 
      mutate(edad=fct_relabel(.f = edad,.fun = str_replace,"-mas","+")) %>% 
      select(-source) %>% 
      rename(deaths=n) 
      # mutate(source=fct_relabel(.f = edad,
      #                           .fun = str_replace,
      #                           "(.+)",
      #                           "COVID-19 Deaths [\\1]"))
  ) %>% 
  # union_all(
  #   # fallecidos %>% 
  #   #   summarize_epiweek(source = "Confirmed Deaths")
  #   fallecidos %>%
  #     summarize_epiweek(source = "Deaths") %>%
  #     rename(Deaths=n) %>%
  #     select(-source) %>%
  #     full_join(
  #         sinadef %>%
  #           summarize_epiweek(source = "Sinadef") %>%
  #           rename(Sinadef=n) %>%
  #           select(-source)
  #     ) %>%
  #     rowwise(epi_date) %>%
  #     mutate(n = sum(c_across(Deaths:Sinadef),na.rm = T)) %>%
  #     ungroup() %>%
  #     select(-Deaths,-Sinadef) %>%
  #     mutate(source = "All causes of Deaths\n(including COVID-19)")
  # )
  mutate(deaths=replace_na(deaths,0)) %>% 
  pivot_longer(cols = c(cases,deaths), 
               names_to = "source", 
               values_to = "number") %>% 
  left_join(local_population) %>% 
  filter(!is.na(edad)) %>% 
  group_by(source,edad) %>% 
  mutate(cumsum=cumsum(number)) %>%
  mutate(ratio_100m=(number/ref_sum_value)*100000) %>% 
  mutate(cumsum_100m=(cumsum/ref_sum_value)*100000) %>% 
  ungroup() %>% 
  mutate(epi_week=epiweek(epi_date)) %>% 
  select(epi_date,epi_week,everything()) #%>% 
  # mutate(edad=fct_explicit_na(edad))

# _________ ---------------------------------------------------------------


# OUTPUT ------------------------------------------------------------------


# __ table output ---------------------------------------------------------


peru_sources_age %>% 
  select(-ref_sum_value) %>% 
  # completely tidy
  pivot_longer(cols = c(number:cumsum_100m), 
               names_to = "measurement", 
               values_to = "value") %>% 
  # mix
  mutate(type=case_when(
    str_detect(measurement,"100m")~"ratio_100m",
    TRUE~"not"
  )) %>% 
  mutate(measurement=case_when(
    str_detect(measurement,"ratio")~"number",
    str_detect(measurement,"cumsum")~"cumsum",
    TRUE ~ measurement
  )) %>% 
  # mutate(mix=str_c(edad,"-",measurement)) %>% 
  # select(-edad,-measurement) %>% 
  mutate(mix=str_c(edad,"-",type)) %>% 
  select(-edad,-type) %>% 
  pivot_wider(id_cols = -mix, 
              names_from = mix, 
              values_from = value) %>% 
  select(epi_date:`0-11-ratio_100m`,
         `12-17-not`,`12-17-ratio_100m`,
         everything()) %>% 
  mutate(across(.cols = contains("ratio_100"),.fns = round,digits=2)) %>% 
  mutate(across(.cols = contains("ratio_100"),.fns = as.character)) %>% 
  arrange(source,epi_date) %>%
  select(source,everything()) %>% 
  # avallecam::print_inf()
  # view()
  writexl::write_xlsx("table/02-seroprev-supp-table06.xlsx")

peru_sources_age_long <- peru_sources_age %>% 
  select(-(number:cumsum)) %>% 
  pivot_longer(cols = c(ratio_100m,cumsum_100m), 
               names_to = "key", 
               values_to = "value") %>% 
  mutate(key=fct_relevel(key,"ratio_100m"))

# __ create plot -------------------------------------------------------------

ggplot() +
  geom_rect(data = interventions,
            aes(xmin = date_min, xmax = date_max, 
                ymin = 0, ymax = Inf, 
                fill =intervention_label),
            alpha=0.3) +
  geom_vline(data = interventions %>% 
               filter(intervention=="seroprev"),
             aes(xintercept = date_min), lty=2) +
  geom_vline(data = interventions %>% 
               filter(intervention=="seroprev"),
             aes(xintercept = date_max), lty=2) +
  geom_line(data = peru_sources_age_long %>% 
              mutate(key=fct_recode(
                key,"Incident"="ratio_100m",
                "Cummulative"="cumsum_100m"
              )) %>% 
              mutate(source=case_when(
                source=="cases"~"COVID-19 Cases",
                source=="deaths"~"COVID-19 Deaths"
                )),
            aes(x = epi_date,y = value, color= edad),lwd=1.25) +
            # aes(x = epi_date,y = ratio_100m, color= edad),lwd=1.25) +
  colorspace::scale_color_discrete_qualitative() +
  scale_x_date(date_breaks = "1 month",date_labels = "%b") +
  facet_wrap(~source+key,scales = "free") +
  # scale_y_log10() +
  labs(title = "Government interventions and Surveillance data",
       subtitle = "Offitial Reports between March and October in Lima Metropolitan Area*, Peru 2020",
       caption = "* Province of Lima and Callao", 
       x = "Epidemiological week",
       y = "Number of events per 100K hab.",
       fill = "Interventions",
       color = "Age groups")

ggsave("figure/03-seroprev-figure03.png",dpi = "retina",height = 5,width = 9)
