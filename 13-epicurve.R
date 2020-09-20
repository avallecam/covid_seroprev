library(covidPeru); library(readr)
library(covid19viz)
library(cdcper)
library(tidyverse)
library(lubridate)
theme_set(theme_bw())


# analysis time limits --------------------------------------------------------------

min_analysis_date <- ymd(20200301)
max_analysis_date <- ymd(20200801)

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
  )
interventions

interventions %>% 
  writexl::write_xlsx("table/02-seroprev-supp-table05.xlsx")

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
  geom_line(aes(x = date,percent_change_from_baseline, color=subregion)) +
  geom_smooth(aes(x = date,percent_change_from_baseline, color=subregion),
              span = 0.1) +
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
ggsave("figure/03-seroprev-figure04.png",dpi = "retina",height = 3.5,width = 7)

# covidPeru R package -----------------------------------------------------


# _import surveillance data ------------------------------------------------

positivos <- da_positivos()
fallecidos <- da_fallecidos()
sinadef <- da_sinadef() %>% 
  rename(PROVINCIA=`PROVINCIA DOMICILIO`)

# _count by epiweek + unite --------------------------------------------------------

summarize_epiweek <- function(data,source) {
  data %>% 
    filter(PROVINCIA=="LIMA"|PROVINCIA=="CALLAO") %>% 
    filter(fecha > min_analysis_date) %>% 
    filter(fecha < max_analysis_date) %>% 
    cdcper::cdc_yearweek_to_date(year_integer = year,week_integer = semana) %>% 
    count(epi_date) %>% 
    mutate(source={{source}})
}

peru_sources <- positivos %>% 
  summarize_epiweek(source = "COVID-19 Confirmed Cases") %>% 
  union_all(
    # fallecidos %>% 
    #   summarize_epiweek(source = "Confirmed Deaths")
    fallecidos %>%
      summarize_epiweek(source = "Deaths") %>%
      rename(Deaths=n) %>%
      select(-source) %>%
      full_join(
          sinadef %>%
            summarize_epiweek(source = "Sinadef") %>%
            rename(Sinadef=n) %>%
            select(-source)
      ) %>%
      rowwise(epi_date) %>%
      mutate(n = sum(c_across(Deaths:Sinadef),na.rm = T)) %>%
      ungroup() %>%
      select(-Deaths,-Sinadef) %>%
      mutate(source = "All causes of Deaths\n(including COVID-19 confirmed)")
  )

peru_sources %>% 
  arrange(desc(source),epi_date) %>% 
  group_by(source) %>% 
  mutate(cumsum=cumsum(n)) %>% 
  # from 11-sampling_comparison
  mutate(cumpct=cumsum/10742559*100) %>% 
  mutate(epi_week=epiweek(epi_date)) %>% 
  ungroup() %>% 
  select(source,epi_date,epi_week,everything()) %>% 
  # avallecam::print_inf()
  writexl::write_xlsx("table/02-seroprev-supp-table06.xlsx")
  

# create plot -------------------------------------------------------------

ggplot() +
  geom_rect(data = interventions,
            aes(xmin = date_min, xmax = date_max, 
                ymin = 0, ymax = Inf, 
                fill =intervention_label),
            alpha=0.4) +
  geom_vline(data = interventions %>% 
               filter(intervention=="seroprev"),
             aes(xintercept = date_min), lty=2) +
  geom_vline(data = interventions %>% 
               filter(intervention=="seroprev"),
             aes(xintercept = date_max), lty=2) +
  geom_line(data = peru_sources %>% 
              mutate(source=fct_relevel(source,"COVID-19 Confirmed Cases")),
            aes(x = epi_date,y = n, color= source)) +
  colorspace::scale_color_discrete_qualitative() +
  # scale_y_log10() +
  labs(title = "Government interventions and Surveillance data",
       subtitle = "Reports between March and August in Lima Metropolitan Area, Peru 2020",
       x = "Epidemiological week",
       y = "Number of events",
       fill = "Interventions",
       color = "Surveillance")

ggsave("figure/03-seroprev-figure03.png",dpi = "retina",height = 3.5,width = 6)
