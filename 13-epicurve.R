library(covidPeru); library(readr)
library(covid19viz)
library(tidyverse)
library(cdcper)
theme_set(theme_bw())

# import surveillance data ------------------------------------------------

positivos <- da_positivos()
fallecidos <- da_fallecidos()
sinadef <- da_sinadef() %>% 
  rename(PROVINCIA=`PROVINCIA DOMICILIO`)

# import intervention data ------------------------------------------------

unesco <- read_unesco_education()
acaps <- read_acaps_governments()

# no data in ACAPS
acaps %>% filter(ISO3=="PER")

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

# count by epiweek --------------------------------------------------------

min_analysis_date <- ymd(20200301)
max_analysis_date <- ymd(20200801)

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
  summarize_epiweek(source = "Confirmed Cases") %>% 
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
      mutate(source = "Total Deaths")
  )

# unite intervention data -------------------------------------------------

interventions <- tibble(
  date_min = ymd(20200628), 
  date_max = ymd(20200701),
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
  geom_line(data = peru_sources,
            aes(x = epi_date,y = n, color= source)) +
  colorspace::scale_color_discrete_qualitative() +
  # scale_y_log10() +
  labs(title = "COVID-19 surveillance and Goverment interventions",
       subtitle = "Reports between March and August in Lima Metropolitan Area, Peru 2020",
       x = "Epidemiological weeks",
       y = "Number of events (log scale)",
       fill = "Interventions",
       color = "Surveillance")

ggsave("figure/03-seroprev-figure03.png",dpi = "retina",height = 4,width = 6.5)
