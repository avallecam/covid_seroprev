library(tidyverse)

theme_set(theme_bw())

source("10-prevalence_functions.R")

# read --------------------------------------------------------------------

outcome_01_adj_tbl <- readr::read_rds("data/outcome_01_adj_tbl.rds")

# OUTPUTS -----------------------------------------------------------------

#' table with raw, population adjusted, test performance adjusted
#' figure 1 (overall, sex, age) showing the four outcomes!
#' figure 2 (age decenio) showing trend
#' figure 3 map per diris
#' 
#' more
#' performance of surveillance due to underreporting


# tables ------------------------------------------------------------------

outcome_01_adj_tbl %>% 
  writexl::write_xlsx("table/00-seroprev-results.xlsx")

outcome_01_adj_tbl %>% 
  select(1:4,starts_with("unite1_"),raw_num,raw_den,prop_cv) %>% 
  filter(numerator=="ig_clasificacion") %>% 
  writexl::write_xlsx("table/01-seroprev-table01.xlsx")

outcome_01_adj_tbl %>% 
  select(1:4,starts_with("unite1_"),raw_num,raw_den,prop_cv) %>% 
  filter(denominator=="ig_clasificacion") %>% 
  writexl::write_xlsx("table/01-seroprev-table02.xlsx")

outcome_01_adj_tbl %>% 
  select(1:4,starts_with("unite1_"),raw_num,raw_den,prop_cv) %>% 
  filter(numerator=="positividad_peru") %>% 
  writexl::write_xlsx("table/02-seroprev-supp-table03.xlsx")

outcome_01_adj_tbl %>% 
  select(1:4,starts_with("unite1_"),raw_num,raw_den,prop_cv) %>% 
  filter(denominator=="positividad_peru") %>% 
  writexl::write_xlsx("table/02-seroprev-supp-table04.xlsx")


# figure ------------------------------------------------------------------

# __supp-fig01: PR + PM -----------------------------------------------------------------

# outcome_01_adj_tbl %>% count(denominator)

figura01 <- outcome_01_adj_tbl %>% 
  filter(!magrittr::is_in(denominator,c("diris","edad_decenios","nm_prov"))) %>%
  filter(!magrittr::is_in(denominator,c("sintomas_cualquier_momento_cat_fecha_rangos"))) %>%
  filter(numerator=="ig_clasificacion"|numerator=="positividad_peru") %>% 
  mutate(denominator=as.factor(denominator),
         denominator_level=as.factor(denominator_level)) %>% 
  mutate(denominator=fct_relevel(denominator,"survey_all",
                                 "sexo",
                                 "edad_etapas_de_vida_t")) %>%
  # grid title
  mutate(denominator=fct_recode(denominator,
                                # "Pob. General"="survey_all",
                                # "Sexo Biológico"="sexo",
                                # "Etapas de Vida"="edad_etapas_de_vida_t",
                                # "Hacinamiento"="hacinamiento",
                                # "Pobreza"="pobreza_dico"
                                # "Síntomas"="sintomas_cualquier_momento_cat",
                                # "Contacto"="contacto_covid"
                                "General Pop."="survey_all",
                                "Biological Sex"="sexo",
                                "Age (years)"="edad_etapas_de_vida_t",
                                "Overcrowding"="hacinamiento",
                                "Symptoms"="sintomas_cualquier_momento_cat",
                                "Contact"="contacto_covid",
                                "Contact Type"="contacto_covid_tipo",
                                "Previous Test"="prueba_previa",
                                "Symptoms (by onset)"="sintomas_cualquier_momento_cat_fecha_14d_v1",
                                # "Symptoms (by onset)."="sintomas_cualquier_momento_cat_fecha_rangos"
  )
  ) %>%
  mutate(denominator_level=fct_relevel(denominator_level,
                                       "ninho","adolescente","joven","adulto",
                                       "sinto_asint","sinto_oligo",
                                       "sinto_covid_onset_u14d_si",
                                       "no","si_contacto_tipo1",
                                       "si_contacto_tipo3",
                                       "si_contacto_tipo2",
                                       "si_contacto_tipo45",
                                       "desconocido"
                                       # "sinto_covid"
  )
  ) %>%
  mutate(denominator_level=fct_rev(denominator_level)) %>%
  mutate(denominator_level=fct_recode(denominator_level,
                                      # "Prueba"="survey_all",
                                      # "Niño"="ninho",
                                      # "Adolescente"="adolescente",
                                      # "Joven"="joven",
                                      # "Adulto"="adulto",
                                      # "Adulto Mayor"="adulto_mayor",
                                      # "Femenino"="femenino",
                                      # "Masculino"="masculino",
                                      # "No"="no",
                                      # "Sí"="si",
                                      # "Desconoce"="desconocido",
                                      # "Asintomático"="sinto_asint",
                                      # "Oligosintomático"="sinto_oligo",
                                      # "Sintomático"="sinto_covid"
                                      
                                      "All"="survey_all",
                                      "0-11"="ninho",
                                      "12-17"="adolescente",
                                      "18-29"="joven",
                                      "30-59"="adulto",
                                      "60+"="adulto_mayor",
                                      "Female"="femenino",
                                      "Male"="masculino",
                                      "No"="no",
                                      "Yes"="si",
                                      "Unknown"="desconocido",
                                      "Asymptomatic"="sinto_asint",
                                      "Oligosymptomatic"="sinto_oligo",
                                      "Symptomatic"="sinto_covid",
                                      # "Asymptomatic"="sinto_asint",
                                      # "Oligosymptomatic"="sinto_oligo",
                                      "Symptomatic\n(<=14 days before\nstudy visit)"="sinto_covid_onset_u14d_no",
                                      "Symptomatic\n(>14 days before\nstudy visit)"="sinto_covid_onset_u14d_si",
                                      # "Un miembro de su hogar" = "si_contacto_tipo1",
                                      # "Otro miembro de la familia" = "si_contacto_tipo2",
                                      # "Compañero del lugar donde trabaja" = "si_contacto_tipo3",
                                      # "Otro" = "si_contacto_tipo45",
                                      "Household member" = "si_contacto_tipo1",
                                      "Other family member" = "si_contacto_tipo2",
                                      "Workmate" = "si_contacto_tipo3",
                                      "Other" = "si_contacto_tipo45",
                                      "With"="Con Hacinamiento",
                                      "Without"="Sin Hacinmaniento" #------------#
  )) %>%
  mutate(numerator=fct_recode(numerator,
                              # "IgM+"="igm",
                              # "IgG+"="igg",
                              "IgM+ or IgG+"="ig_clasificacion",
                              "IgM+ or IgG+ or PCR+"="positividad_peru"#,
  ))

figura01 %>% 
  ggplot_prevalence(category = denominator_level,
                    proportion = prop,
                    outcome = numerator,
                    proportion_upp = prop_upp,
                    proportion_low = prop_low) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0, hjust=0)) +
  coord_flip() +
  facet_wrap(denominator~.,scales = "free_y") +
  # facet_grid(denominator~.,scales = "free_y") +
  colorspace::scale_color_discrete_qualitative() +
  labs(title = "SARS-CoV-2 Prevalence by case definitions across covariates",
       subtitle = "Lima Metropolitan Area, Peru: June 28th-July 9th, 2020",
       y = "Prevalence",x = "",
       color = "Case\ndefinition"#,size = "CV%"
  )
ggsave("figure/00-seroprev-supp-figure01.png",height = 7,width = 12,dpi = "retina")

# __fig01: edad decenio -----------------------------------------------------------------


figura02 <- outcome_01_adj_tbl %>% 
  filter(magrittr::is_in(denominator,c("edad_decenios"))) %>% 
  filter(numerator=="ig_clasificacion") %>% 
  select(1:4,
         "raw_dot"=raw_prop,"raw_low"=raw_prop_low,"raw_upp"=raw_prop_upp,
         "prop_dot"=prop,prop_low,prop_upp,
         "adj_dot"=adj_dot_unk_p50,
         "adj_low"=adj_low_unk_p50,
         "adj_upp"=adj_upp_unk_p50) %>% 
  pivot_longer(
    cols = -denominator:-numerator_level,
    names_to = "estimate",
    values_to = "value"
  ) %>% 
  separate(estimate,into = c("source","type")) %>% 
  pivot_wider(
    names_from = type,
    values_from = value
  ) %>% 
  mutate(source=fct_relevel(source,"raw","prop","adj")) %>% 
  # avallecam::print_inf() 
  mutate(source=fct_recode(source,
                           # "IgM+"="igm","IgG+"="igg",
                           "Raw"="raw",
                           "Sampling weights only"="prop",
                           "Sampling weights and\nTest uncertainty"="adj",
  ))

figura02 %>% 
  ggplot_prevalence(category = denominator_level,
                    outcome = source,
                    proportion = dot,
                    proportion_upp = upp,
                    proportion_low = low) +
  colorspace::scale_color_discrete_qualitative(rev = TRUE) +
  labs(title = "SARS-CoV-2 Seroprevalence Stratified by Age",
       subtitle = "Lima Metropolitan Area, Peru: June 28th-July 9th, 2020",
       y = "Prevalence",x = "Age (years)",
       color = "Estimate"#,size = "CV%"
  )
ggsave("figure/01-seroprev-figure01.png",height = 4,width = 6.5,dpi = "retina")

# out0105 %>% 
#   tidy_srvyr_tibble() %>% 
#   ggplot_prevalence()



# __fig02: espacial diris -----------------------------------------------------------------

library(sf)
library(ggspatial)
shapes_diris <- read_rds("data/per4-shp_distritos_janitor_diris.rds") %>% 
  st_as_sf(crs = 4610, agr = "constant") %>% 
  count(diris) 

figure_03_map <- outcome_01_adj_tbl %>% 
  filter(magrittr::is_in(denominator,c("diris"))) %>% 
  filter(numerator=="ig_clasificacion") %>% 
  # TRANSLATE OR change to ONLY NUMBERS UNITE2
  mutate(category=denominator_level) %>% 
  # # SPANISH
  # mutate(category=if_else(category=="CALLAO",category,
  #                         str_replace(category,"DIRIS (.+)","LIMA \\1"))) %>% 
  # # ENGLISH
  mutate(category=if_else(category=="CALLAO",
                          category,
                          str_replace(category,"DIRIS (.+)","\\1 LIMA"))) %>% 
  mutate(category=case_when(
    str_detect(category,"CENTRO") ~ str_replace(category,"CENTRO","CENTRAL"),
    str_detect(category,"NORTE") ~ str_replace(category,"NORTE","NORTHERN"),
    str_detect(category,"SUR") ~ str_replace(category,"SUR","SOUTHERN"),
    str_detect(category,"ESTE") ~ str_replace(category,"ESTE","EASTERN"),
    TRUE ~ category
  )) %>% 
  mutate(prevalence_map=str_c(category,"\n",unite2_adj_dot_unk_p50)) %>% 
  left_join(shapes_diris %>% select(-n,denominator_level=diris)) %>% 
  mutate(proportion=adj_dot_unk_p50)
# mutate(outcome=fct_recode(outcome,"IgM+"="igm","IgG+"="igg",
#                           "IgM+ o IgG+"="ig_clasificacion",
#                           "IgM+ o IgG+ o PCR+"="positividad_peru",
# )) %>% 

figure_03_map %>% 
  select(denominator_level,
         category,
         geometry,
         adj_dot_unk_p50,proportion)

figure_03_map %>%
  st_as_sf(crs = 4610, agr = "constant") %>% 
  ggplot() +
  geom_sf(aes(fill=proportion),colour = NA) +
  coord_sf() +
  colorspace::scale_fill_continuous_sequential("Reds 3",
                                               limits = c(0.14,0.30),
                                               # rev = F,
                                               labels=scales::percent_format(accuracy = 1)) +
  # colorspace::scale_color_continuous_sequential(palette = "Grays",
  #                                               rev = F,
  #                                               guide = F) +
  # ggsflabel::geom_sf_label_repel(aes(label=prevalence_map,
  #                                    fill=proportion,
  #                                    color=proportion),
  #                                size=3,
  #                                fontface = "bold") #+
  ggsflabel::geom_sf_label(data = figure_03_map %>% 
                             filter(category=="CALLAO"),
                           aes(label=prevalence_map,
                               fill=proportion),
                           color="white",
                           size=3,
                           fontface = "bold") +
  ggsflabel::geom_sf_label(data = figure_03_map %>% 
                             filter(!(category=="CALLAO")),
                           aes(label=prevalence_map,
                               # color=proportion,
                               fill=proportion),
                           color="black",
                           size=3,
                           fontface = "bold") +
  # scale_x_continuous(breaks = scales::pretty_breaks(n = 3)) +
  labs(title = "SARS-CoV-2 Seroprevalence Stratified in Space",
       subtitle = "Lima Metropolitan Area, Peru: June 28th-July 9th, 2020",
       y = "Latitude",
       x = "Longitud",
       fill = "Prevalence"#,size = "CV%"
  ) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", 
                         which_north = "true",
                         pad_x = unit(0.5, "in"),
                         pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
ggsave("figure/02-seroprev-figure02.png",height = 8,width = 9,dpi = "retina")


# __________ --------------------------------------------------------------
