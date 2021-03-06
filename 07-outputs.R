#' 
#' OBJECTIVES:
#' - table with raw, population adjusted, test performance adjusted
#' - figure 1 (overall, sex, age) showing the four outcomes!
#' - figure 2 (age decenio) showing trend
#' - figure 3 map per diris
#' 

library(tidyverse)

theme_set(theme_classic())

# source("10-prevalence_functions.R")

library(serosurvey)

# read --------------------------------------------------------------------

outcome_01_adj_tbl <- readr::read_rds("data/outcome_01_adj_tbl.rds")

# OUTPUTS -----------------------------------------------------------------

#' table with raw, population adjusted, test performance adjusted
#' figure 1 (overall, sex, age) showing the four outcomes!
#' figure 2 (age decenio) showing trend
#' figure 3 map per diris


# tables ------------------------------------------------------------------

outcome_01_adj_tbl %>% 
  writexl::write_xlsx("table/00-seroprev-results.xlsx")

outcome_01_adj_tbl %>% 
  select(1:4,starts_with("unite1_"),raw_num,raw_den,prop_cv) %>% 
  filter(numerator=="ig_clasificacion") %>% 
  # writexl::write_xlsx("table/05-seroprev-tablexx.xlsx")
  writexl::write_xlsx("table/01-seroprev-table01.xlsx")

outcome_01_adj_tbl %>% 
  select(1:4,starts_with("unite1_"),raw_num,raw_den,prop_cv) %>% 
  filter(denominator=="ig_clasificacion") %>% 
  writexl::write_xlsx("table/01-seroprev-table02.xlsx")

outcome_01_adj_tbl %>% 
  select(1:4,starts_with("unite1_"),raw_num,raw_den,prop_cv) %>% 
  filter(numerator=="positividad_peru") %>% 
  # writexl::write_xlsx("table/05-seroprev-tablexx.xlsx")
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
  # filter(denominator!="prueba_previa") %>% 
  filter(denominator!="prueba_previa_cat") %>% 
  # filter(denominator!="prueba_previa_res") %>% 
  filter(denominator!="ind_hacin_cat") %>% 
  filter(denominator!="nse_estrato_cat") %>% 
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
                                # "ind_hacin_cat"="ind_hacin_cat",
                                "Overcrowding index (quartiles)"="ind_hacin_cut2",
                                "Socioeconomic Status"="nse_estrato",
                                # "Socioeconomic Status (cat.)"="nse_estrato_cat",
                                "Symptoms"="sintomas_cualquier_momento_cat",
                                "Contact"="contacto_covid",
                                "Contact Type"="contacto_covid_tipo",
                                "Previous Test"="prueba_previa",
                                # "Previous Test (Type)"="prueba_previa_cat",
                                "Previous Test (Result)"="prueba_previa_res",
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
                                       "desconocido",
                                       "Sin Hacinmaniento"
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
                                      "High"="1",
                                      "Middle-High"="2",
                                      "Middle"="3",
                                      "Middle-Low"="4",
                                      "Low"="5",
                                      # ".Low"="1_low",
                                      # ".Middle"="3_middle",
                                      # ".High"="5_high",
                                      "Unknown"="desconocido",
                                      "No"="no_",
                                      # "with PCR Test"="si_pcr",
                                      # "with Antibody\nRapid Test"="si_pr",
                                      "Negative\nresult"="si_negativo",
                                      "Positive\nresult"="si_positivo",
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
                                      "Without"="Sin Hacinmaniento",
                                      "With"="Con Hacinamiento" #------------#
  )) %>%
  mutate(numerator=fct_recode(numerator,
                              # "IgM+"="igm",
                              # "IgG+"="igg",
                              "IgM+ or IgG+"="ig_clasificacion",
                              "IgM+ or IgG+ or PCR+"="positividad_peru"#,
  ))

figura01 %>% 
  # ggplot_prevalence(denominator_level = denominator_level,
  #                   numerator = numerator,
  #                   proportion = prop,
  #                   proportion_upp = prop_upp,
  #                   proportion_low = prop_low) +
  ggplot(aes(x = denominator_level, 
             y = prop, 
             color = numerator, 
             group = numerator)) + 
  geom_point(position = position_dodge(width = 0.5)) + 
  geom_errorbar(aes(max = prop_upp, 
                    min = prop_low,
                    width = 0.5), 
                position = position_dodge(width = 0.5)) +
  # theme(axis.text.x = element_text(angle = 0, vjust = 0, hjust=0)) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = scales::pretty_breaks(n = 5)) +
  coord_flip() +
  facet_wrap(denominator~.,scales = "free_y",ncol = 3) +
  # facet_grid(denominator~.,scales = "free_y") +
  colorspace::scale_color_discrete_qualitative() +
  labs(#title = "SARS-CoV-2 Prevalence by case definitions across covariates",
       #subtitle = "Lima Metropolitan Area, Peru: June 28th-July 9th, 2020",
       # caption = "* SES: Socioeconomic Status",
       y = "Prevalence [95% Confidence Intervals]",x = "",
       color = "Case\ndefinition"#,size = "CV%"
  ) +
  theme_bw()
ggsave("figure/00-seroprev-supp-figure01.png",height = 8,width = 12,dpi = "retina")
ggsave("figure/00-seroprev-supp-figure01-300dpi.tiff",height = 8,width = 12,dpi = "retina")
ggsave("figure/00-seroprev-supp-figure01-500dpi.tiff",height = 8,width = 12,dpi = 500)
ggsave("figure/00-seroprev-supp-figure01-1000dpi.tiff",height = 8,width = 12,dpi = 1000)

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
  ggplot_prevalence(denominator_level = denominator_level,
                    numerator = source,
                    proportion = dot,
                    proportion_upp = upp,
                    proportion_low = low) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = scales::pretty_breaks(n = 5)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  colorspace::scale_color_discrete_qualitative(rev = TRUE) +
  labs(title = "SARS-CoV-2 Seroprevalence Stratified by Age",
       subtitle = "Lima Metropolitan Area, Peru: June 28th-July 9th, 2020",
       y = "Prevalence",x = "Age (years)",
       color = "Estimate"#,size = "CV%"
  )
ggsave("figure/01-seroprev-figure01.png",height = 3.5,width = 6,dpi = "retina")

# out0105 %>% 
#   tidy_srvyr_tibble() %>% 
#   ggplot_prevalence()


# __fig01: edad etapas [alternativa] -----------------------------------------------------------------


figura02 <- outcome_01_adj_tbl %>% 
  filter(magrittr::is_in(denominator,c("edad_etapas_de_vida_t"))) %>% 
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
  )) %>% 
  mutate(denominator_level=fct_relevel(denominator_level,
                                       "ninho","adolescente",
                                       "joven","adulto")) %>% 
  mutate(denominator_level=fct_recode(denominator_level,
                                      "0-11"="ninho",
                                      "12-17"="adolescente",
                                      "18-29"="joven",
                                      "30-59"="adulto",
                                      "60+"="adulto_mayor"))

figura02_a <- figura02 %>% 
  ggplot_prevalence(denominator_level = denominator_level,
                    numerator = source,
                    proportion = dot,
                    proportion_upp = upp,
                    proportion_low = low) +
  # scale_y_continuous(
  #   # labels = scales::percent_format(accuracy = 1),
  #   breaks = scales::pretty_breaks(n = 5),limits = ) +
  colorspace::scale_color_discrete_qualitative(rev = TRUE)
  #theme(axis.text.x = element_text(angle = NULL, vjust = NULL, hjust=NULL))


# ____ alt 1 --------------------------------------------------------------



library(cowplot)

inset <- figura02_a +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1,suffix = ""),
                     breaks = scales::pretty_breaks(n = 5),
                     # limits = c(0,1)
                     ) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text=element_text(size=7),
        # panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA)
        ) +
  labs(x="",y="")

main <- figura02_a +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1,suffix = ""),
                     breaks = scales::pretty_breaks(n = 10),
                     limits = c(0,1)) +
  labs(#title = "SARS-CoV-2 Seroprevalence Stratified by Age",
       #subtitle = "Lima Metropolitan Area, Peru: June 28th-July 9th, 2020",
       y = "Prevalence (%)",x = "Age (years)",
       color = "Estimate"#,size = "CV%"
  )

done <- ggdraw() +
  draw_plot(main) +
  draw_plot(inset, 
            x = 0.13, y = .44, 
            width = .48, height = .55)

png(filename = "figure/05-seroprev-figure01.png",
    units="in", width=5, height=3.5, res=300
    #height = 500,width = 550,res = 300
    )
plot(done)
dev.off()

# save_plot(filename = "figure/05-seroprev-figure01.png",
#           plot = done,
#           base_height = 3.5,base_width = 4#,
#           # dpi = "retina"
#           )



# ____ alt 2 --------------------------------------------------------------

figura02_a +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1,suffix = ""),
                     breaks = scales::pretty_breaks(n = 10),
                     limits = c(0,0.35)) +
  labs(#title = "SARS-CoV-2 Seroprevalence Stratified by Age",
    #subtitle = "Lima Metropolitan Area, Peru: June 28th-July 9th, 2020",
    y = "Prevalence (%)",x = "Age (years)",
    color = "Estimate"#,size = "CV%"
  )
ggsave("figure/05-seroprev-figure01-2.png",height = 3.5,width = 4.5,dpi = "retina")

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
  
  # # SPANISH
  # labs(title = "Seroprevalencia de SARS-CoV-2",
  #      subtitle = "Lima Metropolitana, Perú: Junio 28 - Julio 9, 2020",
  #      y = "Latitud",
  #      x = "Longitud",
  #      fill = "Prevalencia"#,size = "CV%"
  # ) +
  
  # ENGLISH
  labs(#title = "SARS-CoV-2 Seroprevalence Stratified in Space",
       #subtitle = "Lima Metropolitan Area, Peru: June 28th-July 9th, 2020",
       y = "Latitude",
       x = "Longitude",
       fill = "Prevalence"#,size = "CV%"
  ) +
  
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", 
                         which_north = "true",
                         pad_x = unit(0.5, "in"),
                         pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme_bw()
ggsave("figure/02-seroprev-figure02.png",height = 8,width = 9,dpi = "retina")


# __________ --------------------------------------------------------------
