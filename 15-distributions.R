#'
#' OBJETIVO:
#' ( ) muestreo: calcular n o % de cobertura a nivel vivienda con respecto a nro convivientes
#' (x) positividad: calcular % positivos por total de encuestados o convivientes por vivienda
#' 
#' #' PENDIENTES:
#' 
#' - si una unidad es el hogar o la vivienda,
#' - del total de unidades, ¿cuántas están en hacinamiento?
#' - y de estas, ¿cuántas conformadas por más de una persona?
#' 

library(tidyverse)
library(skimr)
library(writexl)
theme_set(theme_bw())

# functions ---------------------------------------------------------------

library(serosurvey)

set.seed(33)

# inputs ------------------------------------------------------------------

uu_clean_data <- read_rds("data/uu_clean_data.rds") %>% 
  mutate(survey_all="survey_all",
         weight_nul=1) %>% 
  # transformar a factor (prevlaencia ajustada)
  mutate_at(.vars = vars(igg,igm,ig_clasificacion,positividad_peru),
            .funs = as.factor)

# hacin
uu_clean_data %>% 
  ggplot(aes(x = ind_hacin)) +
  geom_histogram()

# nivel hogar
home_cases <- 
  uu_clean_data %>% #glimpse()
  select(cd_dist,conglomerado,PONDERACION,
         numero_vivienda,
         numero_hogar,
         nro_convivientes,
         nro_dormitorios,
         participante,
         n_registros_vv,
         ind_hacin,
         # ind_hacin_qrt,
         ind_hacin_cut2,
         hacinamiento,
         ig_clasificacion) %>% 
  # mutate(ig_clasificacion=as.numeric(ig_clasificacion)-1) %>% 
  # arrange(cd_dist,conglomerado,numero_vivienda) %>% 
  # count(ig_clasificacion)
  # print(n=100)
  group_by(across(cd_dist:hacinamiento)) %>% 
  count(ig_clasificacion) %>% 
  # summarise(prop_seropositive=mean(ig_clasificacion)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = -(ig_clasificacion:n), 
              names_from = ig_clasificacion, 
              values_from = n)

home_cases_ratio <- home_cases %>% 
  mutate(positivo=if_else(is.na(positivo),0L,positivo),
         participante=as.integer(participante)) %>% 
  rowwise() %>% 
  mutate(total_resultados=sum(c_across(negativo:positivo),na.rm = T)) %>% 
  mutate(ratio=positivo/total_resultados) %>% 
  ungroup()

home_cases_ratio %>% 
  naniar::miss_var_summary()

home_cases_ratio %>% 
  skim(ratio) %>% 
  as_tibble() %>% 
  select(skim_variable,
         numeric.p0:numeric.p100) %>% 
  rename_with(.cols = starts_with("numeric"),
              .fn = str_replace,"numeric.p","pct_")

plot_1 <- home_cases_ratio %>% 
  
  # alt manual
  select(x=ratio,w=PONDERACION) %>%
  mutate(ecdf= ecdf(x)(x) ) %>%
  mutate(ewcdf= spatstat::ewcdf(x = x,weights = w)(x) ) %>%
  ggplot(aes(x = x,y = ewcdf)) +
  # ggplot(aes(x = x,y = ecdf)) +
  geom_step(lwd=1) +
  ylim(0,1)
  
  # # alt ok
  # ggplot() +
  # stat_ecdf(aes(x = ratio),
  #           geom = "step",lwd=1)

# home_cases_ratio %>% 
#   filter(ratio>0) %>% 
#   select(total_resultados,ratio) %>% 
#   ggplot(aes(x = total_resultados,ratio)) +
#   geom_point(position = position_jitter(seed = 33,
#                                         height = 0.02,
#                                         width = 0.4),alpha=0.5)

home_cases_ratio %>% 
  select(#contains("ind_hac"),
         ind_hacin_cut2,
         hacinamiento,
         negativo,positivo,
         ratio) %>% 
  group_by(ind_hacin_cut2) %>%
  # group_by(hacinamiento) %>% 
  skim(ratio) %>% 
  as_tibble() %>% 
  select(skim_variable,ind_hacin_cut2,
         numeric.p0:numeric.p100) %>% 
  rename_with(.cols = starts_with("numeric"),
              .fn = str_replace,"numeric.p","pct_")

plot_2 <- home_cases_ratio %>% 
  mutate(hacinamiento=fct_recode(hacinamiento,
                                 "With"="Con Hacinamiento",
                                 "Without"="Sin Hacinmaniento")) %>% 
  
  # alt manual
  select(x=ratio,w=PONDERACION,strata=hacinamiento) %>%
  filter(!is.na(strata)) %>% 
  group_by(strata) %>% 
  mutate(ecdf= ecdf(x)(x) ) %>%
  mutate(ewcdf= spatstat::ewcdf(x = x,weights = w)(x) ) %>%
  ungroup() %>% 
  ggplot(aes(x = x,y = ewcdf,colour=strata)) +
  # ggplot(aes(x = x,y = ecdf,colour=strata)) +
  geom_step(lwd=1) +
  ylim(0,1) +
  
  # # alt ok
  # # filter(ratio>0) %>% 
  # ggplot(aes(x = ratio,
  #            # colour=hacinamiento
  #            colour=ind_hacin_cut2
  #            )) +
  # stat_ecdf(geom = "step",lwd=1,alpha=0.7) +
  colorspace::scale_color_discrete_qualitative() +
  theme(legend.background = element_rect(fill='transparent', 
                                         colour='transparent',
                                         # fill="white",
                                         # colour ="white",
                                         size=0, 
                                         linetype="solid"),
        legend.position = c(0.8, 0.2))

plot_3 <- home_cases_ratio %>% 
  
  # alt manual
  select(x=ratio,w=PONDERACION,strata=ind_hacin_cut2) %>%
  filter(!is.na(strata)) %>% 
  group_by(strata) %>% 
  mutate(ecdf= ecdf(x)(x) ) %>%
  mutate(ewcdf= spatstat::ewcdf(x = x,weights = w)(x) ) %>%
  ungroup() %>% 
  ggplot(aes(x = x,y = ewcdf,colour=strata)) +
  # ggplot(aes(x = x,y = ecdf,colour=strata)) +
  geom_step(lwd=1) +
  ylim(0,1) +
  
  # # alt ok
  # # filter(ratio>0) %>% 
  # ggplot(aes(x = ratio,
  #            # colour=hacinamiento
  #            colour=ind_hacin_cut2
  #            )) +
  # stat_ecdf(geom = "step",lwd=1,alpha=0.7) +
  colorspace::scale_color_discrete_qualitative() +
  theme(legend.background = element_rect(fill='transparent', 
                                         colour='transparent',
                                         # fill="white",
                                         # colour ="white",
                                         size=0, 
                                         linetype="solid"),
        legend.position = c(0.75, 0.35))

library(patchwork)
plot_1 + 
  coord_fixed() +
  labs(title = "Overall",
       x = "Seropositive Ratio",
       y="EWCDF") +
  plot_2 +
  coord_fixed() +
  labs(title = "By Overcrowding Cat.",
       x = "Seropositive Ratio",
       y="EWCDF",
       color = "") +
  plot_3 +
  coord_fixed() +
  labs(title = "By Overcrowding Index",
       x = "Seropositive Ratio",
       y="EWCDF",
       color = "") +
  plot_annotation(
    tag_levels = 'A',
    title = 'Distribution of the Seropositive Ratio at Home Level',
    caption = "EWCDF: Empirical Weighted Cumulative Distribution Function")
ggsave("figure/04-seroprev-figure05.png",height = 3.5,width = 9,dpi = "retina")


# # general
# ~70% de los hogares no tiene seropositivo
# # general
# 75% de los hogares tiene 
# 33% seropositivos o menos
# #
# 25% de los hogares tiene 
# 33% seropositivos o más
# # 
# # categorizdo
# 25% de los hogares 
# con un ind de hacinamiento de (1 - 1.4]
# tiene 
# 20% seropositivos o más
# 
# 
# # interpretación vertical
# # para ver cómo incrementa el percentil
# # y cómo cae el la razón de seropositividad
# 
