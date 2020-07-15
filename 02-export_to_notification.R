#' 
#' OBJETIVOS
#' - crear formatos de envío de datos a SISCOVID y NETLAB
#' 

#' 
#' TAREA:
#' (X) REMOVER TODAS LAS OBSERVACIONES CON ROWNAME YA ENVIADO!!!
#' 
#' 
#' OJO 
#' filtrar solo isopado para INS

#' renipress
#' Lab lima centro: 00006204	LABORATORIO DE REFERENCIA DE SALUD PUBLICA
#' Lima sur: 00005988	CENTRO DE SALUD ALICIA LASTRES DE LA TORRE
#' Lima norte: 00005791	CENTRO MATERNO INFANTIL TAHUANTINSUYO BAJO

#' 
#' acuerdos
#' enviar todos los hisopados
#' ver una prueba
#' 

#' punto 1: elegir a cuántas rutas
#' - opcion 1: siscovid (ins colecta de siscovid)
#' - opción 2: ins + siscovid por separado
#' 
#' punto 3: cómo se va a identificar a los sujetos en el sistema?
#' - propuesta: en variable CONGLOMERADO
#' 
#' punto 2: qué variables disponibles hay para enviar?
#' - propuesta: lista de chequeo
#' - variables listas
#' - variables que requeriran trabajo
#' - *usar metadata de conglomerados*

# NETLAB
# (-) PERSONA:
# (-)   Tipo de documento
# (x)   Número de documento
# (x)   Dirección actual (ubigeo)
# (x)   Número de celular 
# (-) EESS:
# (O)   Origen de EESS: utilizar el más cercano o INS ROM
# (-)   Destino de EESS: Lab. Virus Respiratorios. 
# ( ) PRUEBA:
# (x)   Tipo de muestra
# (x)   Fecha de obtención
# ( )   DNI de la persona que realiza la prueba rápida
# ( )   DNI de la persona que solicita el examen (PM)
# ( )   EESS donde labora. 

#' 1000 muestras ingresadas al rom
#' no se pueden diferenciar aún
#' en netlab motivo: estudio prevalencia covid
#' cuando lelga  arom
#' registran primera información
#' para diferenciar muestras del estudio
#' llegan con carta de estudio de comite de etica con copia de carta
#' tubo con codigo normal (sí tomado por rom) + codigo especifico para el estudio (cdc)
#' ¿es suficiente con dni?
#' razones para diferencias
#' 1. solo extraer resultados del estudio
#' 2. si no identificamos, sumara a conteo diario y no puede pasar eso. deben diferenciarse
#' punto 2 sí se realiza
#' lista de muestras a rom - no util para identificarlas - 4000 muestras
#' etiqueta resulta se r la solucion para sean facil de identificarlos
#' ambos soluciones son validas
#' riesgo de digitación por 
#' consenso: ambas soluciones quedan: lsitado de muestras y campo de registro inicial
#' como llegan muestras al rom?
#' enviar cajas con coollers con las fichas de cada paciente con hisopado
#' codigo de tubo, dni, procedencia y supervisor
#' en el rom están registrando esa información
#' si ingresa mediante una ficha al rom, ya está en el netlab
#' está llegando una relación
#' no hay sustento para procesar las muestras
#' enviar fichas 
#' llegan con copia de comite de etica
#' 1. listado de muestra que vincula persona a tubo
#' 2. no se creo ficha epi, sino ficha de estudio, remitirla electronicamente diariamente
#' 3. verificar dni, reniec y se envia excel
#' 4. regularizar los excels, con sustento de ejecucion del mismo
#' 5. retrazo del excel, ¿cómo llegan a ins? porque diris quieren hacer seguimiento clinico
#' lista visada, enlace concreto de la, el excel va a saltar, 
#' todo proceso de analisis está vinculado con un pre-analisis
#' solucion contrafactual: enviar copia de fichas para retroalimentacion 
#' ¿cómo se da el proceso de regularización?
#' - llegan con hojas escritas en mano visada
#' - 1. regularizar ficha de arriba
#' - 2. data llenada en netlab
#' en ficha investigacoin está contenido relevante de f100 y f200
#' algunos terminos no relevantes se han excluido
#' señirnos a objetivos de estudio, salvando 
#' leonardo:
#' prueba rapida
#' - coordinado con ogti para remitir base
#' prueba molecular
#' - complejidad, para que muestra se admita en rom, necesitamos información mínima (lista arriba)
#' - actualmente, solo se cumple ingreso administrativo
#' - * pendiente, interoperatibidad con trama establecida


# SISCOVID
# {
# (x)   "fecha_envio": "20200422004136",
# (x)   "ipress":"00006000",
# (O)   "id_envio": "20001",
# ( )   "investigacion": {
# (x)     "direccion": "Av sta rosa 375 tablada",
# (x)     "fec_inicio_sintomas": "",
# (x)     "latitud": "",
# (x)     "longitud": "",
# (O)     "tipo_vivienda": "1",
# (x)     "ubigeo": "150101"
#   },
# (x)   "otro_riesgo": "ninguno",
# (x)   "otro_sintoma": "ninguno",
# ( )   "paciente": {
# (x)     "ape_materno": "ARIAS",
# (x)     "ape_paterno": "CHAVEZ",
# (x)     "celular": "951105730",
# ( )     "correo": "",
# (x)     "fec_nacimiento": "20200419",
# ( )     "id_estadoo": "3",
# ( )     "id_profesion": "8",
# (x)     "nombre": "DEIVICK ALFONSO",
# (x)     "num_documento": "41663398",
# ( )     "personal_salud": "NO",
# ( )     "prioridad": "1",
# (x)     "sexo": "1",
# (x)     "telefono": "951105730",
# (x)     "tip_documento": "1"
#   },
# ( )   "prueba": {
# (x)     "fec_prueba": "20200419",
# (x)     "hor_prueba": "114120",
# ( )     "id_clasificacion": "1",
# ( )     "id_procedencia": "1", --------> estudio de seroprevalencia
# ( )     "id_tipo_resultado1": "2",
# ( )     "id_tipo_resultado2": "2",
# ( )     "nro_visita": "1",
# ( )     "observacion": "",
# ( )     "pcd": "2"
#   },
# ( )   "riesgos_array": [{
# (x)     "riesgo": "1"
#   }
#   ],
# (x)   "sintomas_array": [{
# (x)     "sintoma": "1"
#   }
#   ],
# ( )   "usuario": "usuario"
# }


# packages ----------------------------------------------------------------

library(tidyverse)
library(avallecam)
library(lubridate)
library(magrittr)

uu_raw_data <- read_rds("data/uu_raw_data.rds") %>% 
  janitor::clean_names()

# to SISCOVID -------------------------------------------------------------

uu_raw_data %>% glimpse()

uu_raw_data %>% 
  
  #restringir a solo resultados PR en el presente
  filter(presente_prueba=="si",!is.na(resultado_pr),diris!="DIRIS ESTE") %>% 
  
  # count(diris)
  
  #por defecto para siscovid
  mutate(id_estado=3,
         prioridad=1,
         usuario="cdcperu",
         id_procedencia="estudio seroprevalencia",
         nro_visita=1) %>% 
  
  #categorizar profesion para siscovid
  mutate(id_profesion=case_when(
    prof_salud=="medico"~1,
    prof_salud=="enfermero"~2,
    prof_salud=="obstetra"~3,
    str_detect(ocupacion,"biolog")~4,
    str_detect(ocupacion,"tec") & str_detect(ocupacion,"medic")~5,
    prof_salud=="tecnico_enfermeria"~6,
    str_detect(ocupacion,"tec") & str_detect(ocupacion,"lab")~7,
    TRUE~8
  )) %>%
  mutate(personal_salud=if_else(rubro=="salud" & prof_salud!="otros","si","no")) %>% 
  # categorizar docu
  mutate(tip_documento=case_when(
    tipo_doc=="dni"~1,
    tipo_doc=="carnet_extra"~2,
    tipo_doc=="pasaporte"~3,
    str_detect(otro_dni,"refugio|REFUGIO")~5,
    tipo_doc=="otro"|str_detect(otro_dni,"Cédula|cédula|cedula")~4,
    str_detect(tipo_doc,"ninguno|niguno")|is.na(tipo_doc)~6,
  )) %>% 
  # # extraer hora
  mutate(fecha_envio=Sys.time() %>% 
           janitor::make_clean_names() %>% 
           str_replace_all("_","") %>% 
           str_replace("x","")) %>% 
  # mutate(hor_prueba=str_replace(submission_time,"(...........)(.+)","\\2"),
  #        hor_prueba=str_replace_all(hor_prueba,":","")) %>% 
  # categorizar resultados
  mutate(id_tipo_resultado1=case_when(
    resultado_pr=="igg"~5,
    resultado_pr=="igm"~4,
    resultado_pr=="igm_igg"~6,
    resultado_pr=="indeterminado"~3,
    resultado_pr=="negativo"~2,
  ),
         id_tipo_resultado2=case_when(
           resultado_pr2=="igg"~5,
           resultado_pr2=="igm"~4,
           resultado_pr2=="igm_igg"~6,
           resultado_pr2=="indeterminado"~3,
           resultado_pr2=="negativo"~2,
         )) %>% 
  # select(contains("fecha")) %>% 
  mutate_at(.vars = vars(contains("fecha")),.funs = str_replace_all,"-","") %>% 
  mutate(pcd=if_else(is.na(resultado_pcr_previa),"no","si")) %>% 
  
  #seleccion
  select(fecha_envio,
         ipress,
         id_envio=rowname,
         # * pendiente: id de envio correlativo
         otro_riesgo=condicion_riesgo_otro,
         otro_sintoma=sintomas_otros_respiratorios,
         direccion,
         fec_inicio_sintomas=fecha_inicio_sintomas, #cambiar formato
         latitud=gps_latitude,
         longitud=gps_longitude, #más confiable
         # no disponible: tipo_vivienda
         ubigeo=cd_dist,
         ape_materno=apellido_paterno,
         ape_paterno=apellido_materno,
         celular=telefono, # celular
         # no disponible: correo
         fecha_nacimiento,
         id_estado,
         id_profesion,
         nombre=nombres,
         num_documento=dni,
         personal_salud,
         prioridad,
         sexo,
         telefono=otro_telefono, #fijo
         tip_documento, # tipo_doc, # otro_dni,
         fec_prueba=fecha_prueba,
         # no disponible: hor_prueba, # hora de sumision, no de prueba
         # no disponible: id clasificacion
         id_procedencia,
         id_tipo_resultado1,
         id_tipo_resultado2,
         nro_visita,
         # no disponible: observacion
         pcd,
         # starts_with("sintomas"),
         sintomas_si_no,
         sintomas_tos,
         sintomas_dolor_garganta,
         sintomas_congestion,
         sintomas_fiebre,
         # sintomas_malestar_general,
         # sintomas_dificultad_respiratoria,
         sintomas_diarrea,
         sintomas_nauseas,
         sintomas_cefalea,
         # sintomas_otro,
         # sintomas_irritabilidad/confusión,
         # sintomas_dolor_muscular,
         sintomas_dolor_abdominal,
         sintomas_dolor_pecho,
         # sintomas_dolor_articulaciones,
         # starts_with("condicion_riesgo")
         condicion_riesgo:condicion_riesgo_otro,-condicion_riesgo
         ) %>% 
  rename_at(.vars = vars(starts_with("condicion_riesgo_")),
            .funs = str_replace,"condicion_riesgo_","riesgo_") %>% 
  
  # repeat for each sent pack of data
  filter(!is_in(id_envio,
               read_csv("table/20200706-cdc_seroprevalencia-entrega-01.csv") %>% 
                 pull(id_envio))) %>% 
  
  # only one output file of new cases to report
  write_csv("table/20200706-cdc_seroprevalencia-entrega-02.csv")

# read_csv("table/20200706-cdc_seroprevalencia-entrega-02.csv") %>% glimpse()

# E consolidados -------------------------------------------------------------

consolidados <- read_rds("data/cdc-consolidados_a_ins.rds") %>% 
  filter(!is.na(n_final))

# consolidados %>% 
#   filter(n_final=="72848458")

# to INS -------------------------------------------------------------

uu_raw_data %>% glimpse()

# uu_raw_data %>% count(presente_prueba,resultado_pr,tipo_muestra_pcr)

uu_raw_data %>% 
  
  #restringir a solo resultados PR en el presente
  filter(presente_prueba=="si",tipo_muestra_pcr=="nasal") %>% #1229
  
  # * pendiente: cruzar con listas de excels
  filter(magrittr::is_in(dni,consolidados %>% pull(n_final))) %>% #1053 -> 1099
  # filter(!magrittr::is_in(dni,consolidados %>% pull(n_final))) %>% #176
  # select(presente_prueba,tipo_muestra_pcr,contains("dni"),
  #        nombres,contains("apellido"),
  #        diris,fecha_prueba,nm_dist,conglomerado) %>%
  # # writexl::write_xlsx("table/00-20200708-in_kobo-not_consolidado.xlsx")
  # # count(diris) %>%
  # count(diris,nm_dist) %>%
  # arrange(diris,desc(n)) %>%
  # avallecam::print_inf()
  
  # select(fecha_prueba) %>% naniar::miss_var_summary()
  # count(diris)
  
  #formato fecha
  # select(contains("fecha")) %>% 
  mutate_at(.vars = vars(contains("fecha")),
            .funs = str_replace_all,"(....)-(..)-(..)","\\3/\\2/\\1") %>% 
  mutate(fecha_toma_de_muestra=fecha_prueba,
         fecha_solicitud=fecha_prueba) %>% 
  
  # establecimiento
  mutate(establecimiento_origen=str_c(ipress," - ",ipress_name," - ",diris)) %>% 
  
  mutate(
      establecimiento_destino = "INS ROM",
      establecimiento_envio = "0",
      enfermedad = "covid-19",
      examen = "detección molecular sars-cov2",
      tipo_documento_solicitante = "DNI",
      nro_documento_solicitante = "40393185",
      nombre_solicitante = "Mary ",
      apellido_paterno_solicitante = "Reyes",
      apellido_materno_solicitante = "Vega",
      establecimiento_solicitante = "CDC PERU"
    ) %>% 
  
  select(
    # peru_depa,
    # peru_prov,
    # peru_dist,
    starts_with("cd_"),starts_with("nm_"),
    direccion,
    tipo_doc,
    otro_dni,
    dni,
    nombres,
    apellido_paterno,
    apellido_materno,
    telefono,
    otro_telefono,
    sexo,
    fecha_nacimiento,
    edad,
    pais_origen,
    pais_otros,
    sintomas_si_no,
    fecha_inicio_sintomas,
    sintomas_fiebre,
    sintomas_dolor_garganta,
    sintomas_tos,
    sintomas_congestion,
    sintomas_disnea,
    tipo_muestra_pcr,
    riesgo,#condicion_riesgo,
    condicion_riesgo_mayor_60a,
    condicion_riesgo_hipertension,
    condicion_riesgo_enf_cardiovascular,
    condicion_riesgo_diabetes,
    condicion_riesgo_obesidad,
    fecha_toma_de_muestra,
    fecha_solicitud,
    establecimiento_origen,
    establecimiento_destino,
    establecimiento_envio,
    enfermedad,
    examen,
    tipo_documento_solicitante,
    nro_documento_solicitante,
    nombre_solicitante,
    apellido_paterno_solicitante,
    apellido_materno_solicitante,
    establecimiento_solicitante
  ) %>% 
  
  # repeat for each sent pack of data
  filter(!is_in(dni,
                read_csv("table/20200707-cdc_seroprevalencia-entrega-01-netlab.csv") %>% 
                  pull(dni))) %>% 
  
  # only one output file of new cases to report
  write_csv("table/20200707-cdc_seroprevalencia-entrega-02-netlab.csv")

read_csv("table/20200707-cdc_seroprevalencia-entrega-02-netlab.csv") %>%
  naniar::miss_var_summary() %>%
  avallecam::print_inf()
  glimpse()

