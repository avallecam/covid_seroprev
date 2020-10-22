
<!-- README.md is generated from README.Rmd. Please edit that file -->

# covid\_seroprev

<!-- badges: start -->

<!-- badges: end -->

The goal of `covid_seroprev` is to register the data management and
statistical analysis workflow used for the project and manuscript:
**“Prevalence of SARS-CoV-2 in Lima, Peru: a population-based
seroepidemiological survey”**

## Project Workflow

For a reproducible workflow of the analysis performed in this project go
to the [serosurvey](https://avallecam.github.io/serosurvey/) R package
website.

### Files:

  - `Encuesta ESPI_fisico.xlsx`: XLSForm used to collect data.
  - `01-clean`: import, clean and integration of data sources.
    recategorize and create variables.
  - `06-prevalence`: estimate prevalence.
  - `07-outputs.R`: create tables and figures.
  - `11-sampling_comparison.R`: contrast census and sample population.
  - `13-epicurve.R`: create epicurve from open data.
    <!-- - `15-distributions.R`: exploratory ecdf. -->

<!-- - 07-retorno_ins - verificar vinculación con base de participantes -->

<!-- - 01-clean - importe, limpieza e integración de bases de datos. recategorización y creación de variables -->

<!-- - 06-prevalence - estimación de prevalencias -->

<!-- - 08-regresion - exploratory causal analysis (pending)  -->

<!-- - 03 - diccionario de conglomerados y número de viviendas seleccionadas -->

<!-- - 04 - base general de consolidados enviados al INS -->

<!-- - unix makefile - correr a cada retorno de resultados PM por parte de INS -->

<!-- - 02 - exportación de resultados a SISCOVID e INS -->

<!-- - 05 - script libre para la identificación de problemas a notificar y corregir -->

<!-- ## to do list -->

<!-- () _ _ -->
