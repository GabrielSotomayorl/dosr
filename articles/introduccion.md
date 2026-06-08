# Introducción a dosr

`dosr` provee funciones de alto nivel para calcular estimaciones sobre
diseños de encuestas complejas (como la CASEN) y generar reportes
estandarizados en Excel con clasificación de fiabilidad estadística.

## Instalación

``` r

remotes::install_github("GabrielSotomayorl/dosr")
```

## Datos incluidos

El paquete incluye dos subconjuntos de la Encuesta CASEN listos para
usar: `casen_2022` (202 231 hogares) y `casen_2024` (218 367 hogares).
Ambas contienen las variables necesarias para reproducir todos los
ejemplos de esta viñeta.

``` r

library(dosr)
library(srvyr)
#> 
#> Attaching package: 'srvyr'
#> The following object is masked from 'package:stats':
#> 
#>     filter

design_2022 <- as_survey_design(casen_2022,
  ids     = varunit,
  strata  = varstrat,
  weights = expr
)

design_2024 <- as_survey_design(casen_2024,
  ids     = varunit,
  strata  = varstrat,
  weights = expr
)
```

## Proporciones: `obs_prop()`

Distribución de la población según situación de pobreza, desagregada por
región:

``` r

resultado_prop <- obs_prop(
  design_2022,
  sufijo     = "2022",
  var        = "pobreza",
  des        = "region",
  porcentaje = TRUE,
  save_xlsx  = FALSE,
  verbose    = FALSE
)
head(resultado_prop[, c("region", "pobreza", "prop_2022", "fiabilidad_2022")])
#> # A tibble: 6 × 4
#>   region             pobreza             prop_2022 fiabilidad_2022
#>   <fct>              <fct>                   <dbl> <chr>          
#> 1 NA                 Pobreza extrema          8.48 Fiable         
#> 2 NA                 Pobreza no extrema      12.0  Fiable         
#> 3 NA                 Fuera de la pobreza     79.5  Fiable         
#> 4 Región de Tarapacá Pobreza extrema         12.6  Fiable         
#> 5 Región de Tarapacá Pobreza no extrema      14.1  Fiable         
#> 6 Región de Tarapacá Fuera de la pobreza     73.2  Fiable
```

## Medias: `obs_media()`

Ingreso total del hogar corregido (`ytotcorh`) promedio por región en
2022:

``` r

resultado_media <- obs_media(
  design_2022,
  sufijo    = "2022",
  var       = "ytotcorh",
  des       = "region",
  save_xlsx = FALSE,
  verbose   = FALSE
)
head(resultado_media[, c("region", "media_2022", "fiabilidad_2022")])
#> # A tibble: 6 × 3
#>   region                media_2022 fiabilidad_2022
#>   <fct>                      <dbl> <chr>          
#> 1 NA                      1713534. Fiable         
#> 2 Región de Tarapacá      1515201. Fiable         
#> 3 Región de Antofagasta   1819321. Fiable         
#> 4 Región de Atacama       1487318. Fiable         
#> 5 Región de Coquimbo      1438892. Fiable         
#> 6 Región de Valparaíso    1499137. Fiable
```

## Cuantiles: `obs_cuantil()`

Mediana del ingreso del hogar por región:

``` r

resultado_cuantil <- obs_cuantil(
  design_2022,
  sufijo    = "2022",
  var       = "ytotcorh",
  des       = "region",
  cuant     = 0.5,
  save_xlsx = FALSE,
  verbose   = FALSE
)
head(resultado_cuantil[, c("region", "cuantil_2022", "fiabilidad_2022")])
#> # A tibble: 6 × 3
#>   region                cuantil_2022 fiabilidad_2022
#>   <fct>                        <dbl> <chr>          
#> 1 NA                         1250000 Fiable         
#> 2 Región de Tarapacá         1230736 Fiable         
#> 3 Región de Antofagasta      1416697 Fiable         
#> 4 Región de Atacama          1205000 Fiable         
#> 5 Región de Coquimbo         1120083 Fiable         
#> 6 Región de Valparaíso       1165472 Fiable
```

## Totales: `obs_total()`

Población en situación de pobreza por ingresos (variable binaria
construida desde `pobreza`):

``` r

library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

design_2022_pob <- design_2022
design_2022_pob$variables <- design_2022_pob$variables %>%
  mutate(pobre = as.integer(as.numeric(pobreza) %in% c(1, 2)))

resultado_total <- obs_total(
  design_2022_pob,
  sufijo    = "2022",
  var       = "pobre",
  des       = "region",
  save_xlsx = FALSE,
  verbose   = FALSE
)
head(resultado_total[, c("region", "total_2022", "fiabilidad_2022")])
#> # A tibble: 6 × 3
#>   region                total_2022 fiabilidad_2022
#>   <fct>                      <dbl> <chr>          
#> 1 NA                       4070695 Fiable         
#> 2 Región de Tarapacá        106748 Fiable         
#> 3 Región de Antofagasta     138875 Fiable         
#> 4 Región de Atacama          67461 Fiable         
#> 5 Región de Coquimbo        209288 Fiable         
#> 6 Región de Valparaíso      419063 Fiable
```

## Razones: `obs_ratio()`

Razón de feminidad (mujeres / hombres) por región:

``` r

design_2022_sex <- design_2022
design_2022_sex$variables <- design_2022_sex$variables %>%
  mutate(
    mujer  = as.integer(as.numeric(sexo) == 2),
    hombre = as.integer(as.numeric(sexo) == 1)
  )

resultado_ratio <- obs_ratio(
  design_2022_sex,
  sufijo    = "2022",
  num       = "mujer",
  den       = "hombre",
  des       = "region",
  save_xlsx = FALSE,
  verbose   = FALSE
)
head(resultado_ratio[, c("region", "ratio_2022", "fiabilidad_2022")])
#> # A tibble: 6 × 3
#>   region                ratio_2022 fiabilidad_2022
#>   <fct>                      <dbl> <chr>          
#> 1 NA                         1.03  Fiable         
#> 2 Región de Tarapacá         0.984 Fiable         
#> 3 Región de Antofagasta      0.989 Fiable         
#> 4 Región de Atacama          0.983 Fiable         
#> 5 Región de Coquimbo         1.04  Fiable         
#> 6 Región de Valparaíso       1.05  Fiable
```

## Trabajo con múltiples diseños

Pasando una lista de diseños con el argumento `sufijo`, se comparan
múltiples rondas en una sola llamada. El ejemplo compara la tasa de
pobreza por región entre 2022 y 2024:

``` r

resultado_serie <- obs_prop(
  designs    = list(design_2022, design_2024),
  sufijo     = c("2022", "2024"),
  var        = "pobreza",
  des        = "region",
  porcentaje = TRUE,
  save_xlsx  = FALSE,
  verbose    = FALSE
)
cols <- c("region", "pobreza", "prop_2022", "prop_2024",
          "fiabilidad_2022", "fiabilidad_2024")
head(resultado_serie[, cols])
#> # A tibble: 6 × 6
#>   region             pobreza prop_2022 prop_2024 fiabilidad_2022 fiabilidad_2024
#>   <fct>              <fct>       <dbl>     <dbl> <chr>           <chr>          
#> 1 NA                 Pobrez…      8.48      6.87 Fiable          Fiable         
#> 2 NA                 Pobrez…     12.0      10.4  Fiable          Fiable         
#> 3 NA                 Fuera …     79.5      82.7  Fiable          Fiable         
#> 4 Región de Tarapacá Pobrez…     12.6       8.97 Fiable          Fiable         
#> 5 Región de Tarapacá Pobrez…     14.1      11.8  Fiable          Fiable         
#> 6 Región de Tarapacá Fuera …     73.2      79.2  Fiable          Fiable
```

## Múltiples variables binarias: `multi_bin()`

Prevalencia de los ocho indicadores de inseguridad alimentaria (FIES) de
la CASEN 2024, desagregada por área urbana/rural:

``` r

resultado_bin <- multi_bin(
  design_2024,
  vars_binarias = paste0("r8", letters[1:8]),
  des           = "area",
  dir           = tempdir(),
  verbose       = FALSE
)
#> Reporte Excel creado en: /tmp/RtmpykOX1Y/r8a-r8h_area_MULT.xlsx
nac <- resultado_bin$desagregacion_tipo == "Nacional"
resultado_bin[nac, c("etiqueta", "estimacion", "fiabilidad")]
#> # A tibble: 8 × 3
#>   etiqueta estimacion fiabilidad
#>   <chr>         <dbl> <chr>     
#> 1 r8a           31.2  Fiable    
#> 2 r8b           22.7  Fiable    
#> 3 r8c           23.8  Fiable    
#> 4 r8d           11.3  Fiable    
#> 5 r8e           16.1  Fiable    
#> 6 r8f            9.28 Fiable    
#> 7 r8g            8.93 Fiable    
#> 8 r8h            5.14 Fiable
```

## Clasificación de fiabilidad

Todos los resultados incluyen una columna `fiabilidad` (o
`fiabilidad_{sufijo}` en series de tiempo):

| Valor           | Significado              |
|-----------------|--------------------------|
| **Fiable**      | Estimación publicable    |
| **Poco Fiable** | Publicar con advertencia |
| **No Fiable**   | No publicar              |
| **Sin casos**   | Subgrupo vacío           |

Los umbrales son configurables con `cv_umbral_alto`, `cv_umbral_medio` y
`n_minimo`.

## Reportes Excel

Cuando `save_xlsx = TRUE` (valor por defecto), cada función genera un
`.xlsx` en el directorio `dir` con:

- **1_Consolidado**: tabla completa con todas las métricas de calidad
- **2_nac**: hoja de formato para el nivel nacional
- **2\_{variable}**: una hoja de formato por cada variable de
  desagregación

Con `sig = TRUE` y múltiples diseños, las hojas de formato incluyen
adicionalmente tablas de p-valores para comparaciones intra-año, contra
el año anterior y contra el total nacional.
