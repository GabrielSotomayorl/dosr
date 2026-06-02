# Perfilar Múltiples Variables Dicotómicas con Criterios de Calidad

Para un \*\*único diseño de encuesta\*\*, calcula la proporción de "1s"
para un vector de variables dicotómicas (0/1). Esta función es una
herramienta de conveniencia para perfilados rápidos y análisis
exploratorios. A diferencia de \`obs_prop\`, está optimizada para
analizar múltiples variables dentro de una sola encuesta, pero no para
comparar entre años. Genera un reporte en Excel con una hoja consolidada
(con todas las métricas de calidad) y hojas de formato para el nivel
nacional y cada desagregación simple.

## Usage

``` r
multi_bin(
  design,
  vars_binarias,
  des = NULL,
  es_var_estudio = FALSE,
  filt = NULL,
  dir = "output",
  filename = NULL,
  decimales = 1,
  decimales_se = 3,
  n_minimo = 30,
  verbose = TRUE
)
```

## Arguments

- design:

  Un objeto \`tbl_svy\` de \`srvyr\`.

- vars_binarias:

  Un vector de strings con los nombres de las variables dicotómicas
  (codificadas como 0/1) a perfilar.

- des:

  Un vector de strings con los nombres de las variables de desagregación
  simple.

- es_var_estudio:

  Booleano. Si \`TRUE\`, aplica criterios de fiabilidad menos estrictos
  para el tamaño muestral.

- filt:

  Un string con una expresión de filtro para \`dplyr::filter()\`.

- dir:

  Un string con la ruta del directorio de salida.

- filename:

  Un string con el nombre del archivo Excel.

- decimales:

  Entero. Número de decimales para la estimación puntual. Por defecto es
  1.

- decimales_se:

  Entero. Número de decimales para el error estándar. Por defecto es 3.

- n_minimo:

  Entero. Tamaño muestral mínimo para clasificar una estimación como
  fiable. Por defecto es \`30\`.

- verbose:

  Booleano. Si \`TRUE\`, muestra mensajes de progreso.

## Value

Un data.frame con todos los resultados consolidados (invisiblemente).

## Examples

``` r
# \donttest{
library(srvyr)
#> 
#> Attaching package: ‘srvyr’
#> The following object is masked from ‘package:stats’:
#> 
#>     filter

design_2024 <- as_survey_design(casen_2024, ids = varunit,
                                strata = varstrat, weights = expr, nest = TRUE)

# Prevalencia de indicadores de inseguridad alimentaria por región
multi_bin(design_2024, vars_binarias = paste0("r8", letters[1:8]),
          des = "region", dir = tempdir())
#> Aplicando filtro (si aplica)...
#> Calculando perfil nacional...
#> Calculando desagregación por: region ...
#> Generando reporte Excel...
#> Reporte Excel creado en: /tmp/RtmpkUVzUf/r8a-r8h_region_MULT.xlsx
# }
```
