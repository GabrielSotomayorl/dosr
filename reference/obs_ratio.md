# Calcula razones (ratios) para diseños complejos

Calcula la razón entre dos variables numéricas (numerador/denominador)
sobre uno o más objetos \`tbl_svy\`, con las mismas opciones de
desagregación, paralelización y reporte usadas en \`obs_media\`.

## Usage

``` r
obs_ratio(
  designs,
  sufijo = NULL,
  num,
  den,
  des = NULL,
  multi_des = TRUE,
  es_var_estudio = FALSE,
  usar_etiqueta_var = TRUE,
  sig = FALSE,
  filt = NULL,
  rm_na_var = TRUE,
  rm_na_des = FALSE,
  parallel = FALSE,
  n_cores = NULL,
  save_xlsx = TRUE,
  dir = "output",
  formato = TRUE,
  decimales = 2,
  nombre = NULL,
  fuente = NULL,
  snac = FALSE,
  mostrar_pct_fiable = FALSE,
  color_fiabilidad = FALSE,
  universo_crit = FALSE,
  cv_umbral_alto = 0.3,
  cv_umbral_medio = 0.2,
  n_minimo = 30L,
  nivel_confianza = 0.95,
  verbose = TRUE
)
```

## Arguments

- designs:

  Un objeto \`tbl_svy\` o una lista de ellos.

- sufijo:

  Vector de strings para sufijos (p.ej. c("2020","2022")).

- num:

  String con el nombre de la variable \*\*numerador\*\*.

- den:

  String con el nombre de la variable \*\*denominador\*\*.

- des:

  Un vector de strings con los nombres de las variables de
  desagregación.

- multi_des:

  Booleano. Si \`TRUE\` (por defecto), calcula todas las combinaciones
  de \`des\`. Si \`FALSE\`, solo calcula las desagregaciones simples.

- es_var_estudio:

  Booleano. Si \`TRUE\`, aplica criterios de fiabilidad menos estrictos
  para el tamaño muestral. Por defecto es \`FALSE\`.

- usar_etiqueta_var:

  Booleano. Si \`TRUE\` (por defecto), usa las etiquetas de las
  variables \`num\` y \`den\` (si existen) como títulos y rótulos en los
  reportes de Excel; si no hay etiqueta disponible, usa el nombre de la
  variable.

- sig:

  Booleano. Si \`TRUE\`, calcula y añade pruebas de significancia
  estadística a las hojas de reporte con formato. Por defecto es
  \`FALSE\`.

- filt:

  Expresión de filtro. Acepta tanto una expresión R sin comillas (\`filt
  = edad \> 18\`) como un string (\`filt = "edad \> 18"\`). Ambas formas
  son equivalentes y retrocompatibles.

- rm_na_var:

  Booleano. Si \`TRUE\`, elimina observaciones con \`NA\` en el
  \*\*numerador o\*\* el \*\*denominador\*\* antes de calcular la razón.

- rm_na_des:

  Booleano. Si \`TRUE\`, excluye las observaciones con \`NA\` en las
  variables de desagregación correspondientes a cada tabla solicitada.

- parallel:

  Booleano. Activa el cálculo en paralelo. Con un único diseño y
  múltiples desagregaciones distribuye las combinaciones entre workers;
  con múltiples diseños distribuye los diseños. Por defecto \`FALSE\`.

- n_cores:

  Entero. Número de workers a usar. Si es \`NULL\`, se usa un valor
  seguro (máximo 4).

- save_xlsx:

  Booleano. Si \`TRUE\`, guarda un reporte en Excel.

- dir:

  Un string con la ruta del directorio donde se guardará el archivo
  Excel. Por defecto es \`"output"\`.

- formato:

  Booleano. Si \`TRUE\`, genera un reporte de Excel con formato
  avanzado.

- decimales:

  Entero. Número de decimales para las estimaciones en Excel.

- nombre:

  String. Nombre del indicador que se muestra en el reporte Excel. Si se
  especifica, sobreescribe la etiqueta de variable aunque
  \`usar_etiqueta_var = TRUE\`.

- fuente:

  String. Fuente de los datos para el pie del reporte Excel. Acepta
  claves estándar (\`"casen"\`, \`"ebs"\`, \`"endide"\`, \`"eanna"\`,
  \`"elpi"\`) o texto libre.

- snac:

  Booleano. Si \`TRUE\`, omite la hoja de formato del nivel nacional. El
  consolidado siempre incluye el nivel nacional. Por defecto \`FALSE\`.

- mostrar_pct_fiable:

  Booleano. Si \`TRUE\`, la nota de fiabilidad incluye el porcentaje de
  estimaciones fiables del cuadro. Por defecto \`FALSE\`.

- color_fiabilidad:

  Booleano. Si \`TRUE\`, colorea el texto de las celdas de estimación
  según su fiabilidad: ámbar para poco fiable, rojo para no fiable. Por
  defecto \`FALSE\`.

- universo_crit:

  Booleano. Solo aplica a \`obs_prop\`. Si \`TRUE\`, fuerza el uso del N
  total del dominio (suma de categorías) para el criterio muestral de
  fiabilidad, independientemente del número de categorías. Por defecto
  \`FALSE\` (comportamiento automático).

- cv_umbral_alto:

  Numérico. Umbral de CV para clasificar una estimación como "No Fiable
  (CV)". Por defecto \`0.30\`.

- cv_umbral_medio:

  Numérico. Umbral de CV para clasificar una estimación como "Poco
  Fiable (CV)". Por defecto \`0.20\`.

- n_minimo:

  Entero. Tamaño muestral mínimo para el criterio de fiabilidad. Por
  defecto \`30\`.

- nivel_confianza:

  Numérico. Nivel de confianza para intervalos y pruebas de
  significancia. Por defecto \`0.95\`.

- verbose:

  Booleano. Si \`TRUE\` (por defecto), muestra mensajes de progreso.

## Value

Un \`data.frame\` con los resultados consolidados (invisiblemente).

## Examples

``` r
# \donttest{
library(srvyr)
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
design_2022 <- as_survey_design(casen_2022, ids = varunit,
                                strata = varstrat, weights = expr, nest = TRUE)
design_2022$variables <- design_2022$variables %>%
  mutate(mujer  = as.integer(as.numeric(sexo) == 2),
         hombre = as.integer(as.numeric(sexo) == 1))
obs_ratio(design_2022, sufijo = "2022", num = "mujer", den = "hombre",
          save_xlsx = FALSE, verbose = FALSE)
# }
```
