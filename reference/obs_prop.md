# Calcula estimaciones de proporciones para diseños complejos

Procesa uno o más \`tbl_svy\` para calcular proporciones.

## Usage

``` r
obs_prop(
  designs,
  sufijo = NULL,
  var,
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
  porcentaje = TRUE,
  decimales = 2,
  nombre = NULL,
  fuente = NULL,
  snac = FALSE,
  mostrar_pct_fiable = FALSE,
  color_fiabilidad = FALSE,
  universo_crit = FALSE,
  categoria = NULL,
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

- var:

  Un string con el nombre de la variable de interés (numérica).

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

  Booleano. Si \`TRUE\` (por defecto), usa la etiqueta de la variable
  \`var\` como título en los reportes de Excel. Si es \`FALSE\` o la
  variable no tiene etiqueta, usa el nombre de la variable.

- sig:

  Booleano. Si \`TRUE\`, calcula y añade pruebas de significancia
  estadística a las hojas de reporte con formato. Por defecto es
  \`FALSE\`.

- filt:

  Un string con una expresión de filtro para \`dplyr::filter()\`.

- rm_na_var:

  Booleano. Si \`TRUE\`, elimina NAs en \`var\` antes de calcular.

- rm_na_des:

  Booleano. Si \`TRUE\`, excluye las observaciones con \`NA\` en las
  variables de desagregación correspondientes a cada tabla solicitada.

- parallel:

  Booleano. Activa el cálculo en paralelo.

- n_cores:

  Entero. Número de núcleos a usar. Si es NULL, se usa un valor seguro.

- save_xlsx:

  Booleano. Si \`TRUE\`, guarda un reporte en Excel.

- dir:

  Un string con la ruta del directorio donde se guardará el archivo
  Excel. Por defecto es \`"output"\`.

- formato:

  Booleano. Si \`TRUE\`, genera un reporte de Excel con formato
  avanzado.

- porcentaje:

  Booleano. Si \`TRUE\`, las estimaciones y errores estándar se
  multiplican por 100.

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

- categoria:

  Vector de valores (labels o códigos numéricos) para filtrar las
  categorías de \`var\` a mostrar en el output. Las demás categorías se
  excluyen del resultado y del reporte Excel.

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

Un data.frame con los resultados consolidados (invisiblemente).

## Examples

``` r
# \donttest{
library(srvyr)
design_2022 <- as_survey_design(casen_2022, ids = varunit,
                                strata = varstrat, weights = expr, nest = TRUE)
obs_prop(design_2022, sufijo = "2022", var = "pobreza",
         porcentaje = TRUE, save_xlsx = FALSE, verbose = FALSE)
# }
```
