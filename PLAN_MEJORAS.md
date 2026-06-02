# Plan de mejoras dosr — revisión general y detallada

> Revisión del estado en v0.3.0 (rama `main`). Cada tarea está redactada
> para poder implementarse de forma independiente. Prioridad: **P0**
> bloquea/arriesga CRAN · **P1** correcciones de fondo · **P2**
> mantenibilidad/UX · **P3** sitio/extras. Casilla `[ ]` = pendiente.

------------------------------------------------------------------------

## 0. Resumen ejecutivo

El paquete está sólido en funcionalidad y tiene buena cobertura de tests
(~930 líneas), viñeta, sitio pkgdown y datos incluidos. Los principales
riesgos son: (a) detalles que provocan NOTES en `R CMD check` y fricción
con CRAN, (b) ~500 líneas de código duplicado en `public_api.R` que ya
generaron una inconsistencia real (`obs_cuantil`), y (c) varias
afirmaciones de `NEWS.md` que no se cumplen (ejemplos ejecutables,
criterios configurables en todas las funciones).

------------------------------------------------------------------------

## P0 — Bloqueantes / riesgo CRAN

**Strings no-ASCII en código fuente.** `R/engine.R` líneas ~161, 167,
179, 435, 440 contienen literales con tildes dentro de
`paste0`/[`rlang::warn`](https://rlang.r-lib.org/reference/abort.html)
(“combinación”, “estándar”, “devolverá”). `R CMD check` emite *“found
non-ASCII strings”*. Convertir esos literales a escapes `\uXXXX` (como
ya se hace en los [`stop()`](https://rdrr.io/r/base/stop.html) de
`public_api.R`). Los comentarios y roxygen con tildes están bien.

**Faltan `@examples` en las funciones exportadas.**
`obs_prop/media/total/ratio/cuantil` no tienen sección de ejemplos en
sus `.Rd` (solo `multi_bin`). `NEWS.md` 0.3.0 afirma “Ejemplos
ejecutables en todas las funciones exportadas” → la afirmación es falsa.
Añadir `@examples` en `R/public_api.R` usando `casen_2022`/`casen_2024`
con `save_xlsx = FALSE` (o `dir = tempdir()`), envolviendo los más
lentos en `\donttest{}`. Regenerar con `devtools::document()`.

**No existe workflow de `R CMD check`.** Solo hay
`.github/workflows/pkgdown.yml`. Añadir
`.github/workflows/R-CMD-check.yaml`
(`usethis::use_github_action("check-standard")`) para validar en
Linux/macOS/Windows en cada push/PR antes de pensar en CRAN.

**`stata/` se incluiría en el tarball.** Está en `.gitignore` pero NO en
`.Rbuildignore`; `R CMD build` empaqueta por directorio, no por git →
NOTE de archivos no estándar. Añadir `^stata$` a `.Rbuildignore`.
Verificar también `casen_2022.RData`/`casen_2024.RData` (ya ignorados
✓).

**`.DS_Store` versionados.** `man/.DS_Store`, `R/.DS_Store`,
`man/figures/.DS_Store` están trackeados (aparecen en `git status`).
Quitarlos del índice (`git rm --cached`) y confirmar que el `.gitignore`
los cubre (ya lo hace). Añadir `^.*\.DS_Store$` a `.Rbuildignore` por si
acaso.

**Ejecutar `R CMD check --as-cran` localmente** (R 4.4.3 disponible) y
resolver cualquier NOTE/WARNING antes de subir. Actualizar
`cran-comments.md` con resultados reales (hoy dice win-builder/rhub
“pending”).

## P1 — Correcciones de fondo / consistencia

**`obs_cuantil` no expone los criterios de fiabilidad.** Su firma carece
de `cv_umbral_alto`, `cv_umbral_medio`, `n_minimo`, `nivel_confianza`;
usa los defaults del engine. Contradice `NEWS.md` (“Todos los criterios
de fiabilidad son configurables … en todas las funciones `obs_*`”).
Añadir esos argumentos y propagarlos a `calculate_single_design`.

**Helpers de validación muertos.** (`validate_inputs` cableado en las 5
funciones; consolidación pendiente en P2) `validate_inputs`,
`validate_designs`, `validate_filt` existen en `R/utils.R` pero **nunca
se llaman**; cada `obs_*` reimplementa la validación inline (y
`validate_inputs`, que comprueba existencia de variables, no se usa en
ningún lado → errores crípticos si `des`/`var` no existe). Decisión:
cablearlas en el flujo común (recomendado, ver P2) o eliminarlas. No
dejar ambas cosas.

**Taxonomía de fiabilidad inconsistente entre módulos.** `obs_*`/engine
usan etiquetas detalladas (“No Fiable (gl)”, “Poco Fiable (EE)”), pero
`multi_bin` usa las genéricas (“No Fiable”, “Poco Fiable”). Homologar
para que el output sea consistente en todo el paquete.

**Imports posiblemente innecesarios.** (Removidos
[`stats::filter`](https://rdrr.io/r/stats/filter.html),
[`stats::na.omit`](https://rdrr.io/r/stats/na.fail.html),
[`tibble::rownames_to_column`](https://tibble.tidyverse.org/reference/rownames.html))
Revisar `importFrom(stats, filter)` y
`importFrom(tibble, rownames_to_column)` (no parecen usarse). Limpiar
`NAMESPACE`/roxygen para evitar NOTES de imports no utilizados y
enmascaramiento de
[`stats::filter`](https://rdrr.io/r/stats/filter.html).

**`prop_val >= 1 ~ NA_character_`** (eliminado; proporciones del 100%
caen en “Fiable” o “Poco Fiable (EE)” según su SE) (engine, prop): una
proporción exacta de 100% deja `fiabilidad = NA`. Confirmar si es
intencional (caso degenerado dicotómico) y, si lo es, documentarlo; si
no, asignar etiqueta explícita.

**Inconsistencia de versión mínima de R.** (README actualizado a R ≥
3.5.0) `DESCRIPTION` dice `R (>= 3.5.0)`, el README dice “R ≥ 4.1”.
Elegir una y alinear ambos documentos.

**`NEWS.md` vs realidad.** (Comentario “Features 0.4.0” → “0.3.0” en
tests) Tras corregir P0/P1, revisar que todas las afirmaciones de 0.3.0
sean ciertas (ejemplos, criterios configurables). El comentario
“Features 0.4.0” en `tests/testthat/test-public-api.R` no corresponde a
la versión actual.

## P2 — Mantenibilidad / arquitectura / UX

**Eliminar duplicación masiva en `public_api.R`.** Las 5 funciones
`obs_*` repiten ~80 líneas idénticas (coerción a lista, `sufijo`,
validación `tbl_svy`, validación `filt`, etiqueta de variable,
`design_metadata`, `create_lightweight_designs`, plan paralelo,
`calc_fun` con manejo de `survey.lonely.psu`, agregación, factor levels,
`arrange/select`, guardado Excel). Extraer a helpers internos, p. ej.:

- `.prepare_designs(designs, sufijo)` → lista nombrada + metadata +
  validaciones.
- `.run_estimations(designs_light, meta, ..., parallel, n_cores)` →
  maneja paralelo y lonely PSU.
- `.finalize_results(hojas_list, keys, designs)` → factores + orden +
  consolidado. Reduce ~500 líneas y elimina la clase de bug que ya
  produjo la divergencia de `obs_cuantil`.

**Cablear las validaciones tempranas** (`validate_designs`,
`validate_inputs`, `validate_filt`) dentro del helper común para dar
errores claros cuando `var`/`des` no existen en el diseño.

**Reducir
[`utils::globalVariables`](https://rdrr.io/r/utils/globalVariables.html).**
Muchas de esas variables se eliminarían usando `.data[[ ]]`/`.env` de
forma consistente en `dplyr`. Mejora la robustez frente a cambios.

**Tests adicionales** (tras P1):

- `obs_cuantil` con umbrales personalizados.
- `color_fiabilidad = TRUE` (que no falle y aplique estilos).
- `rm_na_des = TRUE` y los totales filtrados en Excel.
- Ruta `parallel = TRUE` con 2 diseños.
- Contenido de las tablas de significancia (`sig = TRUE`) más allá de su
  existencia.
- Homologación de etiquetas de fiabilidad en `multi_bin`.

**Cobertura de tests** con `covr` + badge de Codecov.

**API de `filt` como string.** Documentar explícitamente por qué es un
string parseado (uso programático) y, opcionalmente, evaluar a futuro
una variante tidy-eval (`...`/quosures).

## P3 — Sitio web y documentación

**Badges en README**: R-CMD-check + lifecycle (stable) + MIT license.

**Artículo de metodología** (`vignettes/metodologia.Rmd`): criterios de
fiabilidad por tipo de estimación, fórmulas del umbral EE y CV, pruebas
de significancia inter-anuales, tabla interpretativa, referencias
citables. Visible en `articles/` del sitio pkgdown.

**`Description` de DESCRIPTION**: ‘CASEN’ y ‘Excel’ entre comillas
simples; descripción de criterios del Observatorio Social; referencia a
‘openxlsx’.

**Capturas/GIF** de un reporte Excel (requiere generación manual fuera
de CI).

**Galería / comparación inter-anual** como sección `eval=FALSE` en la
viñeta de metodología (2022 vs 2024 con `sig = TRUE`).

**Codecov badge** (requiere configurar token en el repositorio de
GitHub).

------------------------------------------------------------------------

## Orden de ejecución sugerido

1.  P0 completo → el paquete pasa `R CMD check --as-cran` limpio.
2.  P1 (empezar por `obs_cuantil` y validaciones, luego homologar
    etiquetas).
3.  P2 refactor (con la batería de tests como red de seguridad; correr
    tests antes y después).
4.  P3 sitio/docs.

## Comandos de verificación

``` r

devtools::document()        # regenerar .Rd/NAMESPACE
devtools::test()            # tests
devtools::check(args = "--as-cran")
covr::package_coverage()    # cobertura
pkgdown::build_site()       # sitio
```
