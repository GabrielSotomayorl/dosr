# Plan de mejoras `dosr`

> Estado base: v0.3.0 (rama `main`). El grueso de la revisión inicial (CRAN, refactor,
> consistencia, sitio) **ya está implementado** — ver "Completado". El trabajo vivo ahora es
> la **exploración de rendimiento** (sección P-PERF), guiada por mediciones y pensada para
> ejecutarse de forma independiente. Cada tarea está redactada para implementarse sola.

---

## Completado en v0.3.0

Resumen de lo ya hecho (no requiere acción, queda como registro):

- **CRAN / `R CMD check`:** strings no-ASCII escapados; `@examples` en las 5 funciones
  `obs_*`; workflow `R-CMD-check.yaml` (Linux/macOS/Windows); `^stata$` y `.DS_Store` en
  `.Rbuildignore`; check `--as-cran` local limpio.
- **Consistencia:** `obs_cuantil` expone los umbrales de fiabilidad; validaciones
  (`validate_filt`, `validate_inputs`) cableadas en el flujo común; taxonomía de fiabilidad
  homologada entre `engine`/`obs_*` y `multi_bin`; imports innecesarios limpiados; versión
  mínima de R alineada (≥ 3.5.0) entre `DESCRIPTION` y `README`.
- **Arquitectura:** duplicación de `public_api.R` extraída a `R/api_helpers.R`
  (`.prepare_designs_list`, `.build_meta_and_light`, `.make_calc_fun`, `.run_estimations`,
  `.finalize_results`); batería de tests amplia (`test-engine`, `-public-api`, `-multi-bin`,
  `-excel`, `-significance`, `-utils`).
- **Sitio / docs:** badges; viñetas `introduccion` y `metodologia`; sitio pkgdown con logo,
  favicons, idioma `es`, cita CASEN correcta; capturas de Excel en README.

### Pendientes menores (opcionales, sin urgencia)

- [ ] Reducir `utils::globalVariables` (31 entradas en `R/globals.R`) usando `.data[[ ]]`/`.env`
  de forma consistente. Cosmético; mejora robustez ante cambios de dplyr.
- [ ] Cobertura con `covr` + badge de Codecov (requiere configurar token en GitHub).
- [ ] Documentar explícitamente por qué `filt` es un string parseado (uso programático) y, a
  futuro, evaluar variante tidy-eval.

---

## P-PERF — Rendimiento (exploración guiada por mediciones)

### Contexto que condiciona TODO este apartado

**1. Uso real (qué escenarios importan).** En el Observatorio Social lo más común NO es un año
suelto: se analiza la **serie CASEN, bianual, 2009→2024 (≈ 8 rondas)** en una sola corrida. Por
eso el escenario *headline* de los benchmarks es una **lista de ~8 diseños**. Como solo tenemos
dos años de ejemplo en el paquete (`casen_2022`, `casen_2024`, ~200 mil filas cada uno), la serie
se **simula duplicando** esos datos con sufijos 2009…2024 (el costo de cómputo por ronda es el
mismo, que es lo que mide el tiempo; los p-valores de significancia salen degenerados, da igual
para *timing*). **También** ocurre el caso de un solo año, donde la paralelización por
combinaciones puede ayudar **si no rompe nada**.

**2. Riesgo de hardware (crítico para las decisiones).** Desarrollamos en **Mac M1** (CPU rápida,
memoria unificada veloz, *fork* disponible). Los usuarios finales típicos están en **Windows,
i7 de hace ~15 años, 16 GB RAM, disco lento**. Dos consecuencias que cambian las conclusiones:

- En **Windows NO existe `fork`** → `multicore` no está disponible; `future` cae siempre a
  `multisession` (procesos nuevos + serialización del diseño a cada worker). **Toda mejora que
  dependa de `fork` (ver PERF-2) da CERO en Windows.** No optimizar solo para nuestra máquina.
- En esas máquinas el *overhead* de levantar workers y serializar ~200 mil filas pesa mucho más,
  y hay menos núcleos útiles. El umbral a partir del cual paralelizar conviene es **distinto** y
  hay que **medirlo en la máquina objetivo**, no en la M1.

**Conclusión de diseño:** priorizar mejoras que ayuden al **camino lento universal** (un solo
hilo, cualquier plataforma) — típicamente reducir el número de pasadas `survey_*` (PERF-3,
PERF-5). Las mejoras de paralelización (PERF-1/2) son ganancia condicional y **deben validarse
en hardware tipo usuario** antes de aceptarse.

> **Cómo trabajar esta sección.** Medir primero (PERF-0), validar en otra máquina (PERF-0b), y
> recién ahí decidir qué optimizar. Correr `devtools::test()` antes y después de cada cambio:
> ninguna mejora de rendimiento puede alterar resultados numéricos ni etiquetas de fiabilidad.

### PERF-0 — Harness portable + línea base + profiling (PRERREQUISITO)

- [x] **Añadir a `Suggests`** en `DESCRIPTION`: `bench`, `profvis`. No tocan producción.
- [x] **Crear `dev/bench_dosr.R`** (carpeta fuera del build; añadir `^dev$` a `.Rbuildignore`).
  Debe ser **portable**: usa el paquete instalado (`library(dosr)`), captura las specs de la
  máquina y escribe un CSV comparable entre equipos. El mismo script corre en la M1 (línea base)
  y en la máquina Windows (PERF-0b). Esqueleto:

  ```r
  # dev/bench_dosr.R — correr en cualquier máquina: source("dev/bench_dosr.R")
  # En desarrollo: reemplazar library(dosr) por devtools::load_all(".") si se está iterando.
  library(dosr); library(srvyr); library(bench)

  ## --- EDITAR a mano en cada máquina (lo que detectCores no sabe) ---
  maquina_id  <- "m1_dev"          # p.ej. "win_i7_2010"
  cpu_modelo  <- "Apple M1"        # modelo de CPU
  ram_gb      <- 16                # RAM física
  ## ------------------------------------------------------------------

  ## Serie tipo CASEN bianual 2009-2024 (~8 rondas) simulada por duplicación.
  ## El costo por ronda es realista; los p-valores salen degenerados (ok para timing).
  anios   <- c(2009, 2011, 2013, 2015, 2017, 2019, 2022, 2024)
  fuentes <- list(casen_2022, casen_2024)
  designs <- lapply(seq_along(anios), function(i)
    as_survey_design(fuentes[[(i %% 2) + 1]], ids = varunit, strata = varstrat,
                     weights = expr, nest = TRUE))
  sufijos <- as.character(anios)
  d1  <- designs[[1]]                 # un solo año
  tmp <- tempdir()

  res <- bench::mark(
    iterations = 3, check = FALSE, filter_gc = FALSE,
    # --- caso común: serie de 8 rondas ---
    serie8_secuencial = obs_media(designs, sufijos, "ytotcorh", des = "region",
                                  parallel = FALSE, save_xlsx = FALSE, verbose = FALSE),
    serie8_paralelo   = obs_media(designs, sufijos, "ytotcorh", des = "region",
                                  parallel = TRUE,  save_xlsx = FALSE, verbose = FALSE),
    serie8_sig_xlsx   = obs_media(designs, sufijos, "ytotcorh", des = "region",
                                  sig = TRUE, parallel = TRUE, dir = tmp, verbose = FALSE),
    # --- caso de un año con 7 combos (objetivo de PERF-1) ---
    anio1_3des_multi  = obs_media(d1, "2022", "ytotcorh", des = c("region","sexo","area"),
                                  multi_des = TRUE, save_xlsx = FALSE, verbose = FALSE),
    # --- perfilado de variables binarias (objetivo de PERF-3) ---
    multibin_8x       = multi_bin(designs[[8]], paste0("r8", letters[1:8]), des = "region",
                                  dir = tmp, verbose = FALSE)
  )

  env <- data.frame(
    maquina_id = maquina_id, cpu = cpu_modelo, ram_gb = ram_gb,
    os = Sys.info()[["sysname"]], os_rel = Sys.info()[["release"]],
    arch = Sys.info()[["machine"]], r = paste(R.version$major, R.version$minor, sep = "."),
    cores = parallel::detectCores(), fork = future::supportsMulticore(),
    dosr = as.character(utils::packageVersion("dosr"))
  )
  out <- cbind(env, escenario = as.character(res$expression),
               mediana_s = as.numeric(res$median),
               mem_mb = round(as.numeric(res$mem_alloc) / 1e6, 1))
  print(out[, c("maquina_id","escenario","mediana_s","mem_mb","cores","fork")])

  dir.create("dev/bench_out", showWarnings = FALSE, recursive = TRUE)
  write.csv(out, sprintf("dev/bench_out/bench_%s.csv", maquina_id), row.names = FALSE)
  message("Guardado: dev/bench_out/bench_", maquina_id, ".csv")
  ```

- [ ] **Profiling de los 2 escenarios más lentos** _(benchmark ejecutándose, pendiente resultado)_ (en la M1) para saber dónde se va el tiempo
  ANTES de optimizar — sobre todo: ¿domina la estimación `survey_*` o la escritura Excel
  `generate_*_report`?
  ```r
  profvis::profvis(obs_media(designs, sufijos, "ytotcorh", des = "region",
                             sig = TRUE, parallel = FALSE, dir = tempdir(), verbose = FALSE))
  profvis::profvis(obs_media(d1, "2022", "ytotcorh", des = c("region","sexo","area"),
                             save_xlsx = FALSE, verbose = FALSE))
  ```
  **Entregable PERF-0:** `dev/bench_out/bench_m1_dev.csv` (línea base) + nota de 2–3 líneas sobre
  qué fase domina. Esa nota fija el orden real de PERF-1…PERF-5.

### PERF-0b — Validación cruzada en hardware tipo usuario (Windows i7) [crítico]

- [ ] **Entregar al usuario el mismo `dev/bench_dosr.R`** con instrucciones mínimas: instalar el
  paquete (`remotes::install_github(...)`), editar las 3 líneas de specs (`maquina_id`,
  `cpu_modelo`, `ram_gb`), `source("dev/bench_dosr.R")`, y devolver el CSV generado.
- [ ] **Comparar** `bench_m1_dev.csv` vs. `bench_win_*.csv` en una tablita única. Preguntas que
  el cuadro debe responder, y que **gobiernan las decisiones**:
  - ¿`serie8_paralelo` es realmente más rápido que `serie8_secuencial` en Windows, o el overhead
    de `multisession` lo anula? (en Windows `fork = FALSE` siempre).
  - ¿Cuál es el factor de ralentización M1→Windows por escenario? (calibra expectativas reales).
  - ¿La memoria (`mem_mb`) se acerca a los 16 GB en la serie de 8 con `sig + xlsx`? (riesgo de
    *swap* en la máquina lenta).
- [ ] **Criterio:** una optimización solo se "acepta" si mejora (o al menos no empeora) en el CSV
  de **Windows**, no solo en la M1. Documentar el resultado en el propio plan.

### PERF-1 — Paralelizar sobre combinaciones de desagregación (caso de un año) [impacto: alto en single-year]

- [ ] **Dónde:** `R/api_helpers.R:95` (`.run_estimations`) paraleliza con `furrr::future_pmap`
  **sobre la lista de diseños**, solo si `parallel && n_designs > 1`. El bucle de combos es
  secuencial: `R/engine.R:533` `purrr::map(combos, calc_tabla)`.
  **Hipótesis:** en el caso de **un solo año** con `multi_des = TRUE` y 3 variables (= 7 combos),
  no hay paralelismo alguno; paralelizar sobre combos lo aprovecharía. (En la serie multi-año el
  paralelismo por diseños ya aplica, así que esto es específico del single-year.)
- [ ] **Experimento:** variante de `calculate_estimates` con `furrr::future_map(combos, ...)`
  cuando `parallel = TRUE` y `n_designs == 1`. Comparar `anio1_3des_multi` sec. vs. paralelo,
  **en M1 y en Windows**.
- [ ] **Decisión:** aceptar solo si *speedup* > 1.5× **en Windows** (en M1 puede verse bonito y
  no traducirse). Si el overhead se come la ganancia (combos de pocos segundos en máquina lenta),
  rechazar. Evitar futures anidados (no paralelizar combos *y* años simultáneamente).
- [ ] **Riesgo:** serialización por worker; verificar output idéntico al secuencial.

### PERF-2 — `multicore` (fork) en Unix/macOS [impacto: medio-alto en M1/Linux, NULO en Windows]

- [x] **Dónde:** `R/api_helpers.R:108` fija `future::plan(multisession, ...)` siempre.
  **Hipótesis:** en macOS/Linux, `multicore` (fork, copy-on-write) evita re-serializar ~200 mil
  filas a cada worker → arranque casi nulo. **No aplica a Windows** (sin fork).
- [x] **Implementado:** `plan_fun <- if (future::supportsMulticore()) future::multicore else future::multisession`.
  M1/Linux → fork automático; Windows → multisession sin cambio visible.
  142/142 tests pasan. Cuantificación del speedup: pendiente resultado benchmark.

### PERF-3 — Vectorizar `multi_bin` sobre el conjunto de variables [impacto: alto y UNIVERSAL]

- [x] **Dónde:** `R/multi_bin.R:58` `.calculate_estimates_multi` hace `purrr::map_dfr(vars, ...)`:
  por **cada** variable lanza `survey_mean` + `survey_total` + 2 `summarise`. Para 8 indicadores
  FIES ≈ 32 pasadas de varianza.
  **Hipótesis:** `srvyr` permite estimar varias variables en **una** `summarise(across(...))`,
  con una sola pasada de linearización → gran recorte de tiempo. **Ayuda en todas las
  plataformas** (es menos trabajo, no más paralelismo) → es la mejor candidata para el usuario
  Windows.
- [x] **Implementado:** 3 `summarise` calls totales (medias / totales / conteos+gl) en vez de
  3N (N=vars). Para 8 indicadores FIES: 24→3 llamadas. Nota: `survey_mean` + `survey_total` no
  pueden ir en el mismo `summarise` por conflicto de nombres en el contexto de srvyr (output `r8a`
  pisa input `r8a`); se separaron en dos calls. Pivot largo final es solo reshape de datos.
  142/142 tests pasan. Cuantificación del speedup: pendiente resultado benchmark.

### PERF-4 — Costo de escritura Excel (`openxlsx`) [condicional al profiling de PERF-0]

- [ ] **Dónde:** `R/reporting.R` (~1066 líneas, ~144 llamadas `openxlsx`) y
  `setColWidths(widths = "auto")` / `addStyle` por rangos. `openxlsx` es R puro; `widths="auto"`
  es lento. En la **máquina Windows lenta** esto puede pesar bastante más que en la M1.
- [ ] **Condición:** abordar **solo si** PERF-0 muestra que `generate_*_report` domina
  (p. ej. >30% del total en `serie8_sig_xlsx`), y verificar el peso **en Windows**.
- [ ] **Experimentos (de menor a mayor esfuerzo):** (1) fijar anchos calculados una vez en vez de
  `widths = "auto"`; (2) agrupar `addStyle` con `gridExpand`/`stack` donde aún se estilan celdas
  sueltas; (3) evaluar migrar a **`openxlsx2`** (backend C++, API similar).
- [ ] **Decisión:** (1)–(2) son baratos, aceptar si ayudan. (3) es refactor grande de
  `reporting.R` → solo si (1)–(2) no bastan y el Excel sigue siendo el cuello de botella en
  Windows. Documentar la decisión.

### PERF-5 — Reducir copias/pasadas redundantes en el motor [impacto: bajo-medio, UNIVERSAL; verificar antes]

- [ ] **Dónde:** `R/engine.R:48-60` convierte `var`/`des` a factor sobre **todo**
  `dsgn$variables` y construye `base_df` (copia completa + 3 columnas) por llamada; `calc_tabla`
  re-filtra `base_df_loc` por combo cuando `rm_na_des`.
  **Hipótesis:** probablemente menor frente a `survey_*`, pero es trabajo de un solo hilo que
  pega igual en Windows. Verificar con el profiling antes de invertir tiempo.
- [ ] **Experimento:** si el profiling lo marca, reutilizar agrupaciones para
  `n_mues`/`N_pob`/`gl` y minimizar copias. Medir `anio1_3des_multi`.
- [ ] **Decisión:** aceptar **solo** si PERF-0 lo muestra en el perfil; si no, no tocar (riesgo de
  bug sin ganancia).

### PERF-6 — `survey_quantile` (documentar, no optimizar)

- [ ] `obs_cuantil` es intrínsecamente el más caro (la varianza del cuantil interpola la CDF). Sin
  optimización algorítmica propia razonable. **Acción:** documentar en la ayuda que con muchas
  combinaciones es el escenario más lento y sugerir `multi_des = FALSE` o reducir `des`. Es
  expectativa de usuario, no código.

### Priorización de rendimiento

| Ítem | Esfuerzo | Impacto | Plataforma donde ayuda | Depende de |
|---|---|---|---|---|
| PERF-0 harness + base | Bajo | habilitante | — | — |
| PERF-0b validación Windows | Bajo | habilitante (crítico) | — | PERF-0 |
| **PERF-3 vectorizar `multi_bin`** | Medio | **Alto** | **todas (incl. Windows)** | PERF-0 |
| PERF-1 paralelizar combos | Medio-Alto | Alto (single-year) | con núcleos libres | PERF-0/0b |
| PERF-2 fork vs socket | Bajo | Medio-Alto | solo macOS/Linux | PERF-0 |
| PERF-4 Excel/openxlsx | Variable | Condicional | todas (más en Windows) | PERF-0 (profiling) |
| PERF-5 copias del motor | Bajo | Bajo-Medio | todas | PERF-0 (profiling) |
| PERF-6 doc cuantiles | Trivial | expectativa | — | — |

**Ruta recomendada:** PERF-0 → PERF-0b → **PERF-3** (universal, ayuda al usuario Windows) →
PERF-1 (validado en Windows) → PERF-2 (Mac/Linux) → PERF-4/5 solo si el profiling lo justifica.

---

## Comandos de verificación

```r
devtools::document()                 # regenerar .Rd/NAMESPACE
devtools::test()                     # red de no-regresión (correr antes y después de cada cambio)
devtools::check(args = "--as-cran")
source("dev/bench_dosr.R")           # benchmark portable (M1 y Windows)
covr::package_coverage()             # cobertura (opcional)
pkgdown::build_site()                # sitio
```
