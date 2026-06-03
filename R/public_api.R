# ---------------------------------------------------------------------------- #
# Archivo: public_api.R
# ---------------------------------------------------------------------------- #

#' @title Internal helper function for a single worker process
#' @noRd
calculate_single_design <- function(dsgn, meta, var, des, filt, rm_na_var, rm_na_des = FALSE, type, multi_des, es_var_estudio, porcentaje, quantile_prob = 0.5, ratio_vars = NULL,
                                    cv_umbral_alto = 0.30, cv_umbral_medio = 0.20, n_minimo = 30, nivel_confianza = 0.95, universo_crit = FALSE, par_combos = FALSE) {
  calculate_estimates(
    dsgn = dsgn,
    var = var, des = des, filt = filt, rm_na_var = rm_na_var, rm_na_des = rm_na_des, type = type,
    psu_var = meta$psu, strata_var = meta$strata, weight_var = meta$weight,
    multi_des = multi_des,
    es_var_estudio = es_var_estudio,
    porcentaje = porcentaje,
    quantile_prob = quantile_prob,
    ratio_vars = ratio_vars,
    cv_umbral_alto = cv_umbral_alto,
    cv_umbral_medio = cv_umbral_medio,
    n_minimo = n_minimo,
    nivel_confianza = nivel_confianza,
    universo_crit = universo_crit,
    par_combos = par_combos
  )
}

# --- FUNCIÓN INTERNA PARA CREAR DISEÑOS LIGEROS ---
create_lightweight_designs <- function(design_list, main_var, des_vars, filter_vars, meta_list) {
  purrr::map2(design_list, meta_list, function(dsgn, meta) {
    design_specific_vars <- c(meta$psu, meta$strata, meta$weight)
    vars_to_keep <- unique(c(main_var, des_vars, filter_vars, design_specific_vars))
    vars_to_keep <- intersect(vars_to_keep, names(dsgn$variables))
    light_dsgn <- dsgn %>% srvyr::select(all_of(vars_to_keep))
    return(light_dsgn)
  })
}


#' @title Calcula estimaciones de proporciones para diseños complejos
#' @description Procesa uno o más `tbl_svy` para calcular proporciones.
#' @inheritParams obs_media
#' @param porcentaje Booleano. Si `TRUE`, las estimaciones y errores estándar se multiplican por 100.
#' @param categoria Vector de valores (labels o códigos numéricos) para filtrar las categorías de `var` a mostrar en el output. Las demás categorías se excluyen del resultado y del reporte Excel.
#' @return Un data.frame con los resultados consolidados (invisiblemente).
#' @examples
#' \donttest{
#' library(srvyr)
#' design_2022 <- as_survey_design(casen_2022, ids = varunit,
#'                                 strata = varstrat, weights = expr, nest = TRUE)
#' obs_prop(design_2022, sufijo = "2022", var = "pobreza",
#'          porcentaje = TRUE, save_xlsx = FALSE, verbose = FALSE)
#' }
#' @export
obs_prop <- function(designs,
                     sufijo             = NULL,
                     var,
                     des                = NULL,
                     multi_des          = TRUE,
                     es_var_estudio     = FALSE,
                     usar_etiqueta_var  = TRUE,
                     sig                = FALSE,
                     filt               = NULL,
                     rm_na_var          = TRUE,
                     rm_na_des          = FALSE,
                     parallel           = FALSE,
                     n_cores            = NULL,
                     save_xlsx          = TRUE,
                     dir                = "output",
                     formato            = TRUE,
                     porcentaje         = TRUE,
                     decimales          = 2,
                     nombre             = NULL,
                     fuente             = NULL,
                     snac               = FALSE,
                     mostrar_pct_fiable = FALSE,
                     color_fiabilidad   = FALSE,
                     universo_crit      = FALSE,
                     categoria          = NULL,
                     cv_umbral_alto     = 0.30,
                     cv_umbral_medio    = 0.20,
                     n_minimo           = 30L,
                     nivel_confianza    = 0.95,
                     verbose            = TRUE) {

  .guard_multi_des(des, multi_des)
  prep    <- .prepare_designs_list(designs, sufijo, verbose)
  designs <- prep$designs; sufijo <- prep$sufijo; n_designs <- prep$n_designs

  filt <- .resolve_filt(rlang::enquo(filt))
  validate_filt(filt)
  validate_inputs(designs[[1]], var, des)
  nombre_indicador <- .extract_var_label(designs, var, usar_etiqueta_var, nombre)

  ml       <- .build_meta_and_light(designs, var, des, filt)
  calc_fun <- .make_calc_fun(
    var, des, filt, rm_na_var, rm_na_des, "prop", multi_des, es_var_estudio,
    porcentaje      = porcentaje,
    cv_umbral_alto  = cv_umbral_alto,
    cv_umbral_medio = cv_umbral_medio,
    n_minimo        = n_minimo,
    nivel_confianza = nivel_confianza,
    universo_crit   = universo_crit,
    par_combos      = parallel && n_designs == 1L
  )
  lista_tablas <- .run_estimations(ml$light, ml$metadata, calc_fun,
                                   parallel, n_cores, n_designs, verbose)

  if (verbose) message("Fase 3/3: Agregando resultados y generando reporte Excel...")
  keys_prop  <- c(var, "nivel", des)
  hojas_list <- aggregate_results(lista_tablas, sufijo, keys = keys_prop,
                                  all_designs = designs, type = "prop")

  lista_tests <- NULL
  if (sig && formato) {
    if (verbose) message("... calculando pruebas de significancia...")
    lista_tests <- calculate_significance(hojas_list, sufijo, type = "prop",
                                          main_var_prop = var, des_vars = des)
  }

  if (!is.null(categoria)) {
    cat_chr    <- as.character(categoria)
    cat_num    <- suppressWarnings(as.integer(categoria))
    # Translate numeric codes to factor labels using the design's level ordering
    var_levels <- get_all_levels(designs, var)[[var]]
    num_labels <- if (!is.null(var_levels)) {
      idx <- cat_num[!is.na(cat_num) & cat_num >= 1L & cat_num <= length(var_levels)]
      var_levels[idx]
    } else character(0)
    all_cat_chr <- unique(c(cat_chr, num_labels))
    hojas_list  <- lapply(hojas_list, function(df) {
      df[as.character(df[[var]]) %in% all_cat_chr, ]
    })
  }

  resultado_final <- .finalize_results(hojas_list, keys_prop, designs, "prop", des,
                                       sort_extra = var)

  if (save_xlsx) {
    dir.create(dir, showWarnings = FALSE, recursive = TRUE)
    des_tag  <- if (!is.null(des)) paste(des, collapse = "-") else "nac"
    filename <- file.path(dir, paste0(var, "_", des_tag, "_", paste(sufijo, collapse = "-"), "_PROP.xlsx"))
    if (formato) {
      generate_prop_report(hojas_list, filename, var, des, sufijo, porcentaje, decimales,
                           designs, consolidated_df = resultado_final,
                           nombre_indicador = nombre_indicador, lista_tests = lista_tests,
                           snac = snac, mostrar_pct_fiable = mostrar_pct_fiable,
                           color_fiabilidad = color_fiabilidad, fuente = fuente)
    } else {
      .save_simple_xlsx(dir, filename, resultado_final)
    }
  }
  if (verbose) message("Proceso completado.")
  invisible(resultado_final)
}


#' @title Calcula estimaciones de medias para diseños complejos
#' @description Procesa uno o más `tbl_svy` para calcular medias.
#' @param designs Un objeto `tbl_svy` o una lista de ellos.
#' @param sufijo Vector de strings para sufijos (p.ej. c("2020","2022")).
#' @param var Un string con el nombre de la variable de interés (numérica).
#' @param des Un vector de strings con los nombres de las variables de desagregación.
#' @param multi_des Booleano. Si `TRUE` (por defecto), calcula todas las combinaciones de `des`. Si `FALSE`, solo calcula las desagregaciones simples.
#' @param es_var_estudio Booleano. Si `TRUE`, aplica criterios de fiabilidad menos estrictos para el tamaño muestral. Por defecto es `FALSE`.
#' @param usar_etiqueta_var Booleano. Si `TRUE` (por defecto), usa la etiqueta de la variable `var` como título en los reportes de Excel. Si es `FALSE` o la variable no tiene etiqueta, usa el nombre de la variable.
#' @param sig Booleano. Si `TRUE`, calcula y añade pruebas de significancia estadística a las hojas de reporte con formato. Por defecto es `FALSE`.
#' @param filt Expresión de filtro. Acepta tanto una expresión R sin comillas
#'   (`filt = edad > 18`) como un string (`filt = "edad > 18"`). Ambas formas
#'   son equivalentes y retrocompatibles.
#' @param rm_na_var Booleano. Si `TRUE`, elimina NAs en `var` antes de calcular.
#' @param rm_na_des Booleano. Si `TRUE`, excluye las observaciones con `NA` en las variables de desagregación correspondientes a
#'   cada tabla solicitada.
#' @param parallel Booleano. Activa el cálculo en paralelo. Con un único diseño
#'   y múltiples desagregaciones distribuye las combinaciones entre workers;
#'   con múltiples diseños distribuye los diseños. Por defecto `FALSE`.
#' @param n_cores Entero. Número de workers a usar. Si es `NULL`, se usa un valor seguro (máximo 4).
#' @param save_xlsx Booleano. Si `TRUE`, guarda un reporte en Excel.
#' @param dir Un string con la ruta del directorio donde se guardará el archivo Excel. Por defecto es `"output"`.
#' @param formato Booleano. Si `TRUE`, genera un reporte de Excel con formato avanzado.
#' @param decimales Entero. Número de decimales para las estimaciones en Excel.
#' @param nombre String. Nombre del indicador que se muestra en el reporte Excel. Si se especifica, sobreescribe la etiqueta de variable aunque `usar_etiqueta_var = TRUE`.
#' @param fuente String. Fuente de los datos para el pie del reporte Excel. Acepta claves estándar (`"casen"`, `"ebs"`, `"endide"`, `"eanna"`, `"elpi"`) o texto libre.
#' @param snac Booleano. Si `TRUE`, omite la hoja de formato del nivel nacional. El consolidado siempre incluye el nivel nacional. Por defecto `FALSE`.
#' @param mostrar_pct_fiable Booleano. Si `TRUE`, la nota de fiabilidad incluye el porcentaje de estimaciones fiables del cuadro. Por defecto `FALSE`.
#' @param color_fiabilidad Booleano. Si `TRUE`, colorea el texto de las celdas de estimación según su fiabilidad: ámbar para poco fiable, rojo para no fiable. Por defecto `FALSE`.
#' @param universo_crit Booleano. Solo aplica a `obs_prop`. Si `TRUE`, fuerza el uso del N total del dominio (suma de categorías) para el criterio muestral de fiabilidad, independientemente del número de categorías. Por defecto `FALSE` (comportamiento automático).
#' @param cv_umbral_alto Numérico. Umbral de CV para clasificar una estimación como "No Fiable (CV)". Por defecto `0.30`.
#' @param cv_umbral_medio Numérico. Umbral de CV para clasificar una estimación como "Poco Fiable (CV)". Por defecto `0.20`.
#' @param n_minimo Entero. Tamaño muestral mínimo para el criterio de fiabilidad. Por defecto `30`.
#' @param nivel_confianza Numérico. Nivel de confianza para intervalos y pruebas de significancia. Por defecto `0.95`.
#' @param verbose Booleano. Si `TRUE` (por defecto), muestra mensajes de progreso.
#' @return Un data.frame con los resultados consolidados (invisiblemente).
#' @examples
#' \donttest{
#' library(srvyr)
#' design_2022 <- as_survey_design(casen_2022, ids = varunit,
#'                                 strata = varstrat, weights = expr, nest = TRUE)
#' obs_media(design_2022, sufijo = "2022", var = "ytotcorh",
#'           save_xlsx = FALSE, verbose = FALSE)
#' }
#' @export
obs_media <- function(designs,
                      sufijo             = NULL,
                      var,
                      des                = NULL,
                      multi_des          = TRUE,
                      es_var_estudio     = FALSE,
                      usar_etiqueta_var  = TRUE,
                      sig                = FALSE,
                      filt               = NULL,
                      rm_na_var          = TRUE,
                      rm_na_des          = FALSE,
                      parallel           = FALSE,
                      n_cores            = NULL,
                      save_xlsx          = TRUE,
                      dir                = "output",
                      formato            = TRUE,
                      decimales          = 2,
                      nombre             = NULL,
                      fuente             = NULL,
                      snac               = FALSE,
                      mostrar_pct_fiable = FALSE,
                      color_fiabilidad   = FALSE,
                      universo_crit      = FALSE,
                      cv_umbral_alto     = 0.30,
                      cv_umbral_medio    = 0.20,
                      n_minimo           = 30L,
                      nivel_confianza    = 0.95,
                      verbose            = TRUE) {

  .guard_multi_des(des, multi_des)
  prep    <- .prepare_designs_list(designs, sufijo, verbose)
  designs <- prep$designs; sufijo <- prep$sufijo; n_designs <- prep$n_designs

  filt <- .resolve_filt(rlang::enquo(filt))
  validate_filt(filt)
  validate_inputs(designs[[1]], var, des)
  nombre_indicador <- .extract_var_label(designs, var, usar_etiqueta_var, nombre)

  ml       <- .build_meta_and_light(designs, var, des, filt)
  calc_fun <- .make_calc_fun(
    var, des, filt, rm_na_var, rm_na_des, "mean", multi_des, es_var_estudio,
    cv_umbral_alto  = cv_umbral_alto,
    cv_umbral_medio = cv_umbral_medio,
    n_minimo        = n_minimo,
    nivel_confianza = nivel_confianza,
    universo_crit   = universo_crit,
    par_combos      = parallel && n_designs == 1L
  )
  lista_tablas <- .run_estimations(ml$light, ml$metadata, calc_fun,
                                   parallel, n_cores, n_designs, verbose)

  if (verbose) message("Fase 3/3: Agregando resultados y generando reporte Excel...")
  keys_media <- c("variable", "nivel", des)
  hojas_list <- aggregate_results(lista_tablas, sufijo, keys = keys_media,
                                  all_designs = designs, type = "mean")

  lista_tests <- NULL
  if (sig && formato) {
    if (verbose) message("... calculando pruebas de significancia...")
    lista_tests <- calculate_significance(hojas_list, sufijo, type = "mean",
                                          main_var_prop = NULL, des_vars = des)
  }

  resultado_final <- .finalize_results(hojas_list, keys_media, designs, "mean", des)

  if (save_xlsx) {
    dir.create(dir, showWarnings = FALSE, recursive = TRUE)
    des_tag  <- if (!is.null(des)) paste(des, collapse = "-") else "nac"
    filename <- file.path(dir, paste0(var, "_", des_tag, "_", paste(sufijo, collapse = "-"), "_MEDIA.xlsx"))
    if (formato) {
      generate_mean_report(hojas_list, filename, var, des, sufijo, decimales,
                           designs = designs, consolidated_df = resultado_final,
                           nombre_indicador = nombre_indicador, lista_tests = lista_tests,
                           snac = snac, mostrar_pct_fiable = mostrar_pct_fiable,
                           color_fiabilidad = color_fiabilidad, fuente = fuente)
    } else {
      .save_simple_xlsx(dir, filename, resultado_final)
    }
  }
  if (verbose) message("Proceso completado.")
  invisible(resultado_final)
}


#' @title Calcula estimaciones de totales para diseños complejos
#' @description Procesa uno o más `tbl_svy` para calcular totales ponderados.
#' @inheritParams obs_media
#' @return Un data.frame con los resultados consolidados (invisiblemente).
#' @examples
#' \donttest{
#' library(srvyr)
#' design_2022 <- as_survey_design(casen_2022, ids = varunit,
#'                                 strata = varstrat, weights = expr, nest = TRUE)
#' obs_total(design_2022, sufijo = "2022", var = "ytotcorh",
#'           save_xlsx = FALSE, verbose = FALSE)
#' }
#' @export
obs_total <- function(designs,
                      sufijo             = NULL,
                      var,
                      des                = NULL,
                      multi_des          = TRUE,
                      es_var_estudio     = FALSE,
                      usar_etiqueta_var  = TRUE,
                      sig                = FALSE,
                      filt               = NULL,
                      rm_na_var          = TRUE,
                      rm_na_des          = FALSE,
                      parallel           = FALSE,
                      n_cores            = NULL,
                      save_xlsx          = TRUE,
                      dir                = "output",
                      formato            = TRUE,
                      decimales          = 2,
                      nombre             = NULL,
                      fuente             = NULL,
                      snac               = FALSE,
                      mostrar_pct_fiable = FALSE,
                      color_fiabilidad   = FALSE,
                      universo_crit      = FALSE,
                      cv_umbral_alto     = 0.30,
                      cv_umbral_medio    = 0.20,
                      n_minimo           = 30L,
                      nivel_confianza    = 0.95,
                      verbose            = TRUE) {

  .guard_multi_des(des, multi_des)
  prep    <- .prepare_designs_list(designs, sufijo, verbose)
  designs <- prep$designs; sufijo <- prep$sufijo; n_designs <- prep$n_designs

  filt <- .resolve_filt(rlang::enquo(filt))
  validate_filt(filt)
  validate_inputs(designs[[1]], var, des)
  nombre_indicador <- .extract_var_label(designs, var, usar_etiqueta_var, nombre)

  ml       <- .build_meta_and_light(designs, var, des, filt)
  calc_fun <- .make_calc_fun(
    var, des, filt, rm_na_var, rm_na_des, "total", multi_des, es_var_estudio,
    cv_umbral_alto  = cv_umbral_alto,
    cv_umbral_medio = cv_umbral_medio,
    n_minimo        = n_minimo,
    nivel_confianza = nivel_confianza,
    universo_crit   = universo_crit,
    par_combos      = parallel && n_designs == 1L
  )
  lista_tablas <- .run_estimations(ml$light, ml$metadata, calc_fun,
                                   parallel, n_cores, n_designs, verbose)

  if (verbose) message("Fase 3/3: Agregando resultados y generando reporte Excel...")
  keys_total <- c("variable", "nivel", des)
  hojas_list <- aggregate_results(lista_tablas, sufijo, keys = keys_total,
                                  all_designs = designs, type = "total")

  lista_tests <- NULL
  if (sig && formato) {
    if (verbose) message("... calculando pruebas de significancia...")
    lista_tests <- calculate_significance(hojas_list, sufijo, type = "total",
                                          main_var_prop = NULL, des_vars = des)
  }

  resultado_final <- .finalize_results(hojas_list, keys_total, designs, "total", des)

  if (save_xlsx) {
    dir.create(dir, showWarnings = FALSE, recursive = TRUE)
    des_tag  <- if (!is.null(des)) paste(des, collapse = "-") else "nac"
    filename <- file.path(dir, paste0(var, "_", des_tag, "_", paste(sufijo, collapse = "-"), "_TOTAL.xlsx"))
    if (formato) {
      generate_total_report(hojas_list, filename, var, des, sufijo, decimales,
                            designs = designs, consolidated_df = resultado_final,
                            nombre_indicador = nombre_indicador, lista_tests = lista_tests,
                            snac = snac, mostrar_pct_fiable = mostrar_pct_fiable,
                            color_fiabilidad = color_fiabilidad, fuente = fuente)
    } else {
      .save_simple_xlsx(dir, filename, resultado_final)
    }
  }
  if (verbose) message("Proceso completado.")
  invisible(resultado_final)
}


#' @title Calcula razones (ratios) para diseños complejos
#' @description Calcula la razón entre dos variables numéricas (numerador/denominador)
#' sobre uno o más objetos `tbl_svy`, con las mismas opciones de desagregación,
#' paralelización y reporte usadas en `obs_media`.
#'
#' @inheritParams obs_media
#' @param num String con el nombre de la variable **numerador**.
#' @param den String con el nombre de la variable **denominador**.
#' @param usar_etiqueta_var Booleano. Si `TRUE` (por defecto), usa las etiquetas de
#'   las variables `num` y `den` (si existen) como títulos y rótulos en los reportes
#'   de Excel; si no hay etiqueta disponible, usa el nombre de la variable.
#' @param rm_na_var Booleano. Si `TRUE`, elimina observaciones con `NA` en el
#'   **numerador o** el **denominador** antes de calcular la razón.
#'
#' @return Un `data.frame` con los resultados consolidados (invisiblemente).
#' @examples
#' \donttest{
#' library(srvyr)
#' library(dplyr)
#' design_2022 <- as_survey_design(casen_2022, ids = varunit,
#'                                 strata = varstrat, weights = expr, nest = TRUE)
#' design_2022$variables <- design_2022$variables %>%
#'   mutate(mujer  = as.integer(as.numeric(sexo) == 2),
#'          hombre = as.integer(as.numeric(sexo) == 1))
#' obs_ratio(design_2022, sufijo = "2022", num = "mujer", den = "hombre",
#'           save_xlsx = FALSE, verbose = FALSE)
#' }
#' @export
obs_ratio <- function(designs,
                      sufijo             = NULL,
                      num,
                      den,
                      des                = NULL,
                      multi_des          = TRUE,
                      es_var_estudio     = FALSE,
                      usar_etiqueta_var  = TRUE,
                      sig                = FALSE,
                      filt               = NULL,
                      rm_na_var          = TRUE,
                      rm_na_des          = FALSE,
                      parallel           = FALSE,
                      n_cores            = NULL,
                      save_xlsx          = TRUE,
                      dir                = "output",
                      formato            = TRUE,
                      decimales          = 2,
                      nombre             = NULL,
                      fuente             = NULL,
                      snac               = FALSE,
                      mostrar_pct_fiable = FALSE,
                      color_fiabilidad   = FALSE,
                      universo_crit      = FALSE,
                      cv_umbral_alto     = 0.30,
                      cv_umbral_medio    = 0.20,
                      n_minimo           = 30L,
                      nivel_confianza    = 0.95,
                      verbose            = TRUE) {

  stopifnot(
    "'num' debe ser un string no vac\u00edo" = is.character(num) && length(num) == 1 && nzchar(num),
    "'den' debe ser un string no vac\u00edo" = is.character(den) && length(den) == 1 && nzchar(den)
  )
  if (identical(num, den)) {
    stop("El numerador y el denominador deben ser variables distintas.", call. = FALSE)
  }

  .guard_multi_des(des, multi_des)
  prep    <- .prepare_designs_list(designs, sufijo, verbose)
  designs <- prep$designs; sufijo <- prep$sufijo; n_designs <- prep$n_designs

  filt <- .resolve_filt(rlang::enquo(filt))
  validate_filt(filt)
  validate_inputs(designs[[1]], c(num, den), des)

  ratio_nombre <- paste(num, den, sep = "/")
  num_display  <- num
  den_display  <- den
  if (usar_etiqueta_var) {
    num_label     <- labelled::var_label(designs[[1]]$variables[[num]])
    den_label     <- labelled::var_label(designs[[1]]$variables[[den]])
    num_label_chr <- if (!is.null(num_label)) as.character(num_label) else character(0)
    den_label_chr <- if (!is.null(den_label)) as.character(den_label) else character(0)
    if (length(num_label_chr) > 0 && nzchar(num_label_chr[1])) num_display <- num_label_chr[1]
    if (length(den_label_chr) > 0 && nzchar(den_label_chr[1])) den_display <- den_label_chr[1]
    ratio_nombre <- paste(num_display, den_display, sep = " / ")
  }
  nombre_indicador <- if (!is.null(nombre)) nombre else ratio_nombre

  ml       <- .build_meta_and_light(designs, unique(c(num, den)), des, filt)
  calc_fun <- .make_calc_fun(
    var         = nombre_indicador,
    des         = des,
    filt        = filt,
    rm_na_var   = rm_na_var,
    rm_na_des   = rm_na_des,
    type        = "ratio",
    multi_des   = multi_des,
    es_var_estudio = es_var_estudio,
    ratio_vars     = list(num = num, den = den),
    cv_umbral_alto  = cv_umbral_alto,
    cv_umbral_medio = cv_umbral_medio,
    n_minimo        = n_minimo,
    nivel_confianza = nivel_confianza,
    universo_crit   = universo_crit,
    par_combos      = parallel && n_designs == 1L
  )
  lista_tablas <- .run_estimations(ml$light, ml$metadata, calc_fun,
                                   parallel, n_cores, n_designs, verbose)

  if (verbose) message("Fase 3/3: Agregando resultados y generando reporte Excel...")
  keys_ratio <- c("variable", "nivel", des)
  hojas_list <- aggregate_results(lista_tablas, sufijo, keys = keys_ratio,
                                  all_designs = designs, type = "ratio")

  lista_tests <- NULL
  if (sig && formato) {
    if (verbose) message("... calculando pruebas de significancia...")
    lista_tests <- calculate_significance(hojas_list, sufijo, type = "ratio",
                                          main_var_prop = NULL, des_vars = des)
  }

  resultado_final <- .finalize_results(hojas_list, keys_ratio, designs, "ratio", des)

  if (save_xlsx) {
    dir.create(dir, showWarnings = FALSE, recursive = TRUE)
    des_tag   <- if (!is.null(des)) paste(des, collapse = "-") else "nac"
    ratio_tag <- paste(num, den, sep = "-")
    filename  <- file.path(dir, paste0(ratio_tag, "_", des_tag, "_", paste(sufijo, collapse = "-"), "_RATIO.xlsx"))
    if (formato) {
      generate_ratio_report(hojas_list, filename, nombre_indicador, des, sufijo, decimales,
                            designs = designs, consolidated_df = resultado_final,
                            nombre_indicador = nombre_indicador, lista_tests = lista_tests,
                            snac = snac, mostrar_pct_fiable = mostrar_pct_fiable,
                            color_fiabilidad = color_fiabilidad, fuente = fuente)
    } else {
      .save_simple_xlsx(dir, filename, resultado_final)
    }
  }
  if (verbose) message("Proceso completado.")
  invisible(resultado_final)
}


#' @title Calcula estimaciones de cuantiles para diseños complejos
#' @description Procesa uno o más `tbl_svy` para calcular cuantiles.
#' @inheritParams obs_media
#' @param cuant Probabilidad del cuantil a calcular. Debe estar entre 0 y 1. Por defecto `0.5` (mediana).
#' @return Un data.frame con los resultados consolidados (invisiblemente).
#' @examples
#' \donttest{
#' library(srvyr)
#' design_2022 <- as_survey_design(casen_2022, ids = varunit,
#'                                 strata = varstrat, weights = expr, nest = TRUE)
#' obs_cuantil(design_2022, sufijo = "2022", var = "ytotcorh", cuant = 0.5,
#'             save_xlsx = FALSE, verbose = FALSE)
#' }
#' @export
obs_cuantil <- function(designs,
                        sufijo             = NULL,
                        var,
                        cuant              = 0.5,
                        des                = NULL,
                        multi_des          = TRUE,
                        es_var_estudio     = FALSE,
                        usar_etiqueta_var  = TRUE,
                        sig                = FALSE,
                        filt               = NULL,
                        rm_na_var          = TRUE,
                        rm_na_des          = FALSE,
                        parallel           = FALSE,
                        n_cores            = NULL,
                        save_xlsx          = TRUE,
                        dir                = "output",
                        formato            = TRUE,
                        decimales          = 2,
                        nombre             = NULL,
                        fuente             = NULL,
                        snac               = FALSE,
                        mostrar_pct_fiable = FALSE,
                        color_fiabilidad   = FALSE,
                        universo_crit      = FALSE,
                        cv_umbral_alto     = 0.30,
                        cv_umbral_medio    = 0.20,
                        n_minimo           = 30L,
                        nivel_confianza    = 0.95,
                        verbose            = TRUE) {

  stopifnot(
    "'cuant' debe ser un n\u00famero" = is.numeric(cuant),
    "'cuant' debe tener longitud 1" = length(cuant) == 1,
    "'cuant' debe estar entre 0 y 1" = !is.na(cuant) && cuant >= 0 && cuant <= 1
  )

  .guard_multi_des(des, multi_des)
  prep    <- .prepare_designs_list(designs, sufijo, verbose)
  designs <- prep$designs; sufijo <- prep$sufijo; n_designs <- prep$n_designs

  filt <- .resolve_filt(rlang::enquo(filt))
  validate_filt(filt)
  validate_inputs(designs[[1]], var, des)
  nombre_indicador <- .extract_var_label(designs, var, usar_etiqueta_var, nombre)

  ml       <- .build_meta_and_light(designs, var, des, filt)
  calc_fun <- .make_calc_fun(
    var, des, filt, rm_na_var, rm_na_des, "quantile", multi_des, es_var_estudio,
    quantile_prob   = cuant,
    cv_umbral_alto  = cv_umbral_alto,
    cv_umbral_medio = cv_umbral_medio,
    n_minimo        = n_minimo,
    nivel_confianza = nivel_confianza,
    universo_crit   = universo_crit,
    par_combos      = parallel && n_designs == 1L
  )
  lista_tablas <- .run_estimations(ml$light, ml$metadata, calc_fun,
                                   parallel, n_cores, n_designs, verbose)

  if (verbose) message("Fase 3/3: Agregando resultados y generando reporte Excel...")
  keys_cuantil <- c("variable", "nivel", des)
  hojas_list   <- aggregate_results(lista_tablas, sufijo, keys = keys_cuantil,
                                    all_designs = designs, type = "quantile")

  lista_tests <- NULL
  if (sig && formato) {
    if (verbose) message("... calculando pruebas de significancia...")
    lista_tests <- calculate_significance(hojas_list, sufijo, type = "quantile",
                                          main_var_prop = NULL, des_vars = des)
  }

  resultado_final <- .finalize_results(hojas_list, keys_cuantil, designs, "quantile", des)

  if (save_xlsx) {
    dir.create(dir, showWarnings = FALSE, recursive = TRUE)
    des_tag   <- if (!is.null(des)) paste(des, collapse = "-") else "nac"
    cuant_tag <- gsub("[^0-9A-Za-z]", "_", formatC(cuant, format = "f", digits = 2))
    filename  <- file.path(dir, paste0(var, "_", des_tag, "_", paste(sufijo, collapse = "-"),
                                       "_CUANTIL_", cuant_tag, ".xlsx"))
    if (formato) {
      generate_quantile_report(hojas_list, filename, var, des, sufijo, cuant, decimales,
                               designs = designs, consolidated_df = resultado_final,
                               nombre_indicador = nombre_indicador, lista_tests = lista_tests,
                               snac = snac, mostrar_pct_fiable = mostrar_pct_fiable,
                               color_fiabilidad = color_fiabilidad, fuente = fuente)
    } else {
      .save_simple_xlsx(dir, filename, resultado_final)
    }
  }
  if (verbose) message("Proceso completado.")
  invisible(resultado_final)
}
