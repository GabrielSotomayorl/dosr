# ---------------------------------------------------------------------------- #
# Archivo: public_api.R (VERSIÓN CON ARGUMENTO 'dir' PERSONALIZABLE)
# ---------------------------------------------------------------------------- #

#' @title Internal helper function for a single worker process
#' @noRd
calculate_single_design <- function(dsgn, meta, var, des, filt, rm_na_var, type, multi_des, es_var_estudio, porcentaje, quantile_prob = 0.5, ratio_vars = NULL) {
  calculate_estimates(
    dsgn = dsgn,
    var = var, des = des, filt = filt, rm_na_var = rm_na_var, type = type,
    psu_var = meta$psu, strata_var = meta$strata, weight_var = meta$weight,
    multi_des = multi_des,
    es_var_estudio = es_var_estudio,
    porcentaje = porcentaje,
    quantile_prob = quantile_prob,
    ratio_vars = ratio_vars
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
#' @return Un data.frame con los resultados consolidados (invisiblemente).
#' @export
obs_prop <- function(designs,
                     sufijo            = NULL,
                     var,
                     des               = NULL,
                     multi_des         = TRUE,
                     es_var_estudio    = FALSE,
                     usar_etiqueta_var = TRUE,
                     sig               = FALSE,
                     filt              = NULL,
                     rm_na_var         = TRUE,
                     parallel          = FALSE,
                     n_cores           = NULL,
                     save_xlsx         = TRUE,
                     dir               = "output", # <--- NUEVO ARGUMENTO
                     formato           = TRUE,
                     porcentaje        = TRUE,
                     decimales         = 2,
                     verbose           = TRUE) {

  if (multi_des && length(des) > 3) {
    stop(paste(
      "Se solicitaron", length(des), "variables de desagregaci\u00f3n con 'multi_des = TRUE'.",
      "Esto generar\u00eda", 2^length(des) - 1, "combinaciones y ser\u00eda extremadamente lento.",
      "\nSoluci\u00f3n: Use 3 o menos variables en 'des', o establezca 'multi_des = FALSE' para obtener solo las desagregaciones simples."
    ), call. = FALSE)
  }
  if (verbose) message("Fase 1/3: Preparando datos y dise\u00f1os...")
  if (!inherits(designs, "list")) designs <- list(designs)
  n_designs <- length(designs)
  if (is.null(sufijo)) {
    sufijo <- if (!is.null(names(designs)) && all(nzchar(names(designs)))) names(designs) else as.character(seq_len(n_designs))
  }
  stopifnot("La longitud de 'sufijo' debe coincidir con el n\u00famero de dise\u00f1os." = length(sufijo) == n_designs)
  names(designs) <- sufijo
  nombre_indicador <- var
  if (usar_etiqueta_var) {
    var_label <- labelled::var_label(designs[[1]]$variables[[var]])
    if (!is.null(var_label)) {
      nombre_indicador <- var_label
    }
  }
  design_metadata <- purrr::map(designs, ~list(psu=colnames(.x$cluster)[1], strata=colnames(.x$strata)[1], weight=names(.x$allprob)[1]))
  vars_in_filter <- if (!is.null(filt)) all.vars(rlang::parse_expr(filt)) else character(0)
  designs_light <- create_lightweight_designs(designs, var, des, vars_in_filter, design_metadata)

  if (verbose) message("Fase 2/3: Calculando estimaciones... (Esta etapa puede tardar varios minutos)")
  pmap_args <- list(dsgn = designs_light, meta = design_metadata)
  calc_fun <- function(dsgn, meta) {
    calculate_single_design(dsgn, meta, var, des, filt, rm_na_var, "prop", multi_des, es_var_estudio, porcentaje)
  }

  if (parallel && n_designs > 1) {
    max_safe_cores <- 4
    cores_detected <- tryCatch(parallel::detectCores(), error = function(e) 2)
    if (is.null(n_cores)) {
      cores <- min(cores_detected - 1, max_safe_cores, n_designs)
      cores <- max(1, cores)
    } else { cores <- n_cores }
    if (verbose) message(paste("... usando modo paralelo con", cores, "workers."))
    old_plan <- future::plan(multisession, workers = cores)
    on.exit(future::plan(old_plan), add = TRUE)
    lista_tablas <- furrr::future_pmap(pmap_args, calc_fun, .progress = FALSE)
  } else {
    lista_tablas <- purrr::pmap(pmap_args, calc_fun)
  }

  if (verbose) message("Fase 3/3: Agregando resultados y generando reporte Excel...")
  keys_prop <- c(var, "nivel", des)
  hojas_list <- aggregate_results(lista_tablas, sufijo, keys = keys_prop, all_designs = designs, type = "prop")

  lista_tests <- NULL
  if (sig && formato) {
    if (verbose) message("... calculando pruebas de significancia...")
    lista_tests <- calculate_significance(hojas_list, sufijo, type = "prop", main_var_prop = var, des_vars = des)
  }

  all_factor_levels <- get_all_levels(designs, c(var, des))
  resultado_final_unordered <- bind_rows(hojas_list, .id = "combo_maestro") %>% unique_cols()
  resultado_final_unordered$combo_maestro <- factor(resultado_final_unordered$combo_maestro, levels = names(hojas_list))
  for(col_name in names(all_factor_levels)) {
    if(col_name %in% names(resultado_final_unordered)) {
      resultado_final_unordered[[col_name]] <- factor(resultado_final_unordered[[col_name]], levels = all_factor_levels[[col_name]])
    }
  }
  resultado_final <- resultado_final_unordered %>%
    arrange(.data$combo_maestro, across(any_of(c(des, var)))) %>%
    select(-.data$combo_maestro) %>%
    select(any_of(keys_prop), everything())

  if (save_xlsx) {
    # ## MODIFICACIÓN ##: Usar el argumento 'dir'
    dir.create(dir, showWarnings = FALSE, recursive = TRUE)
    des_tag <- if (!is.null(des)) paste(des, collapse = "-") else "nac"
    filename <- file.path(dir, paste0(var, "_", des_tag, "_", paste(sufijo, collapse = "-"), "_PROP.xlsx"))

    if (formato) {
      generate_prop_report(hojas_list, filename, var, des, sufijo, porcentaje, decimales, designs, consolidated_df = resultado_final, nombre_indicador = nombre_indicador, lista_tests = lista_tests)
    } else {
      wb <- createWorkbook()
      addWorksheet(wb, "Consolidado")
      write_clean_table(wb, "Consolidado", resultado_final, startRow = 1, startCol = 1)
      saveWorkbook(wb, filename, overwrite = TRUE)
      message("Reporte Excel simple creado en: ", filename)
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
#' @param filt Un string con una expresión de filtro para `dplyr::filter()`.
#' @param rm_na_var Booleano. Si `TRUE`, elimina NAs en `var` antes de calcular.
#' @param parallel Booleano. Activa el cálculo en paralelo.
#' @param n_cores Entero. Número de núcleos a usar. Si es NULL, se usa un valor seguro.
#' @param save_xlsx Booleano. Si `TRUE`, guarda un reporte en Excel.
#' @param dir Un string con la ruta del directorio donde se guardará el archivo Excel. Por defecto es `"output"`.
#' @param formato Booleano. Si `TRUE`, genera un reporte de Excel con formato avanzado.
#' @param decimales Entero. Número de decimales para las estimaciones en Excel.
#' @param verbose Booleano. Si `TRUE` (por defecto), muestra mensajes de progreso.
#' @return Un data.frame con los resultados consolidados (invisiblemente).
#' @export
obs_media <- function(designs,
                      sufijo            = NULL,
                      var,
                      des               = NULL,
                      multi_des         = TRUE,
                      es_var_estudio    = FALSE,
                      usar_etiqueta_var = TRUE,
                      sig               = FALSE,
                      filt              = NULL,
                      rm_na_var         = TRUE,
                      parallel          = FALSE,
                      n_cores           = NULL,
                      save_xlsx         = TRUE,
                      dir               = "output", # <--- NUEVO ARGUMENTO
                      formato           = TRUE,
                      decimales         = 2,
                      verbose           = TRUE) {

  if (multi_des && length(des) > 3) {
    stop(paste(
      "Se solicitaron", length(des), "variables de desagregaci\u00f3n con 'multi_des = TRUE'.",
      "Esto generar\u00eda", 2^length(des) - 1, "combinaciones y ser\u00eda extremadamente lento.",
      "\nSoluci\u00f3n: Use 3 o menos variables en 'des', o establezca 'multi_des = FALSE' para obtener solo las desagregaciones simples."
    ), call. = FALSE)
  }

  if (verbose) message("Fase 1/3: Preparando datos y dise\u00f1os...")
  if (!inherits(designs, "list")) designs <- list(designs)
  n_designs <- length(designs)
  if (is.null(sufijo)) {
    sufijo <- if (!is.null(names(designs)) && all(nzchar(names(designs)))) names(designs) else as.character(seq_len(n_designs))
  }
  stopifnot("La longitud de 'sufijo' debe coincidir con el n\u00famero de dise\u00f1os." = length(sufijo) == n_designs)
  names(designs) <- sufijo

  nombre_indicador <- var
  if (usar_etiqueta_var) {
    var_label <- labelled::var_label(designs[[1]]$variables[[var]])
    if (!is.null(var_label)) {
      nombre_indicador <- var_label
    }
  }

  design_metadata <- purrr::map(designs, ~list(psu=colnames(.x$cluster)[1], strata=colnames(.x$strata)[1], weight=names(.x$allprob)[1]))
  vars_in_filter <- if (!is.null(filt)) all.vars(rlang::parse_expr(filt)) else character(0)
  designs_light <- create_lightweight_designs(designs, var, des, vars_in_filter, design_metadata)

  if (verbose) message("Fase 2/3: Calculando estimaciones... (Esta etapa puede tardar varios minutos)")
  pmap_args <- list(dsgn = designs_light, meta = design_metadata)

  calc_fun <- function(dsgn, meta) {
    calculate_single_design(dsgn, meta, var, des, filt, rm_na_var, "mean", multi_des, es_var_estudio, porcentaje = FALSE)
  }

  if (parallel && n_designs > 1) {
    max_safe_cores <- 4
    cores_detected <- tryCatch(parallel::detectCores(), error = function(e) 2)
    if (is.null(n_cores)) {
      cores <- min(cores_detected - 1, max_safe_cores, n_designs)
      cores <- max(1, cores)
    } else { cores <- n_cores }
    if (verbose) message(paste("... usando modo paralelo con", cores, "workers."))
    old_plan <- future::plan(multisession, workers = cores)
    on.exit(future::plan(old_plan), add = TRUE)
    lista_tablas <- furrr::future_pmap(pmap_args, calc_fun, .progress = FALSE)
  } else {
    lista_tablas <- purrr::pmap(pmap_args, calc_fun)
  }

  if (verbose) message("Fase 3/3: Agregando resultados y generando reporte Excel...")
  keys_media <- c("variable", "nivel", des)
  hojas_list <- aggregate_results(lista_tablas, sufijo, keys = keys_media, all_designs = designs, type = "mean")

  lista_tests <- NULL
  if (sig && formato) {
    if (verbose) message("... calculando pruebas de significancia...")
    lista_tests <- calculate_significance(hojas_list, sufijo, type = "mean", main_var_prop = NULL, des_vars = des)
  }

  all_factor_levels <- get_all_levels(designs, des)
  resultado_final_unordered <- bind_rows(hojas_list, .id = "combo_maestro") %>% unique_cols()
  resultado_final_unordered$combo_maestro <- factor(resultado_final_unordered$combo_maestro, levels = names(hojas_list))
  for(col_name in names(all_factor_levels)) {
    if(col_name %in% names(resultado_final_unordered)) {
      resultado_final_unordered[[col_name]] <- factor(resultado_final_unordered[[col_name]], levels = all_factor_levels[[col_name]])
    }
  }
  resultado_final <- resultado_final_unordered %>%
    arrange(.data$combo_maestro, across(any_of(des))) %>%
    select(-.data$combo_maestro) %>%
    select(any_of(keys_media), everything())

  if (save_xlsx) {
    # ## MODIFICACIÓN ##: Usar el argumento 'dir'
    dir.create(dir, showWarnings = FALSE, recursive = TRUE)
    des_tag <- if (!is.null(des)) paste(des, collapse = "-") else "nac"
    filename <- file.path(dir, paste0(var, "_", des_tag, "_", paste(sufijo, collapse = "-"), "_MEDIA.xlsx"))

    if (formato) {
      generate_mean_report(hojas_list, filename, var, des, sufijo, decimales, designs = designs, consolidated_df = resultado_final, nombre_indicador = nombre_indicador, lista_tests = lista_tests)
    } else {
      wb <- createWorkbook()
      addWorksheet(wb, "Consolidado")
      write_clean_table(wb, "Consolidado", resultado_final, startRow = 1, startCol = 1)
      saveWorkbook(wb, filename, overwrite = TRUE)
      message("Reporte Excel simple creado en: ", filename)
    }
  }
  if (verbose) message("Proceso completado.")
  invisible(resultado_final)
}


#' @title Calcula estimaciones de totales para diseños complejos
#' @description Procesa uno o más `tbl_svy` para calcular totales ponderados.
#' @inheritParams obs_media
#' @return Un data.frame con los resultados consolidados (invisiblemente).
#' @export
obs_total <- function(designs,
                      sufijo            = NULL,
                      var,
                      des               = NULL,
                      multi_des         = TRUE,
                      es_var_estudio    = FALSE,
                      usar_etiqueta_var = TRUE,
                      sig               = FALSE,
                      filt              = NULL,
                      rm_na_var         = TRUE,
                      parallel          = FALSE,
                      n_cores           = NULL,
                      save_xlsx         = TRUE,
                      dir               = "output",
                      formato           = TRUE,
                      decimales         = 2,
                      verbose           = TRUE) {

  if (multi_des && length(des) > 3) {
    stop(paste(
      "Se solicitaron", length(des), "variables de desagregación con 'multi_des = TRUE'.",
      "Esto generaría", 2^length(des) - 1, "combinaciones y sería extremadamente lento.",
      "\nSolución: Use 3 o menos variables en 'des', o establezca 'multi_des = FALSE' para obtener solo las desagregaciones simples."
    ), call. = FALSE)
  }

  if (verbose) message("Fase 1/3: Preparando datos y diseños...")
  if (!inherits(designs, "list")) designs <- list(designs)
  n_designs <- length(designs)
  if (is.null(sufijo)) {
    sufijo <- if (!is.null(names(designs)) && all(nzchar(names(designs)))) names(designs) else as.character(seq_len(n_designs))
  }
  stopifnot("La longitud de 'sufijo' debe coincidir con el número de diseños." = length(sufijo) == n_designs)
  names(designs) <- sufijo

  nombre_indicador <- var
  if (usar_etiqueta_var) {
    var_label <- labelled::var_label(designs[[1]]$variables[[var]])
    if (!is.null(var_label)) {
      nombre_indicador <- var_label
    }
  }

  design_metadata <- purrr::map(designs, ~list(psu=colnames(.x$cluster)[1], strata=colnames(.x$strata)[1], weight=names(.x$allprob)[1]))
  vars_in_filter <- if (!is.null(filt)) all.vars(rlang::parse_expr(filt)) else character(0)
  designs_light <- create_lightweight_designs(designs, var, des, vars_in_filter, design_metadata)

  if (verbose) message("Fase 2/3: Calculando estimaciones... (Esta etapa puede tardar varios minutos)")
  pmap_args <- list(dsgn = designs_light, meta = design_metadata)
  calc_fun <- function(dsgn, meta) {
    calculate_single_design(dsgn, meta, var, des, filt, rm_na_var, "total", multi_des, es_var_estudio, porcentaje = FALSE)
  }

  if (parallel && n_designs > 1) {
    max_safe_cores <- 4
    cores_detected <- tryCatch(parallel::detectCores(), error = function(e) 2)
    if (is.null(n_cores)) {
      cores <- min(cores_detected - 1, max_safe_cores, n_designs)
      cores <- max(1, cores)
    } else { cores <- n_cores }
    if (verbose) message(paste("... usando modo paralelo con", cores, "workers."))
    old_plan <- future::plan(multisession, workers = cores)
    on.exit(future::plan(old_plan), add = TRUE)
    lista_tablas <- furrr::future_pmap(pmap_args, calc_fun, .progress = FALSE)
  } else {
    lista_tablas <- purrr::pmap(pmap_args, calc_fun)
  }

  if (verbose) message("Fase 3/3: Agregando resultados y generando reporte Excel...")
  keys_total <- c("variable", "nivel", des)
  hojas_list <- aggregate_results(lista_tablas, sufijo, keys = keys_total, all_designs = designs, type = "total")

  lista_tests <- NULL
  if (sig && formato) {
    if (verbose) message("... calculando pruebas de significancia...")
    lista_tests <- calculate_significance(hojas_list, sufijo, type = "total", main_var_prop = NULL, des_vars = des)
  }

  all_factor_levels <- get_all_levels(designs, des)
  resultado_final_unordered <- bind_rows(hojas_list, .id = "combo_maestro") %>% unique_cols()
  resultado_final_unordered$combo_maestro <- factor(resultado_final_unordered$combo_maestro, levels = names(hojas_list))
  for(col_name in names(all_factor_levels)) {
    if(col_name %in% names(resultado_final_unordered)) {
      resultado_final_unordered[[col_name]] <- factor(resultado_final_unordered[[col_name]], levels = all_factor_levels[[col_name]])
    }
  }
  resultado_final <- resultado_final_unordered %>%
    arrange(.data$combo_maestro, across(any_of(des))) %>%
    select(-.data$combo_maestro) %>%
    select(any_of(keys_total), everything())

  if (save_xlsx) {
    dir.create(dir, showWarnings = FALSE, recursive = TRUE)
    des_tag <- if (!is.null(des)) paste(des, collapse = "-") else "nac"
    filename <- file.path(dir, paste0(var, "_", des_tag, "_", paste(sufijo, collapse = "-"), "_TOTAL.xlsx"))

    if (formato) {
      generate_total_report(hojas_list, filename, var, des, sufijo, decimales, designs = designs, consolidated_df = resultado_final, nombre_indicador = nombre_indicador, lista_tests = lista_tests)
    } else {
      wb <- createWorkbook()
      addWorksheet(wb, "Consolidado")
      write_clean_table(wb, "Consolidado", resultado_final, startRow = 1, startCol = 1)
      saveWorkbook(wb, filename, overwrite = TRUE)
      message("Reporte Excel simple creado en: ", filename)
    }
  }
  if (verbose) message("Proceso completado.")
  invisible(resultado_final)
}


#' @title Calcula razones (ratios) para diseños complejos
#' @description Procesa uno o más `tbl_svy` para calcular razones entre dos variables numéricas.
#' @inheritParams obs_media params = c("designs", "sufijo", "des", "multi_des", "es_var_estudio", "usar_etiqueta_var", "sig", "filt", "rm_na_var", "parallel", "n_cores", "save_xlsx", "dir", "formato", "decimales", "verbose")
#' @param num String con el nombre de la variable numerador.
#' @param den String con el nombre de la variable denominador.
#' @return Un data.frame con los resultados consolidados (invisiblemente).
#' @export
obs_ratio <- function(designs,
                      sufijo            = NULL,
                      num,
                      den,
                      des               = NULL,
                      multi_des         = TRUE,
                      es_var_estudio    = FALSE,
                      usar_etiqueta_var = TRUE,
                      sig               = FALSE,
                      filt              = NULL,
                      rm_na_var         = TRUE,
                      parallel          = FALSE,
                      n_cores           = NULL,
                      save_xlsx         = TRUE,
                      dir               = "output",
                      formato           = TRUE,
                      decimales         = 2,
                      verbose           = TRUE) {

  stopifnot(
    "'num' debe ser un string no vacío" = is.character(num) && length(num) == 1 && nzchar(num),
    "'den' debe ser un string no vacío" = is.character(den) && length(den) == 1 && nzchar(den)
  )

  if (identical(num, den)) {
    stop("El numerador y el denominador deben ser variables distintas.", call. = FALSE)
  }

  if (multi_des && length(des) > 3) {
    stop(paste(
      "Se solicitaron", length(des), "variables de desagregación con 'multi_des = TRUE'.",
      "Esto generaría", 2^length(des) - 1, "combinaciones y sería extremadamente lento.",
      "\nSolución: Use 3 o menos variables en 'des', o establezca 'multi_des = FALSE' para obtener solo las desagregaciones simples."
    ), call. = FALSE)
  }

  if (verbose) message("Fase 1/3: Preparando datos y diseños...")
  if (!inherits(designs, "list")) designs <- list(designs)
  n_designs <- length(designs)
  if (is.null(sufijo)) {
    sufijo <- if (!is.null(names(designs)) && all(nzchar(names(designs)))) names(designs) else as.character(seq_len(n_designs))
  }
  stopifnot("La longitud de 'sufijo' debe coincidir con el número de diseños." = length(sufijo) == n_designs)
  names(designs) <- sufijo

  ratio_nombre <- paste(num, den, sep = "/")
  num_display <- num
  den_display <- den
  if (usar_etiqueta_var) {
    num_label <- labelled::var_label(designs[[1]]$variables[[num]])
    den_label <- labelled::var_label(designs[[1]]$variables[[den]])
    num_label_chr <- if (!is.null(num_label)) as.character(num_label) else character(0)
    den_label_chr <- if (!is.null(den_label)) as.character(den_label) else character(0)
    if (length(num_label_chr) > 0 && nzchar(num_label_chr[1])) num_display <- num_label_chr[1]
    if (length(den_label_chr) > 0 && nzchar(den_label_chr[1])) den_display <- den_label_chr[1]
    ratio_nombre <- paste(num_display, den_display, sep = " / ")
  }
  nombre_indicador <- ratio_nombre

  design_metadata <- purrr::map(designs, ~list(psu=colnames(.x$cluster)[1], strata=colnames(.x$strata)[1], weight=names(.x$allprob)[1]))
  vars_in_filter <- if (!is.null(filt)) all.vars(rlang::parse_expr(filt)) else character(0)
  main_vars <- unique(c(num, den))
  designs_light <- create_lightweight_designs(designs, main_vars, des, vars_in_filter, design_metadata)

  if (verbose) message("Fase 2/3: Calculando estimaciones... (Esta etapa puede tardar varios minutos)")
  pmap_args <- list(dsgn = designs_light, meta = design_metadata)
  calc_fun <- function(dsgn, meta) {
    calculate_single_design(
      dsgn, meta,
      var = nombre_indicador,
      des = des,
      filt = filt,
      rm_na_var = rm_na_var,
      type = "ratio",
      multi_des = multi_des,
      es_var_estudio = es_var_estudio,
      porcentaje = FALSE,
      ratio_vars = list(num = num, den = den)
    )
  }

  if (parallel && n_designs > 1) {
    max_safe_cores <- 4
    cores_detected <- tryCatch(parallel::detectCores(), error = function(e) 2)
    if (is.null(n_cores)) {
      cores <- min(cores_detected - 1, max_safe_cores, n_designs)
      cores <- max(1, cores)
    } else { cores <- n_cores }
    if (verbose) message(paste("... usando modo paralelo con", cores, "workers."))
    old_plan <- future::plan(multisession, workers = cores)
    on.exit(future::plan(old_plan), add = TRUE)
    lista_tablas <- furrr::future_pmap(pmap_args, calc_fun, .progress = FALSE)
  } else {
    lista_tablas <- purrr::pmap(pmap_args, calc_fun)
  }

  if (verbose) message("Fase 3/3: Agregando resultados y generando reporte Excel...")
  keys_ratio <- c("variable", "nivel", des)
  hojas_list <- aggregate_results(lista_tablas, sufijo, keys = keys_ratio, all_designs = designs, type = "ratio")

  lista_tests <- NULL
  if (sig && formato) {
    if (verbose) message("... calculando pruebas de significancia...")
    lista_tests <- calculate_significance(hojas_list, sufijo, type = "ratio", main_var_prop = NULL, des_vars = des)
  }

  all_factor_levels <- get_all_levels(designs, des)
  resultado_final_unordered <- bind_rows(hojas_list, .id = "combo_maestro") %>% unique_cols()
  resultado_final_unordered$combo_maestro <- factor(resultado_final_unordered$combo_maestro, levels = names(hojas_list))
  for(col_name in names(all_factor_levels)) {
    if(col_name %in% names(resultado_final_unordered)) {
      resultado_final_unordered[[col_name]] <- factor(resultado_final_unordered[[col_name]], levels = all_factor_levels[[col_name]])
    }
  }
  resultado_final <- resultado_final_unordered %>%
    arrange(.data$combo_maestro, across(any_of(des))) %>%
    select(-.data$combo_maestro) %>%
    select(any_of(keys_ratio), everything())

  if (save_xlsx) {
    dir.create(dir, showWarnings = FALSE, recursive = TRUE)
    des_tag <- if (!is.null(des)) paste(des, collapse = "-") else "nac"
    ratio_tag <- paste(num, den, sep = "-")
    filename <- file.path(dir, paste0(ratio_tag, "_", des_tag, "_", paste(sufijo, collapse = "-"), "_RATIO.xlsx"))

    if (formato) {
      generate_ratio_report(hojas_list, filename, nombre_indicador, des, sufijo, decimales, designs = designs, consolidated_df = resultado_final, nombre_indicador = nombre_indicador, lista_tests = lista_tests)
    } else {
      wb <- createWorkbook()
      addWorksheet(wb, "Consolidado")
      write_clean_table(wb, "Consolidado", resultado_final, startRow = 1, startCol = 1)
      saveWorkbook(wb, filename, overwrite = TRUE)
      message("Reporte Excel simple creado en: ", filename)
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
#' @export
obs_cuantil <- function(designs,
                        sufijo            = NULL,
                        var,
                        cuant             = 0.5,
                        des               = NULL,
                        multi_des         = TRUE,
                        es_var_estudio    = FALSE,
                        usar_etiqueta_var = TRUE,
                        sig               = FALSE,
                        filt              = NULL,
                        rm_na_var         = TRUE,
                        parallel          = FALSE,
                        n_cores           = NULL,
                        save_xlsx         = TRUE,
                        dir               = "output",
                        formato           = TRUE,
                        decimales         = 2,
                        verbose           = TRUE) {

  stopifnot(
    "'cuant' debe ser un número" = is.numeric(cuant),
    "'cuant' debe tener longitud 1" = length(cuant) == 1,
    "'cuant' debe estar entre 0 y 1" = !is.na(cuant) && cuant >= 0 && cuant <= 1
  )

  if (multi_des && length(des) > 3) {
    stop(paste(
      "Se solicitaron", length(des), "variables de desagregación con 'multi_des = TRUE'.",
      "Esto generaría", 2^length(des) - 1, "combinaciones y sería extremadamente lento.",
      "\nSolución: Use 3 o menos variables en 'des', o establezca 'multi_des = FALSE' para obtener solo las desagregaciones simples."
    ), call. = FALSE)
  }

  if (verbose) message("Fase 1/3: Preparando datos y diseños...")
  if (!inherits(designs, "list")) designs <- list(designs)
  n_designs <- length(designs)
  if (is.null(sufijo)) {
    sufijo <- if (!is.null(names(designs)) && all(nzchar(names(designs)))) names(designs) else as.character(seq_len(n_designs))
  }
  stopifnot("La longitud de 'sufijo' debe coincidir con el número de diseños." = length(sufijo) == n_designs)
  names(designs) <- sufijo

  nombre_indicador <- var
  if (usar_etiqueta_var) {
    var_label <- labelled::var_label(designs[[1]]$variables[[var]])
    if (!is.null(var_label)) {
      nombre_indicador <- var_label
    }
  }

  design_metadata <- purrr::map(designs, ~list(psu=colnames(.x$cluster)[1], strata=colnames(.x$strata)[1], weight=names(.x$allprob)[1]))
  vars_in_filter <- if (!is.null(filt)) all.vars(rlang::parse_expr(filt)) else character(0)
  designs_light <- create_lightweight_designs(designs, var, des, vars_in_filter, design_metadata)

  if (verbose) message("Fase 2/3: Calculando estimaciones... (Esta etapa puede tardar varios minutos)")
  pmap_args <- list(dsgn = designs_light, meta = design_metadata)

  calc_fun <- function(dsgn, meta) {
    calculate_single_design(dsgn, meta, var, des, filt, rm_na_var, "quantile", multi_des, es_var_estudio, porcentaje = FALSE, quantile_prob = cuant)
  }

  if (parallel && n_designs > 1) {
    max_safe_cores <- 4
    cores_detected <- tryCatch(parallel::detectCores(), error = function(e) 2)
    if (is.null(n_cores)) {
      cores <- min(cores_detected - 1, max_safe_cores, n_designs)
      cores <- max(1, cores)
    } else { cores <- n_cores }
    if (verbose) message(paste("... usando modo paralelo con", cores, "workers."))
    old_plan <- future::plan(multisession, workers = cores)
    on.exit(future::plan(old_plan), add = TRUE)
    lista_tablas <- furrr::future_pmap(pmap_args, calc_fun, .progress = FALSE)
  } else {
    lista_tablas <- purrr::pmap(pmap_args, calc_fun)
  }

  if (verbose) message("Fase 3/3: Agregando resultados y generando reporte Excel...")
  keys_cuantil <- c("variable", "nivel", des)
  hojas_list <- aggregate_results(lista_tablas, sufijo, keys = keys_cuantil, all_designs = designs, type = "quantile")

  lista_tests <- NULL
  if (sig && formato) {
    if (verbose) message("... calculando pruebas de significancia...")
    lista_tests <- calculate_significance(hojas_list, sufijo, type = "quantile", main_var_prop = NULL, des_vars = des)
  }

  all_factor_levels <- get_all_levels(designs, des)
  resultado_final_unordered <- bind_rows(hojas_list, .id = "combo_maestro") %>% unique_cols()
  resultado_final_unordered$combo_maestro <- factor(resultado_final_unordered$combo_maestro, levels = names(hojas_list))
  for(col_name in names(all_factor_levels)) {
    if(col_name %in% names(resultado_final_unordered)) {
      resultado_final_unordered[[col_name]] <- factor(resultado_final_unordered[[col_name]], levels = all_factor_levels[[col_name]])
    }
  }
  resultado_final <- resultado_final_unordered %>%
    arrange(.data$combo_maestro, across(any_of(des))) %>%
    select(-.data$combo_maestro) %>%
    select(any_of(keys_cuantil), everything())

  if (save_xlsx) {
    dir.create(dir, showWarnings = FALSE, recursive = TRUE)
    des_tag <- if (!is.null(des)) paste(des, collapse = "-") else "nac"
    cuant_tag <- gsub("[^0-9A-Za-z]", "_", formatC(cuant, format = "f", digits = 2))
    filename <- file.path(dir, paste0(var, "_", des_tag, "_", paste(sufijo, collapse = "-"), "_CUANTIL_", cuant_tag, ".xlsx"))

    if (formato) {
      generate_quantile_report(hojas_list, filename, var, des, sufijo, cuant, decimales, designs = designs, consolidated_df = resultado_final, nombre_indicador = nombre_indicador, lista_tests = lista_tests)
    } else {
      wb <- createWorkbook()
      addWorksheet(wb, "Consolidado")
      write_clean_table(wb, "Consolidado", resultado_final, startRow = 1, startCol = 1)
      saveWorkbook(wb, filename, overwrite = TRUE)
      message("Reporte Excel simple creado en: ", filename)
    }
  }
  if (verbose) message("Proceso completado.")
  invisible(resultado_final)
}
