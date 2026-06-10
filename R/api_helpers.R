# Internal helpers shared by all obs_* API functions.
# These are not exported; they exist to eliminate repetition across the five
# obs_prop / obs_media / obs_total / obs_ratio / obs_cuantil functions.

.guard_multi_des <- function(des, multi_des) {
  if (multi_des && length(des) > 3) {
    stop(paste(
      "Se solicitaron", length(des), "variables de desagregaci\u00f3n con 'multi_des = TRUE'.",
      "Esto generar\u00eda", 2^length(des) - 1, "combinaciones y ser\u00eda extremadamente lento.",
      "\nSoluci\u00f3n: Use 3 o menos variables en 'des', o establezca 'multi_des = FALSE'",
      "para obtener solo las desagregaciones simples."
    ), call. = FALSE)
  }
}

.prepare_designs_list <- function(designs, sufijo, verbose) {
  if (verbose) message("Fase 1/3: Preparando datos y dise\u00f1os...")
  if (!inherits(designs, "list")) designs <- list(designs)
  n_designs <- length(designs)
  if (is.null(sufijo)) {
    sufijo <- if (!is.null(names(designs)) && all(nzchar(names(designs))))
      names(designs) else as.character(seq_len(n_designs))
  }
  stopifnot(
    "La longitud de 'sufijo' debe coincidir con el n\u00famero de dise\u00f1os." =
      length(sufijo) == n_designs
  )
  names(designs) <- sufijo
  bad <- which(!vapply(designs, inherits, logical(1), what = "tbl_svy"))
  if (length(bad) > 0) {
    stop(
      sprintf(
        "El elemento %d de 'designs' no es un objeto tbl_svy v\u00e1lido. Use srvyr::as_survey_design() para crearlo.",
        bad[1]
      ),
      call. = FALSE
    )
  }
  list(designs = designs, sufijo = sufijo, n_designs = n_designs)
}

.extract_var_label <- function(designs, var, usar_etiqueta_var, nombre) {
  nombre_indicador <- var
  if (usar_etiqueta_var) {
    lbl <- labelled::var_label(designs[[1]]$variables[[var]])
    if (!is.null(lbl)) nombre_indicador <- lbl
  }
  if (!is.null(nombre)) nombre_indicador <- nombre
  nombre_indicador
}

.build_meta_and_light <- function(designs, main_vars, des, filt) {
  design_metadata <- purrr::map(designs, ~ list(
    psu    = colnames(.x$cluster)[1],
    strata = colnames(.x$strata)[1],
    weight = names(.x$allprob)[1]
  ))
  vars_in_filter <- if (!is.null(filt)) all.vars(rlang::parse_expr(filt)) else character(0)
  list(
    metadata = design_metadata,
    light    = create_lightweight_designs(designs, main_vars, des, vars_in_filter, design_metadata)
  )
}

.make_calc_fun <- function(var, des, filt, rm_na_var, rm_na_des, type, multi_des,
                            es_var_estudio, porcentaje = FALSE,
                            quantile_prob = 0.5, ratio_vars = NULL,
                            cv_umbral_alto = 0.30, cv_umbral_medio = 0.20,
                            n_minimo = 30L, nivel_confianza = 0.95,
                            universo_crit = FALSE, par_combos = FALSE) {
  lonely_psu_option <- getOption("survey.lonely.psu", NULL)
  function(dsgn, meta) {
    if (!is.null(lonely_psu_option)) {
      old_lonely <- getOption("survey.lonely.psu", NULL)
      on.exit({
        if (is.null(old_lonely)) options(survey.lonely.psu = NULL)
        else options(survey.lonely.psu = old_lonely)
      }, add = TRUE)
      options(survey.lonely.psu = lonely_psu_option)
    }
    base_args <- list(
      dsgn, meta, var, des, filt, rm_na_var, rm_na_des, type, multi_des,
      es_var_estudio, porcentaje,
      quantile_prob   = quantile_prob,
      ratio_vars      = ratio_vars,
      cv_umbral_alto  = cv_umbral_alto,
      cv_umbral_medio = cv_umbral_medio,
      n_minimo        = n_minimo,
      nivel_confianza = nivel_confianza,
      universo_crit   = universo_crit
    )
    if (isTRUE(par_combos)) base_args$par_combos <- TRUE
    do.call(calculate_single_design, base_args)
  }
}

.run_estimations <- function(designs_light, design_metadata, calc_fun,
                              parallel, n_cores, n_designs, verbose) {
  if (verbose) message("Fase 2/3: Calculando estimaciones... (Esta etapa puede tardar varios minutos)")
  pmap_args <- list(dsgn = designs_light, meta = design_metadata)
  if (parallel) {
    max_safe_cores <- 4L
    cores_detected <- tryCatch(parallel::detectCores(), error = function(e) 2L)
    cores <- if (is.null(n_cores)) {
      max(1L, min(cores_detected - 1L, max_safe_cores,
                  if (n_designs > 1L) n_designs else max_safe_cores))
    } else {
      stopifnot(
        "'n_cores' debe ser un entero positivo de longitud 1" =
          is.numeric(n_cores) && length(n_cores) == 1L &&
          is.finite(n_cores) && n_cores >= 1
      )
      as.integer(n_cores)
    }
    if (verbose) message(paste("... usando modo paralelo con", cores, "workers."))
    old_plan <- future::plan(future::multisession, workers = cores)
    on.exit(future::plan(old_plan), add = TRUE)
    if (n_designs > 1L) {
      furrr::future_pmap(pmap_args, calc_fun, .progress = FALSE)
    } else {
      # n_designs == 1: el plan ya está activo; calc_fun usa furrr internamente
      # para los combos de desagregación (par_combos = TRUE capturado en closure)
      purrr::pmap(pmap_args, calc_fun)
    }
  } else {
    purrr::pmap(pmap_args, calc_fun)
  }
}

.finalize_results <- function(hojas_list, keys, designs, type, des,
                               sort_extra = NULL) {
  factor_vars <- if (identical(type, "prop")) c(des, keys[1L]) else des
  all_factor_levels <- get_all_levels(designs, factor_vars)
  resultado <- bind_rows(hojas_list, .id = "combo_maestro") %>% unique_cols()
  resultado[["combo_maestro"]] <- factor(resultado[["combo_maestro"]], levels = names(hojas_list))
  for (col_name in names(all_factor_levels)) {
    if (col_name %in% names(resultado)) {
      resultado[[col_name]] <- factor(resultado[[col_name]], levels = all_factor_levels[[col_name]])
    }
  }
  sort_cols <- unique(c(des, sort_extra))
  resultado %>%
    arrange(.data[["combo_maestro"]], across(any_of(sort_cols))) %>%
    select(-all_of("combo_maestro")) %>%
    select(any_of(keys), everything())
}

.save_simple_xlsx <- function(dir, filename, resultado_final, verbose = TRUE) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  wb <- createWorkbook()
  addWorksheet(wb, "Consolidado")
  write_clean_table(wb, "Consolidado", resultado_final, startRow = 1L, startCol = 1L)
  saveWorkbook(wb, filename, overwrite = TRUE)
  if (verbose) message("Reporte Excel simple creado en: ", filename)
}
