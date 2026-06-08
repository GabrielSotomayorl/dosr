# ---------------------------------------------------------------------------- #
# Archivo: multi_bin.R (VERSIÓN FINAL CON CORRECCIONES PARA CHECK)
# ---------------------------------------------------------------------------- #

#' @importFrom stats setNames
#' @title Perfilar Múltiples Variables Dicotómicas con Criterios de Calidad
#'
#' @description Para un **único diseño de encuesta**, calcula la proporción de
#'   "1s" para un vector de variables dicotómicas (0/1). Esta función es una
#'   herramienta de conveniencia para perfilados rápidos y análisis exploratorios.
#'   A diferencia de `obs_prop`, está optimizada para analizar múltiples
#'   variables dentro de una sola encuesta, pero no para comparar entre años.
#'   Genera un reporte en Excel con una hoja consolidada (con todas las métricas
#'   de calidad) y hojas de formato para el nivel nacional y cada desagregación
#'   simple.
#'
#' @param design Un objeto `tbl_svy` de `srvyr`.
#' @param vars_binarias Un vector de strings con los nombres de las variables
#'   dicotómicas (codificadas como 0/1) a perfilar.
#' @param des Un vector de strings con los nombres de las variables de
#'   desagregación simple.
#' @param es_var_estudio Booleano. Si `TRUE`, aplica criterios de fiabilidad menos
#'   estrictos para el tamaño muestral.
#' @param filt Expresión de filtro. Acepta tanto una expresión R sin comillas
#'   (`filt = edad > 18`) como un string (`filt = "edad > 18"`). Ambas formas
#'   son equivalentes y retrocompatibles.
#' @param dir Un string con la ruta del directorio de salida.
#' @param filename Un string con el nombre del archivo Excel.
#' @param decimales Entero. Número de decimales para la estimación puntual. Por defecto es 1.
#' @param decimales_se Entero. Número de decimales para el error estándar. Por defecto es 3.
#' @param n_minimo Entero. Tamaño muestral mínimo para clasificar una estimación como fiable. Por defecto es `30`.
#' @param verbose Booleano. Si `TRUE`, muestra mensajes de progreso.
#'
#' @return Un data.frame con todos los resultados consolidados (invisiblemente).
#' @examples
#' \donttest{
#' library(srvyr)
#'
#' design_2024 <- as_survey_design(casen_2024, ids = varunit,
#'                                 strata = varstrat, weights = expr, nest = TRUE)
#'
#' # Prevalencia de indicadores de inseguridad alimentaria por región
#' multi_bin(design_2024, vars_binarias = paste0("r8", letters[1:8]),
#'           des = "region", dir = tempdir())
#' }
#' @export
multi_bin <- function(
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
) {
  # --- Helpers Internos ---
  .calculate_estimates_multi <- function(d, vars, by = NULL) {
    vars_valid <- Filter(function(v) {
      ok <- v %in% names(d$variables)
      if (!ok) warning(paste("Variable", v, "no encontrada. Se omitir\u00e1."), call. = FALSE)
      ok
    }, vars)
    if (length(vars_valid) == 0L) return(tibble::tibble())

    psu_var    <- colnames(d$cluster)[1]
    strata_var <- colnames(d$strata)[1]

    d_calc <- if (!is.null(by)) {
      srvyr::group_by(d, dplyr::across(dplyr::all_of(by)))
    } else {
      d
    }

    # Pasada 1: medias para todas las variables (una sola llamada survey)
    mean_exprs <- setNames(
      lapply(vars_valid, function(v)
        rlang::expr(srvyr::survey_mean(!!rlang::sym(v), na.rm = TRUE, vartype = "se"))),
      vars_valid
    )
    est_means <- d_calc %>% srvyr::summarise(!!!mean_exprs)

    # Pasada 2: totales para todas las variables
    total_exprs <- setNames(
      lapply(vars_valid, function(v)
        rlang::expr(srvyr::survey_total(!!rlang::sym(v), na.rm = TRUE))),
      paste0(vars_valid, "_tot")
    )
    est_totals <- d_calc %>% srvyr::summarise(!!!total_exprs)

    # Pasada 3: conteos no ponderados + gl (una sola llamada)
    nmues_exprs <- setNames(
      lapply(vars_valid, function(v)
        rlang::expr(srvyr::unweighted(sum(!!rlang::sym(v) == 1L, na.rm = TRUE)))),
      paste0(vars_valid, "_n")
    )
    counts_df <- d_calc %>%
      srvyr::summarise(
        !!!nmues_exprs,
        gl = srvyr::unweighted(
          dplyr::n_distinct(!!rlang::sym(psu_var)) -
            dplyr::n_distinct(!!rlang::sym(strata_var))
        )
      )

    wide_df <- if (is.null(by)) {
      dplyr::bind_cols(est_means, est_totals, counts_df)
    } else {
      est_means %>%
        dplyr::left_join(est_totals, by = by) %>%
        dplyr::left_join(counts_df, by = by)
    }

    # Pivote a formato largo (solo reshape de datos, sin survey)
    purrr::map_dfr(vars_valid, function(v) {
      dplyr::select(wide_df,
        dplyr::any_of(by),
        estimacion     = dplyr::all_of(v),
        estimacion_se  = dplyr::all_of(paste0(v, "_se")),
        n_expandido    = dplyr::all_of(paste0(v, "_tot")),
        n_expandido_se = dplyr::all_of(paste0(v, "_tot_se")),
        n_mues         = dplyr::all_of(paste0(v, "_n")),
        gl
      ) %>%
        dplyr::mutate(
          estimacion    = estimacion * 100,
          estimacion_se = estimacion_se * 100,
          variable  = v,
          etiqueta  = labelled::var_label(d$variables[[v]]) %||% v,
          n_mues    = as.integer(dplyr::coalesce(n_mues, 0L)),
          gl        = as.numeric(gl),
          .before   = 1
        )
    })
  }

  .calculate_reliability_multi <- function(df, by_vars = NULL, n_minimo = 30) {
    if (nrow(df) == 0) {
      return(dplyr::mutate(
        df,
        n_universo = numeric(),
        n_niveles = numeric(),
        prop_val = numeric(),
        se_umbral_prop = numeric(),
        se_umbral = numeric(),
        fiabilidad = character()
      ))
    }
    df %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(by_vars))) %>%
      dplyr::mutate(
        n_universo = sum(n_mues, na.rm = TRUE),
        n_niveles = 2
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        prop_val = estimacion / 100,
        se_umbral_prop = dplyr::if_else(
          prop_val < 0.5,
          (prop_val^(2 / 3)) / 9,
          ((1 - prop_val)^(2 / 3)) / 9
        ),
        se_umbral = se_umbral_prop * 100,
        fiabilidad = dplyr::case_when(
          is.na(n_mues) | n_mues == 0 ~ "Sin casos",
          is.na(gl) | gl <= 9 ~ "No Fiable (gl)",
          n_universo < n_minimo & !es_var_estudio ~ "No Fiable (muestra)",
          is.na(estimacion_se) | estimacion_se > se_umbral ~ "Poco Fiable (EE)",
          TRUE ~ "Fiable"
        )
      )
  }

  # --- Lógica Principal ---

  filt <- .resolve_filt(rlang::enquo(filt))
  if (verbose) {
    message("Aplicando filtro (si aplica)...")
  }
  if (!is.null(filt) && nzchar(filt)) {
    design <- design %>% srvyr::filter(!!rlang::parse_expr(filt))
  }

  if (verbose) {
    message("Calculando perfil nacional...")
  }
  res_nacional <- .calculate_estimates_multi(design, vars_binarias) %>%
    .calculate_reliability_multi(n_minimo = n_minimo)

  res_desagregados <- list()
  if (!is.null(des)) {
    res_desagregados <- purrr::map(des, function(d_var) {
      if (verbose) {
        message(paste("Calculando desagregaci\u00f3n por:", d_var, "..."))
      }
      design_des <- design %>%
        dplyr::mutate(dplyr::across(dplyr::all_of(d_var), haven::as_factor))
      .calculate_estimates_multi(design_des, vars_binarias, by = d_var) %>%
        .calculate_reliability_multi(by_vars = d_var, n_minimo = n_minimo)
    }) %>%
      rlang::set_names(des)
  }

  # --- Consolidación de Resultados ---
  lista_para_unir <- list()
  lista_para_unir$nacional <- res_nacional %>%
    dplyr::mutate(
      desagregacion_tipo = "Nacional",
      desagregacion_categoria = "Nacional"
    )
  if (length(res_desagregados) > 0) {
    lista_des <- purrr::imap(res_desagregados, function(df, nombre_des) {
      df %>%
        dplyr::mutate(desagregacion_tipo = nombre_des) %>%
        dplyr::rename(desagregacion_categoria = !!sym(nombre_des)) %>%
        dplyr::mutate(
          desagregacion_categoria = as.character(desagregacion_categoria)
        )
    })
    lista_para_unir <- c(lista_para_unir, lista_des)
  }
  all_results <- dplyr::bind_rows(lista_para_unir) %>%
    dplyr::relocate(desagregacion_tipo, desagregacion_categoria)

  # --- Creación del Reporte en Excel ---
  if (verbose) {
    message("Generando reporte Excel...")
  }

  dir.create(dir, showWarnings = FALSE, recursive = TRUE)

  if (is.null(filename)) {
    vars_tag <- if (length(vars_binarias) > 1) {
      paste0(vars_binarias[1], "-", utils::tail(vars_binarias, 1))
    } else {
      vars_binarias[1]
    }
    des_tag <- if (!is.null(des)) {
      paste0("_", paste(des, collapse = "-"))
    } else {
      ""
    }
    filename <- paste0(vars_tag, des_tag, "_MULT.xlsx")
  }

  filepath <- file.path(dir, filename)

  wb <- openxlsx::createWorkbook()

  est_num_fmt <- if (decimales > 0) {
    paste0("0.", paste(rep("0", decimales), collapse = ""))
  } else {
    "0"
  }
  se_num_fmt <- if (decimales_se > 0) {
    paste0("0.", paste(rep("0", decimales_se), collapse = ""))
  } else {
    "0"
  }

  estStyle <- openxlsx::createStyle(numFmt = est_num_fmt)
  seStyle <- openxlsx::createStyle(numFmt = se_num_fmt)
  countStyle <- openxlsx::createStyle(numFmt = "#,##0")
  hdrStyle <- openxlsx::createStyle(
    textDecoration = "bold",
    fgFill = "#D9D9D9",
    border = "TopBottomLeftRight"
  )
  boldStyle <- openxlsx::createStyle(textDecoration = "bold")

  openxlsx::addWorksheet(wb, "1_Consolidado")
  openxlsx::writeData(wb, "1_Consolidado", all_results, headerStyle = hdrStyle)
  openxlsx::setColWidths(
    wb,
    "1_Consolidado",
    cols = 1:ncol(all_results),
    widths = "auto"
  )

  openxlsx::addWorksheet(wb, "2_Nacional")
  openxlsx::writeData(
    wb,
    "2_Nacional",
    "Nombre indicador",
    startRow = 2,
    startCol = 1
  )
  openxlsx::addStyle(wb, "2_Nacional", style = boldStyle, rows = 2, cols = 1)
  openxlsx::writeData(
    wb,
    "2_Nacional",
    "Tipo de c\u00e1lculo",
    startRow = 3,
    startCol = 1
  )

  df_nac_final <- res_nacional %>%
    dplyr::select(etiqueta, estimacion, estimacion_se, n_expandido, n_mues)
  openxlsx::writeData(
    wb,
    "2_Nacional",
    df_nac_final,
    headerStyle = hdrStyle,
    startRow = 5
  )

  openxlsx::addStyle(
    wb,
    "2_Nacional",
    style = estStyle,
    rows = 6:(nrow(df_nac_final) + 5),
    cols = 2
  )
  openxlsx::addStyle(
    wb,
    "2_Nacional",
    style = seStyle,
    rows = 6:(nrow(df_nac_final) + 5),
    cols = 3
  )
  openxlsx::addStyle(
    wb,
    "2_Nacional",
    style = countStyle,
    rows = 6:(nrow(df_nac_final) + 5),
    cols = 4:5,
    gridExpand = TRUE
  )
  openxlsx::setColWidths(
    wb,
    "2_Nacional",
    cols = 1:ncol(df_nac_final),
    widths = "auto"
  )

  for (d_var in names(res_desagregados)) {
    sheet_name <- paste0("2_", d_var)
    openxlsx::addWorksheet(wb, sheet_name)

    openxlsx::writeData(
      wb,
      sheet_name,
      "Nombre indicador",
      startRow = 2,
      startCol = 1
    )
    openxlsx::addStyle(wb, sheet_name, style = boldStyle, rows = 2, cols = 1)
    openxlsx::writeData(
      wb,
      sheet_name,
      "Tipo de c\u00e1lculo",
      startRow = 3,
      startCol = 1
    )

    df_des_final <- res_desagregados[[d_var]] %>%
      dplyr::arrange(dplyr::across(dplyr::all_of(d_var))) %>%
      dplyr::select(
        dplyr::all_of(d_var),
        etiqueta,
        estimacion,
        estimacion_se,
        n_expandido,
        n_mues
      )

    openxlsx::writeData(
      wb,
      sheet_name,
      df_des_final,
      headerStyle = hdrStyle,
      startRow = 5
    )

    openxlsx::addStyle(
      wb,
      sheet_name,
      style = estStyle,
      rows = 6:(nrow(df_des_final) + 5),
      cols = 3
    )
    openxlsx::addStyle(
      wb,
      sheet_name,
      style = seStyle,
      rows = 6:(nrow(df_des_final) + 5),
      cols = 4
    )
    openxlsx::addStyle(
      wb,
      sheet_name,
      style = countStyle,
      rows = 6:(nrow(df_des_final) + 5),
      cols = 5:6,
      gridExpand = TRUE
    )
    openxlsx::setColWidths(
      wb,
      sheet_name,
      cols = 1:ncol(df_des_final),
      widths = "auto"
    )

    if (nrow(df_des_final) > 0) {
      runs <- rle(as.character(df_des_final[[d_var]]))
      end_rows <- cumsum(runs$lengths)
      start_rows <- end_rows - runs$lengths + 1
      for (i in seq_along(runs$lengths)) {
        if (runs$lengths[i] > 1) {
          openxlsx::mergeCells(
            wb,
            sheet_name,
            cols = 1,
            rows = (start_rows[i] + 5):(end_rows[i] + 5)
          )
        }
      }
    }
  }

  openxlsx::saveWorkbook(wb, filepath, overwrite = TRUE)
  message(paste("Reporte Excel creado en:", filepath))

  invisible(all_results)
}
