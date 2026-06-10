# ---------------------------------------------------------------------------- #
# Archivo: reporting.R (VERSI\u00d3N CON ESCRITURA DE TESTS DE SIGNIFICANCIA)
# ---------------------------------------------------------------------------- #

# --- Ayudante para fusionar celdas ---
merge_group_cells <- function(wb, sheet, df, grp_des, start_row) {
  if (length(grp_des) == 0) return()
  for (col_idx in seq_along(grp_des)) {
    runs <- rle(as.character(df[[grp_des[col_idx]]]))
    run_lengths <- runs$lengths
    end_rows_r <- cumsum(run_lengths)
    start_rows_r <- end_rows_r - run_lengths + 1
    for (j in seq_along(run_lengths)) {
      if (run_lengths[j] > 1 && !grepl("Total", runs$values[j], ignore.case = TRUE)) {
        merge_rows <- start_row:(start_row + run_lengths[j] - 1) + (start_rows_r[j] - 1)
        openxlsx::mergeCells(wb, sheet, cols = col_idx, rows = merge_rows)
      }
    }
  }
}

# --- Obtener niveles de factores sin ordenar alfab\u00e9ticamente ---
get_all_levels <- function(designs, vars) {
  purrr::map(vars, function(v) {
    if (v %in% names(designs[[1]]$variables)) {
      levels_list <- purrr::map(designs, ~ levels(haven::as_factor(.x$variables[[v]])))
      all_lvls <- unique(unlist(levels_list))
      return(all_lvls[!is.na(all_lvls)])
    } else {
      return(NULL)
    }
  }) %>%
    rlang::set_names(vars) %>%
    purrr::compact()
}

# --- Agregador de resultados ---
aggregate_results <- function(lista_tablas, sufijo, keys, all_designs, type) {
  combos_nombres <- names(lista_tablas[[1]])
  if (type == "prop") {
    var_interes <- keys[1]
    des_vars <- setdiff(keys, c(var_interes, "nivel"))
  } else {
    var_interes <- character(0)
    des_vars <- setdiff(keys, c("variable", "nivel"))
  }
  all_levels_des <- get_all_levels(all_designs, des_vars)
  all_levels_var <- if(length(var_interes) > 0) get_all_levels(all_designs, var_interes) else list()
  combinar_por_combo <- function(combo_name) {
    tablas_combo <- purrr::map(lista_tablas, ~ .x[[combo_name]])
    totals_combo <- purrr::imap(tablas_combo, function(df, idx) {
      totals_attr <- attr(df, "totals_filtered")
      if (!is.null(totals_attr)) totals_attr else NULL
    }) %>% purrr::compact()
    tablas_combo_char <- purrr::map(tablas_combo, ~ .x %>% mutate(across(any_of(keys), as.character)))
    metric_cols <- setdiff(names(tablas_combo_char[[1]]), keys)
    tablas_renombradas <- purrr::imap(tablas_combo_char, function(df, sfx) {
      df %>% rename_with(~ paste0(.x, "_", sfx), all_of(metric_cols))
    })
    joined_df <- reduce(tablas_renombradas, function(x, y) full_join(x, y, by = keys))
    grp_des <- if (combo_name == "nac") character(0) else strsplit(combo_name, "__")[[1]]
    if (type == "prop") {
      all_levels_to_use <- c(all_levels_des[grp_des], all_levels_var)
    } else {
      all_levels_to_use <- all_levels_des[grp_des]
    }
    all_levels_to_use <- all_levels_to_use[names(all_levels_to_use) != ""]
    if (length(all_levels_to_use) > 0) {
      fill_list_prop <- purrr::map(sufijo, ~ 0) %>% set_names(paste0("prop_", sufijo))
      fill_list_n <- purrr::map(sufijo, ~ 0) %>% set_names(paste0("n_mues_", sufijo))
      fill_list_n_univ <- purrr::map(sufijo, ~ 0) %>% set_names(paste0("n_universo_", sufijo))
      fill_list_N <- purrr::map(sufijo, ~ 0) %>% set_names(paste0("N_pob_", sufijo))
      fill_list_media <- purrr::map(sufijo, ~ NA_real_) %>% set_names(paste0("media_", sufijo))
      fill_list_total <- purrr::map(sufijo, ~ NA_real_) %>% set_names(paste0("total_", sufijo))
      fill_list_ratio <- purrr::map(sufijo, ~ NA_real_) %>% set_names(paste0("ratio_", sufijo))
      fill_list_cuantil <- purrr::map(sufijo, ~ NA_real_) %>% set_names(paste0("cuantil_", sufijo))
      final_fill_list <- switch(
        type,
        prop = c(fill_list_prop, fill_list_N, fill_list_n, fill_list_n_univ),
        mean = c(fill_list_media, fill_list_n, fill_list_N),
        total = c(fill_list_total, fill_list_n, fill_list_N),
        quantile = c(fill_list_cuantil, fill_list_n, fill_list_N),
        ratio = c(fill_list_ratio, fill_list_n, fill_list_N)
      )
      grouping_vars <- if (type %in% c("mean", "quantile", "total", "ratio")) "variable" else NULL
      joined_df <- joined_df %>%
        group_by(across(any_of(grouping_vars))) %>%
        tidyr::complete(!!!all_levels_to_use, fill = final_fill_list) %>%
        ungroup() %>%
        mutate(nivel = ifelse(is.na(nivel) & length(grp_des) > 0, paste(grp_des, collapse = "-"), nivel))
      for (sfx in sufijo) {
        n_mues_col <- paste0("n_mues_", sfx)
        fiabilidad_col <- paste0("fiabilidad_", sfx)
        if (fiabilidad_col %in% names(joined_df)) {
          joined_df <- joined_df %>%
            mutate(!!fiabilidad_col := if_else(.data[[n_mues_col]] == 0, "Sin casos", .data[[fiabilidad_col]]))
        }
      }
    }
    if (length(totals_combo) > 0) {
      attr(joined_df, "totals_filtered") <- totals_combo
    }
    return(joined_df)
  }
  hojas_list <- purrr::map(combos_nombres, combinar_por_combo)
  names(hojas_list) <- combos_nombres
  return(hojas_list)
}

# ---------------------------------------------------------------------------- #
# Helpers de nota de fiabilidad, fuente y colores
# ---------------------------------------------------------------------------- #

# Construye las l\u00edneas de la nota de fiabilidad para una hoja de formato.
.build_quality_note <- function(df_wide, sufijo, id_cols, mostrar_pct = FALSE) {
  fial_cols <- paste0("fiabilidad_", sufijo)
  fial_cols <- intersect(fial_cols, names(df_wide))
  if (length(fial_cols) == 0) return(character(0))

  classify_kind <- function(x) {
    if (is.na(x) || x == "" || x == "Sin casos") return(NA_character_)
    if (startsWith(as.character(x), "No Fiable"))   return("no")
    if (startsWith(as.character(x), "Poco Fiable")) return("poco")
    if (startsWith(as.character(x), "Fiable"))      return("fiable")
    NA_character_
  }

  row_label <- function(row) {
    parts <- vapply(id_cols, function(col) {
      v <- if (col %in% names(row)) as.character(row[[col]]) else NA_character_
      if (is.na(v) || !nzchar(v)) NA_character_ else v
    }, character(1))
    parts <- parts[!is.na(parts)]
    if (length(parts) == 0) "Total" else paste(parts, collapse = " - ")
  }

  join_labels <- function(lbls) {
    if (length(lbls) == 1) return(lbls)
    if (length(lbls) == 2) return(paste(lbls[1], "y", lbls[2]))
    paste0(paste(lbls[-length(lbls)], collapse = "; "), "; y ", lbls[length(lbls)])
  }

  total_q <- 0L
  total_f <- 0L
  excep_poco <- list()
  excep_no   <- list()

  for (sfx in sufijo) {
    fcol <- paste0("fiabilidad_", sfx)
    if (!fcol %in% names(df_wide)) next
    poco_lbl <- character(0)
    no_lbl   <- character(0)
    for (i in seq_len(nrow(df_wide))) {
      kind <- classify_kind(df_wide[[fcol]][i])
      if (is.na(kind)) next
      total_q <- total_q + 1L
      lbl <- row_label(as.list(df_wide[i, ]))
      if (kind == "fiable") {
        total_f <- total_f + 1L
      } else if (kind == "poco") {
        poco_lbl <- c(poco_lbl, lbl)
      } else {
        no_lbl <- c(no_lbl, lbl)
      }
    }
    if (length(poco_lbl) > 0) excep_poco[[sfx]] <- poco_lbl
    if (length(no_lbl)   > 0) excep_no[[sfx]]   <- no_lbl
  }

  if (total_q == 0L) return(character(0))

  pct <- 100 * total_f / total_q
  lines <- "Nota:"

  if (mostrar_pct) {
    lines <- c(lines, sprintf("El %.1f%% de las estimaciones del cuadro son fiables.", pct))
  }

  if (pct < 50) {
    lines <- c(lines, "Dado que menos del 50% de las estimaciones del cuadro son fiables, se recomienda no publicar.")
  }

  has_excep <- length(excep_poco) > 0 || length(excep_no) > 0
  if (!has_excep) {
    lines <- c(lines, "Todas las estimaciones son fiables.")
  } else {
    lines <- c(lines, "Todas las estimaciones son fiables, exceptuando las siguientes:")
    for (sfx in sufijo) {
      if (!is.null(excep_poco[[sfx]]))
        lines <- c(lines, paste0(sfx, " - Poco fiables: ", join_labels(excep_poco[[sfx]]), "."))
      if (!is.null(excep_no[[sfx]]))
        lines <- c(lines, paste0(sfx, " - No fiables: ", join_labels(excep_no[[sfx]]), "."))
    }
  }
  lines
}

# Escribe nota de fiabilidad en la hoja. Devuelve la siguiente fila disponible.
.write_quality_note <- function(wb, sheet_nm, start_row, note_lines, n_cols) {
  if (length(note_lines) == 0) return(start_row)
  bold_st  <- openxlsx::createStyle(textDecoration = "bold")
  plain_st <- openxlsx::createStyle()
  n_cols   <- max(2L, n_cols)
  for (i in seq_along(note_lines)) {
    r <- start_row + i - 1L
    openxlsx::writeData(wb, sheet_nm, note_lines[i], startRow = r, startCol = 1)
    openxlsx::mergeCells(wb, sheet_nm, cols = 1:n_cols, rows = r)
    sty <- if (note_lines[i] == "Nota:") bold_st else plain_st
    openxlsx::addStyle(wb, sheet_nm, style = sty, rows = r, cols = 1)
  }
  start_row + length(note_lines)
}

# Escribe l\u00ednea de fuente. Acepta claves est\u00e1ndar o texto libre.
.write_fuente <- function(wb, sheet_nm, row, fuente, sufijo, n_cols) {
  if (is.null(fuente) || !nzchar(fuente)) return()
  mapa <- c(
    casen  = "Encuesta Casen",
    ebs    = "Encuesta de Bienestar Social",
    endide = "Encuesta de Discapacidad y Dependencia",
    eanna  = "Encuesta de Actividades de Ni\u00f1os, Ni\u00f1as y Adolescentes",
    elpi   = "Encuesta Longitudinal de Primera Infancia"
  )
  display <- mapa[tolower(fuente)]
  if (is.na(display)) display <- fuente
  sfx_num <- suppressWarnings(as.integer(sufijo))
  yr_txt  <- if (all(!is.na(sfx_num)) && length(sfx_num) > 0) {
    rng <- range(sfx_num)
    if (rng[1] == rng[2]) as.character(rng[1]) else paste0(rng[1], "-", rng[2])
  } else paste(sufijo, collapse = "-")
  txt <- paste0("Fuente: ", display, " ", yr_txt)
  n_cols <- max(2L, n_cols)
  openxlsx::writeData(wb, sheet_nm, txt, startRow = row, startCol = 1)
  openxlsx::mergeCells(wb, sheet_nm, cols = 1:n_cols, rows = row)
  openxlsx::addStyle(wb, sheet_nm,
    style = openxlsx::createStyle(textDecoration = "bold"),
    rows = row, cols = 1, stack = TRUE)
}

# Aplica colores de fiabilidad a celdas num\u00e9ricas de un bloque ya escrito.
.apply_quality_colors <- function(wb, sheet_nm, block_df, df_wide, key_cols,
                                   sufijo, first_data_row, first_num_col) {
  color_poco <- openxlsx::createStyle(fontColour = "#BF9000")
  color_no   <- openxlsx::createStyle(fontColour = "#C00000")

  classify_kind <- function(x) {
    if (is.na(x) || x == "" || x == "Sin casos") return(NA_character_)
    if (startsWith(as.character(x), "No Fiable"))   return("no")
    if (startsWith(as.character(x), "Poco Fiable")) return("poco")
    NA_character_
  }

  for (i in seq_len(nrow(block_df))) {
    for (j in seq_along(sufijo)) {
      sfx   <- sufijo[j]
      fcol  <- paste0("fiabilidad_", sfx)
      if (!fcol %in% names(df_wide)) next

      valid_keys <- intersect(key_cols, intersect(names(block_df), names(df_wide)))
      if (length(valid_keys) > 0) {
        mask <- rep(TRUE, nrow(df_wide))
        for (kc in valid_keys) {
          bv <- as.character(block_df[[kc]][i])
          dv <- as.character(df_wide[[kc]])
          mask <- mask & (!is.na(dv)) & (dv == bv)
        }
        match_rows <- which(mask)
      } else {
        match_rows <- i
      }
      if (length(match_rows) == 0) next
      kind <- classify_kind(df_wide[[fcol]][match_rows[1]])
      if (is.na(kind)) next
      sty <- if (kind == "poco") color_poco else color_no
      excel_row <- first_data_row + i - 1L
      excel_col <- first_num_col + j - 1L
      openxlsx::addStyle(wb, sheet_nm, style = sty, rows = excel_row, cols = excel_col, stack = TRUE)
    }
  }
}

# ---------------------------------------------------------------------------- #
# --- Reporte espec\u00edfico para Proporciones ---
# ---------------------------------------------------------------------------- #
generate_prop_report <- function(hojas_list, filename, var, des, sufijo, porcentaje, decimales, designs, consolidated_df, nombre_indicador,
                                  lista_tests = NULL,
                                  snac = FALSE,
                                  mostrar_pct_fiable = FALSE,
                                  color_fiabilidad = FALSE,
                                  fuente = NULL,
                                  verbose = TRUE) {
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "1_Consolidado")
  write_clean_table(wb, "1_Consolidado", consolidated_df, startRow = 1, startCol = 1)

  countStyle <- openxlsx::createStyle(numFmt = "#,##0")
  percent_num_fmt_string <- if (decimales > 0) paste0("0.", paste(rep("0", decimales), collapse = "")) else "0"
  percentStyle <- if (porcentaje) {
    openxlsx::createStyle(numFmt = percent_num_fmt_string)
  } else {
    openxlsx::createStyle(numFmt = paste0(percent_num_fmt_string, "%"))
  }
  testStyle <- openxlsx::createStyle(numFmt = "0.000")

  estilos <- list(percentStyle, countStyle, percentStyle, countStyle)
  titulos  <- c("Estimaci\u00f3n", "Poblaci\u00f3n expandida", "Error est\u00e1ndar", "Casos muestrales")
  metricas <- c("prop", "N_pob", "se", "n_mues")
  met_suffix_list <- purrr::map(metricas, ~ paste0(.x, "_", sufijo))
  names(met_suffix_list) <- metricas
  all_factor_levels <- get_all_levels(designs, c(var, des))
  bold_style <- createStyle(textDecoration = "bold")

  make_prop_block <- function(df_wide, grp_des, metric_cols, factor_levels_list) {
    base_tbl <- df_wide %>% select(all_of(c(grp_des, var, metric_cols)))
    subtotal_prop_val <- if(porcentaje) 100 else 1
    subtot <- base_tbl %>%
      group_by(across(all_of(grp_des))) %>%
      summarise(
        !!sym(var) := "Total",
        across(all_of(metric_cols), ~ {
          col_nm <- dplyr::cur_column()
          if (startsWith(col_nm, "prop_")) {
            subtotal_prop_val
          } else if (startsWith(col_nm, "se_")) {
            0
          } else if (startsWith(col_nm, "n_mues_")) {
            sum(.x, na.rm = TRUE)
          } else if (startsWith(col_nm, "n_universo_")) {
            vals <- .x[!is.na(.x)]
            if (length(vals) == 0) 0 else max(vals)
          } else if (startsWith(col_nm, "gl_")) {
            vals <- .x[!is.na(.x)]
            if (length(vals) == 0) NA_real_ else max(vals)
          } else {
            sum(.x, na.rm = TRUE)
          }
        }),
        .groups = "drop"
      )
    block_unordered <- bind_rows(base_tbl, subtot) %>% unique_cols()
    factor_cols <- intersect(names(factor_levels_list), names(block_unordered))
    for(col in factor_cols) {
      if(col %in% names(block_unordered)) {
        current_levels <- factor_levels_list[[col]]
        if("Total" %in% block_unordered[[col]]) current_levels <- c(current_levels, "Total")
        block_unordered[[col]] <- factor(block_unordered[[col]], levels = current_levels)
      }
    }
    block_ordered <- block_unordered %>% arrange(across(all_of(c(grp_des, var))))
    return(block_ordered)
  }

  for (combo in names(hojas_list)) {
    if (snac && combo == "nac") next
    grp_des <- if (combo == "nac") character(0) else strsplit(combo, "__")[[1]]
    hoja_nm <- truncate_sheet_name(paste0("2_", combo), existing = names(wb))
    openxlsx::addWorksheet(wb, hoja_nm)

    openxlsx::writeData(wb, hoja_nm, nombre_indicador, startRow = 2, startCol = 1)
    openxlsx::addStyle(wb, hoja_nm, style = bold_style, rows = 2, cols = 1)
    openxlsx::writeData(wb, hoja_nm, "Tipo de c\u00e1lculo", startRow = 3, startCol = 1)

    df_combo_wide <- hojas_list[[combo]]

    fila <- 5
    max_col_metrics <- 0
    for (i in seq_along(titulos)) {
      block <- make_prop_block(df_combo_wide, grp_des, met_suffix_list[[metricas[i]]], all_factor_levels)
      if (ncol(block) > max_col_metrics) max_col_metrics <- ncol(block)
      names(block)[names(block) %in% met_suffix_list[[metricas[i]]]] <- sufijo
      openxlsx::writeData(wb, hoja_nm, titulos[i], startRow = fila, startCol = 1)
      cols_to_merge <- 1:ncol(block)
      openxlsx::addStyle(wb, hoja_nm, style = hdrStyle, rows = fila, cols = cols_to_merge, gridExpand = TRUE)
      if (ncol(block) > 1) {
        openxlsx::mergeCells(wb, hoja_nm, cols = cols_to_merge, rows = fila)
      }
      write_clean_table(wb, hoja_nm, block, startRow = fila + 1, startCol = 1)
      merge_group_cells(wb, hoja_nm, block, grp_des, start_row = fila + 2)
      numeric_cols <- (length(grp_des) + length(var) + 1):ncol(block)
      data_rows <- (fila + 2):(fila + 1 + nrow(block))
      if(length(numeric_cols) > 0 && length(data_rows) > 0) {
        openxlsx::addStyle(wb, hoja_nm, style = estilos[[i]], rows = data_rows, cols = numeric_cols, gridExpand = TRUE, stack = TRUE)
      }
      # Aplicar colores de fiabilidad solo al primer bloque (Estimaci\u00f3n)
      if (i == 1 && color_fiabilidad) {
        block_for_color <- make_prop_block(df_combo_wide, grp_des, met_suffix_list[[metricas[i]]], all_factor_levels)
        .apply_quality_colors(wb, hoja_nm, block_for_color, df_combo_wide, c(grp_des, var),
                              sufijo,
                              first_data_row = fila + 2L,
                              first_num_col  = length(grp_des) + length(var) + 1L)
      }
      fila <- fila + nrow(block) + 3
    }

    # Nota de fiabilidad y fuente
    note_id_cols <- c(grp_des, var)
    note_lines <- .build_quality_note(df_combo_wide, sufijo, note_id_cols, mostrar_pct = mostrar_pct_fiable)
    fila_nota <- fila + 1L
    fila_nota <- .write_quality_note(wb, hoja_nm, fila_nota, note_lines, max_col_metrics)
    .write_fuente(wb, hoja_nm, fila_nota + 1L, fuente, sufijo, max_col_metrics)

    if (!is.null(lista_tests) && (
          combo %in% names(lista_tests$intra_year) ||
          combo %in% names(lista_tests$against_last_year) ||
          combo %in% names(lista_tests$against_national))) {
      fila <- 5
      col_inicio_tests <- max_col_metrics + 3

      tests_combo <- lista_tests$intra_year[[combo]]
      for (sfx in names(tests_combo)) {
        test_df <- tests_combo[[sfx]]
        if (!is.null(test_df)) {
          titulo_test <- paste0("Test entre categor\u00edas a\u00f1o: ", sfx)
          openxlsx::writeData(wb, hoja_nm, titulo_test, startRow = fila, startCol = col_inicio_tests)
          cols_to_merge <- col_inicio_tests:(col_inicio_tests + ncol(test_df) - 1)
          openxlsx::mergeCells(wb, hoja_nm, cols = cols_to_merge, rows = fila)
          openxlsx::addStyle(wb, hoja_nm, style = hdrStyle, rows = fila, cols = cols_to_merge, gridExpand = TRUE)

          write_clean_table(wb, hoja_nm, test_df, startRow = fila + 1, startCol = col_inicio_tests)
          numeric_cols <- (col_inicio_tests + 1):(col_inicio_tests + ncol(test_df) - 1)
          data_rows <- (fila + 2):(fila + 1 + nrow(test_df))
          if(length(numeric_cols) > 0 && length(data_rows) > 0) {
            openxlsx::addStyle(wb, hoja_nm, style = testStyle, rows = data_rows, cols = numeric_cols, gridExpand = TRUE, stack = TRUE)
          }
          fila <- fila + nrow(test_df) + 3
        }
      }

      if (length(sufijo) > 1) {
        test_df_last <- lista_tests$against_last_year[[combo]]
        if (!is.null(test_df_last)) {
          titulo_test <- "Test contra \u00faltimo a\u00f1o:"
          openxlsx::writeData(wb, hoja_nm, titulo_test, startRow = fila, startCol = col_inicio_tests)
          cols_to_merge <- col_inicio_tests:(col_inicio_tests + ncol(test_df_last) - 1)
          openxlsx::mergeCells(wb, hoja_nm, cols = cols_to_merge, rows = fila)
          openxlsx::addStyle(wb, hoja_nm, style = hdrStyle, rows = fila, cols = cols_to_merge, gridExpand = TRUE)
          write_clean_table(wb, hoja_nm, test_df_last, startRow = fila + 1, startCol = col_inicio_tests)
          numeric_cols <- (col_inicio_tests + 1):(col_inicio_tests + ncol(test_df_last) - 1)
          data_rows <- (fila + 2):(fila + 1 + nrow(test_df_last))
          if(length(numeric_cols) > 0 && length(data_rows) > 0) {
            openxlsx::addStyle(wb, hoja_nm, style = testStyle, rows = data_rows, cols = numeric_cols, gridExpand = TRUE, stack = TRUE)
          }
          fila <- fila + nrow(test_df_last) + 3
        }

        test_df_nac <- lista_tests$against_national[[combo]]
        if (!is.null(test_df_nac)) {
          titulo_test <- "Test contra estimaci\u00f3n nacional:"
          openxlsx::writeData(wb, hoja_nm, titulo_test, startRow = fila, startCol = col_inicio_tests)
          cols_to_merge <- col_inicio_tests:(col_inicio_tests + ncol(test_df_nac) - 1)
          openxlsx::mergeCells(wb, hoja_nm, cols = cols_to_merge, rows = fila)
          openxlsx::addStyle(wb, hoja_nm, style = hdrStyle, rows = fila, cols = cols_to_merge, gridExpand = TRUE)
          write_clean_table(wb, hoja_nm, test_df_nac, startRow = fila + 1, startCol = col_inicio_tests)
          numeric_cols <- (col_inicio_tests + 1):(col_inicio_tests + ncol(test_df_nac) - 1)
          data_rows <- (fila + 2):(fila + 1 + nrow(test_df_nac))
          if(length(numeric_cols) > 0 && length(data_rows) > 0) {
            openxlsx::addStyle(wb, hoja_nm, style = testStyle, rows = data_rows, cols = numeric_cols, gridExpand = TRUE, stack = TRUE)
          }
        }
      }
    }
  }
  openxlsx::saveWorkbook(wb, filename, overwrite = TRUE)
  if (verbose) message("Reporte Excel de proporciones creado en: ", filename)
}

# ---------------------------------------------------------------------------- #
# Helper interno para el bloque de datos de reportes continuos
# ---------------------------------------------------------------------------- #
.make_continuous_block <- function(df_combo_wide, combo, grp_des, metric_cols, sufijo,
                                    all_factor_levels, total_row) {
  if (combo == "nac") {
    block <- df_combo_wide %>%
      select(nivel, all_of(metric_cols)) %>%
      rename_with(~ sufijo, all_of(metric_cols))
  } else {
    block_main <- df_combo_wide %>% select(all_of(c(grp_des, metric_cols)))
    totals_lookup <- attr(df_combo_wide, "totals_filtered")
    metric_id <- sub("_[^_]+$", "", metric_cols[1])  # strip suffix

    get_total_val <- function(col_nm) {
      sufijo_id <- sub("^.*_", "", col_nm)
      if (!is.null(totals_lookup) && sufijo_id %in% names(totals_lookup)) {
        total_tbl  <- totals_lookup[[sufijo_id]]
        value_col  <- metric_id
        if (!is.null(total_tbl) && value_col %in% names(total_tbl)) {
          return(total_tbl[[value_col]][1])
        }
      }
      if (!is.null(total_row) && col_nm %in% names(total_row)) {
        return(total_row[[col_nm]][1])
      }
      NA_real_
    }

    total_values <- purrr::map_dfc(metric_cols, function(col_nm) {
      tibble::tibble(!!col_nm := get_total_val(col_nm))
    })
    tot_line <- as.list(rep(".", length(grp_des)))
    names(tot_line) <- grp_des
    tot_line[[grp_des[1]]] <- "Total pais"
    tot_line_df <- bind_cols(as_tibble(tot_line), total_values)

    block_unordered <- bind_rows(block_main, tot_line_df) %>%
      rename_with(~ sufijo, all_of(metric_cols))
    factor_cols <- intersect(names(all_factor_levels), names(block_unordered))
    for (col in factor_cols) {
      if (col %in% names(block_unordered)) {
        current_levels <- all_factor_levels[[col]]
        if (any(grepl("Total", block_unordered[[col]], ignore.case = TRUE))) {
          total_label    <- unique(grep("Total", block_unordered[[col]], value = TRUE, ignore.case = TRUE))
          current_levels <- c(current_levels, total_label)
        }
        block_unordered[[col]] <- factor(block_unordered[[col]], levels = unique(current_levels))
      }
    }
    block <- block_unordered %>% arrange(across(all_of(grp_des)))
  }
  block
}

# ---------------------------------------------------------------------------- #
# --- Reporte espec\u00edfico para Medias ---
# ---------------------------------------------------------------------------- #
generate_mean_report <- function(hojas_list, filename, var, des, sufijo, decimales, designs, consolidated_df, nombre_indicador,
                                  lista_tests = NULL,
                                  snac = FALSE,
                                  mostrar_pct_fiable = FALSE,
                                  color_fiabilidad = FALSE,
                                  fuente = NULL,
                                  verbose = TRUE) {
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "1_Consolidado")
  write_clean_table(wb, "1_Consolidado", consolidated_df, startRow = 1, startCol = 1)

  num_fmt_string <- if (decimales > 0) paste0("#,##0.", paste(rep("0", decimales), collapse = "")) else "#,##0"
  numStyle <- openxlsx::createStyle(numFmt = num_fmt_string)
  testStyle <- openxlsx::createStyle(numFmt = "0.000")

  estilos <- list(numStyle, openxlsx::createStyle(numFmt = "#,##0"), numStyle, openxlsx::createStyle(numFmt = "#,##0"))
  titulos  <- c("Estimaci\u00f3n", "Poblaci\u00f3n expandida", "Error est\u00e1ndar", "Casos muestrales")
  metricas <- c("media", "N_pob", "se", "n_mues")
  met_suffix_list <- purrr::map(metricas, ~ paste0(.x, "_", sufijo))
  names(met_suffix_list) <- metricas
  total_row <- hojas_list[["nac"]]
  all_factor_levels <- get_all_levels(designs, des)
  bold_style <- createStyle(textDecoration = "bold")

  for (combo in names(hojas_list)) {
    if (snac && combo == "nac") next
    grp_des <- if (combo == "nac") character(0) else strsplit(combo, "__")[[1]]
    hoja_nm <- truncate_sheet_name(paste0("2_", combo), existing = names(wb))
    openxlsx::addWorksheet(wb, hoja_nm)

    openxlsx::writeData(wb, hoja_nm, nombre_indicador, startRow = 2, startCol = 1)
    openxlsx::addStyle(wb, hoja_nm, style = bold_style, rows = 2, cols = 1)
    openxlsx::writeData(wb, hoja_nm, "Tipo de c\u00e1lculo", startRow = 3, startCol = 1)

    df_combo_wide <- hojas_list[[combo]]

    fila <- 5
    max_col_metrics <- 0
    for (i in seq_along(titulos)) {
      metric_cols <- met_suffix_list[[metricas[i]]]
      block <- .make_continuous_block(df_combo_wide, combo, grp_des, metric_cols, sufijo, all_factor_levels, total_row)
      if (ncol(block) > max_col_metrics) max_col_metrics <- ncol(block)
      openxlsx::writeData(wb, hoja_nm, titulos[i], startRow = fila, startCol = 1)
      cols_to_merge <- 1:ncol(block)
      openxlsx::addStyle(wb, hoja_nm, style = hdrStyle, rows = fila, cols = cols_to_merge, gridExpand = TRUE)
      if (ncol(block) > 1) {
        openxlsx::mergeCells(wb, hoja_nm, cols = cols_to_merge, rows = fila)
      }
      write_clean_table(wb, hoja_nm, block, startRow = fila + 1, startCol = 1)
      if (combo != "nac") {
        merge_group_cells(wb, hoja_nm, block, grp_des, start_row = fila + 2)
      }
      numeric_cols <- (length(grp_des) + 1):ncol(block)
      data_rows <- (fila + 2):(fila + 1 + nrow(block))
      if(length(numeric_cols) > 0 && length(data_rows) > 0) {
        openxlsx::addStyle(wb, hoja_nm, style = estilos[[i]], rows = data_rows, cols = numeric_cols, gridExpand = TRUE, stack = TRUE)
      }
      if (i == 1 && color_fiabilidad) {
        block_for_color <- .make_continuous_block(df_combo_wide, combo, grp_des, metric_cols, sufijo, all_factor_levels, total_row)
        .apply_quality_colors(wb, hoja_nm, block_for_color, df_combo_wide, grp_des,
                              sufijo,
                              first_data_row = fila + 2L,
                              first_num_col  = length(grp_des) + 1L)
      }
      fila <- fila + nrow(block) + 3
    }

    # Nota de fiabilidad y fuente
    note_lines <- .build_quality_note(df_combo_wide, sufijo, grp_des, mostrar_pct = mostrar_pct_fiable)
    fila_nota <- fila + 1L
    fila_nota <- .write_quality_note(wb, hoja_nm, fila_nota, note_lines, max_col_metrics)
    .write_fuente(wb, hoja_nm, fila_nota + 1L, fuente, sufijo, max_col_metrics)

    if (!is.null(lista_tests) && (
          combo %in% names(lista_tests$intra_year) ||
          combo %in% names(lista_tests$against_last_year) ||
          combo %in% names(lista_tests$against_national))) {
      fila <- 5
      col_inicio_tests <- max_col_metrics + 3

      tests_combo <- lista_tests$intra_year[[combo]]
      for (sfx in names(tests_combo)) {
        test_df <- tests_combo[[sfx]]
        if (!is.null(test_df)) {
          titulo_test <- paste0("Test entre categor\u00edas a\u00f1o: ", sfx)
          openxlsx::writeData(wb, hoja_nm, titulo_test, startRow = fila, startCol = col_inicio_tests)
          cols_to_merge <- col_inicio_tests:(col_inicio_tests + ncol(test_df) - 1)
          openxlsx::mergeCells(wb, hoja_nm, cols = cols_to_merge, rows = fila)
          openxlsx::addStyle(wb, hoja_nm, style = hdrStyle, rows = fila, cols = cols_to_merge, gridExpand = TRUE)

          write_clean_table(wb, hoja_nm, test_df, startRow = fila + 1, startCol = col_inicio_tests)
          numeric_cols <- (col_inicio_tests + 1):(col_inicio_tests + ncol(test_df) - 1)
          data_rows <- (fila + 2):(fila + 1 + nrow(test_df))
          if(length(numeric_cols) > 0 && length(data_rows) > 0) {
            openxlsx::addStyle(wb, hoja_nm, style = testStyle, rows = data_rows, cols = numeric_cols, gridExpand = TRUE, stack = TRUE)
          }
          fila <- fila + nrow(test_df) + 3
        }
      }

      if (length(sufijo) > 1) {
        test_df_last <- lista_tests$against_last_year[[combo]]
        if (!is.null(test_df_last)) {
          titulo_test <- "Test contra \u00faltimo a\u00f1o:"
          openxlsx::writeData(wb, hoja_nm, titulo_test, startRow = fila, startCol = col_inicio_tests)
          cols_to_merge <- col_inicio_tests:(col_inicio_tests + ncol(test_df_last) - 1)
          openxlsx::mergeCells(wb, hoja_nm, cols = cols_to_merge, rows = fila)
          openxlsx::addStyle(wb, hoja_nm, style = hdrStyle, rows = fila, cols = cols_to_merge, gridExpand = TRUE)
          write_clean_table(wb, hoja_nm, test_df_last, startRow = fila + 1, startCol = col_inicio_tests)
          numeric_cols <- (col_inicio_tests + 1):(col_inicio_tests + ncol(test_df_last) - 1)
          data_rows <- (fila + 2):(fila + 1 + nrow(test_df_last))
          if(length(numeric_cols) > 0 && length(data_rows) > 0) {
            openxlsx::addStyle(wb, hoja_nm, style = testStyle, rows = data_rows, cols = numeric_cols, gridExpand = TRUE, stack = TRUE)
          }
          fila <- fila + nrow(test_df_last) + 3
        }

        test_df_nac <- lista_tests$against_national[[combo]]
        if (!is.null(test_df_nac)) {
          titulo_test <- "Test contra estimaci\u00f3n nacional:"
          openxlsx::writeData(wb, hoja_nm, titulo_test, startRow = fila, startCol = col_inicio_tests)
          cols_to_merge <- col_inicio_tests:(col_inicio_tests + ncol(test_df_nac) - 1)
          openxlsx::mergeCells(wb, hoja_nm, cols = cols_to_merge, rows = fila)
          openxlsx::addStyle(wb, hoja_nm, style = hdrStyle, rows = fila, cols = cols_to_merge, gridExpand = TRUE)
          write_clean_table(wb, hoja_nm, test_df_nac, startRow = fila + 1, startCol = col_inicio_tests)
          numeric_cols <- (col_inicio_tests + 1):(col_inicio_tests + ncol(test_df_nac) - 1)
          data_rows <- (fila + 2):(fila + 1 + nrow(test_df_nac))
          if(length(numeric_cols) > 0 && length(data_rows) > 0) {
            openxlsx::addStyle(wb, hoja_nm, style = testStyle, rows = data_rows, cols = numeric_cols, gridExpand = TRUE, stack = TRUE)
          }
        }
      }
    }
  }
  openxlsx::saveWorkbook(wb, filename, overwrite = TRUE)
  if (verbose) message("Reporte Excel de medias creado en: ", filename)
}

# ---------------------------------------------------------------------------- #
# --- Reporte espec\u00edfico para Totales ---
# ---------------------------------------------------------------------------- #
generate_total_report <- function(hojas_list, filename, var, des, sufijo, decimales, designs, consolidated_df, nombre_indicador,
                                   lista_tests = NULL,
                                   snac = FALSE,
                                   mostrar_pct_fiable = FALSE,
                                   color_fiabilidad = FALSE,
                                   fuente = NULL,
                                  verbose = TRUE) {
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "1_Consolidado")
  write_clean_table(wb, "1_Consolidado", consolidated_df, startRow = 1, startCol = 1)

  num_fmt_string <- if (decimales > 0) paste0("#,##0.", paste(rep("0", decimales), collapse = "")) else "#,##0"
  numStyle <- openxlsx::createStyle(numFmt = num_fmt_string)
  countStyle <- openxlsx::createStyle(numFmt = "#,##0")
  testStyle <- openxlsx::createStyle(numFmt = "0.000")

  estilos <- list(numStyle, countStyle, numStyle, countStyle)
  titulos  <- c("Estimaci\u00f3n", "Poblaci\u00f3n expandida", "Error est\u00e1ndar", "Casos muestrales")
  metricas <- c("total", "N_pob", "se", "n_mues")
  met_suffix_list <- purrr::map(metricas, ~ paste0(.x, "_", sufijo))
  names(met_suffix_list) <- metricas
  total_row <- hojas_list[["nac"]]
  all_factor_levels <- get_all_levels(designs, des)
  bold_style <- createStyle(textDecoration = "bold")

  for (combo in names(hojas_list)) {
    if (snac && combo == "nac") next
    grp_des <- if (combo == "nac") character(0) else strsplit(combo, "__")[[1]]
    hoja_nm <- truncate_sheet_name(paste0("2_", combo), existing = names(wb))
    openxlsx::addWorksheet(wb, hoja_nm)

    openxlsx::writeData(wb, hoja_nm, nombre_indicador, startRow = 2, startCol = 1)
    openxlsx::addStyle(wb, hoja_nm, style = bold_style, rows = 2, cols = 1)
    openxlsx::writeData(wb, hoja_nm, "Tipo de c\u00e1lculo", startRow = 3, startCol = 1)

    df_combo_wide <- hojas_list[[combo]]

    fila <- 5
    max_col_metrics <- 0
    for (i in seq_along(titulos)) {
      metric_cols <- met_suffix_list[[metricas[i]]]
      block <- .make_continuous_block(df_combo_wide, combo, grp_des, metric_cols, sufijo, all_factor_levels, total_row)
      if (ncol(block) > max_col_metrics) max_col_metrics <- ncol(block)
      openxlsx::writeData(wb, hoja_nm, titulos[i], startRow = fila, startCol = 1)
      cols_to_merge <- 1:ncol(block)
      openxlsx::addStyle(wb, hoja_nm, style = hdrStyle, rows = fila, cols = cols_to_merge, gridExpand = TRUE)
      if (ncol(block) > 1) {
        openxlsx::mergeCells(wb, hoja_nm, cols = cols_to_merge, rows = fila)
      }
      write_clean_table(wb, hoja_nm, block, startRow = fila + 1, startCol = 1)
      if (combo != "nac") {
        merge_group_cells(wb, hoja_nm, block, grp_des, start_row = fila + 2)
      }
      numeric_cols <- (length(grp_des) + 1):ncol(block)
      data_rows <- (fila + 2):(fila + 1 + nrow(block))
      if(length(numeric_cols) > 0 && length(data_rows) > 0) {
        openxlsx::addStyle(wb, hoja_nm, style = estilos[[i]], rows = data_rows, cols = numeric_cols, gridExpand = TRUE, stack = TRUE)
      }
      if (i == 1 && color_fiabilidad) {
        block_for_color <- .make_continuous_block(df_combo_wide, combo, grp_des, metric_cols, sufijo, all_factor_levels, total_row)
        .apply_quality_colors(wb, hoja_nm, block_for_color, df_combo_wide, grp_des,
                              sufijo,
                              first_data_row = fila + 2L,
                              first_num_col  = length(grp_des) + 1L)
      }
      fila <- fila + nrow(block) + 3
    }

    # Nota de fiabilidad y fuente
    note_lines <- .build_quality_note(df_combo_wide, sufijo, grp_des, mostrar_pct = mostrar_pct_fiable)
    fila_nota <- fila + 1L
    fila_nota <- .write_quality_note(wb, hoja_nm, fila_nota, note_lines, max_col_metrics)
    .write_fuente(wb, hoja_nm, fila_nota + 1L, fuente, sufijo, max_col_metrics)

    if (!is.null(lista_tests) && (
          combo %in% names(lista_tests$intra_year) ||
          combo %in% names(lista_tests$against_last_year) ||
          combo %in% names(lista_tests$against_national))) {
      fila <- 5
      col_inicio_tests <- max_col_metrics + 3

      tests_combo <- lista_tests$intra_year[[combo]]
      for (sfx in names(tests_combo)) {
        test_df <- tests_combo[[sfx]]
        if (!is.null(test_df)) {
          titulo_test <- paste0("Test entre categor\u00edas a\u00f1o: ", sfx)
          openxlsx::writeData(wb, hoja_nm, titulo_test, startRow = fila, startCol = col_inicio_tests)
          cols_to_merge <- col_inicio_tests:(col_inicio_tests + ncol(test_df) - 1)
          openxlsx::mergeCells(wb, hoja_nm, cols = cols_to_merge, rows = fila)
          openxlsx::addStyle(wb, hoja_nm, style = hdrStyle, rows = fila, cols = cols_to_merge, gridExpand = TRUE)

          write_clean_table(wb, hoja_nm, test_df, startRow = fila + 1, startCol = col_inicio_tests)
          numeric_cols <- (col_inicio_tests + 1):(col_inicio_tests + ncol(test_df) - 1)
          data_rows <- (fila + 2):(fila + 1 + nrow(test_df))
          if(length(numeric_cols) > 0 && length(data_rows) > 0) {
            openxlsx::addStyle(wb, hoja_nm, style = testStyle, rows = data_rows, cols = numeric_cols, gridExpand = TRUE, stack = TRUE)
          }
          fila <- fila + nrow(test_df) + 3
        }
      }

      if (length(sufijo) > 1) {
        test_df_last <- lista_tests$against_last_year[[combo]]
        if (!is.null(test_df_last)) {
          titulo_test <- "Test contra \u00faltimo a\u00f1o:"
          openxlsx::writeData(wb, hoja_nm, titulo_test, startRow = fila, startCol = col_inicio_tests)
          cols_to_merge <- col_inicio_tests:(col_inicio_tests + ncol(test_df_last) - 1)
          openxlsx::mergeCells(wb, hoja_nm, cols = cols_to_merge, rows = fila)
          openxlsx::addStyle(wb, hoja_nm, style = hdrStyle, rows = fila, cols = cols_to_merge, gridExpand = TRUE)
          write_clean_table(wb, hoja_nm, test_df_last, startRow = fila + 1, startCol = col_inicio_tests)
          numeric_cols <- (col_inicio_tests + 1):(col_inicio_tests + ncol(test_df_last) - 1)
          data_rows <- (fila + 2):(fila + 1 + nrow(test_df_last))
          if(length(numeric_cols) > 0 && length(data_rows) > 0) {
            openxlsx::addStyle(wb, hoja_nm, style = testStyle, rows = data_rows, cols = numeric_cols, gridExpand = TRUE, stack = TRUE)
          }
          fila <- fila + nrow(test_df_last) + 3
        }

        test_df_nac <- lista_tests$against_national[[combo]]
        if (!is.null(test_df_nac)) {
          titulo_test <- "Test contra estimaci\u00f3n nacional:"
          openxlsx::writeData(wb, hoja_nm, titulo_test, startRow = fila, startCol = col_inicio_tests)
          cols_to_merge <- col_inicio_tests:(col_inicio_tests + ncol(test_df_nac) - 1)
          openxlsx::mergeCells(wb, hoja_nm, cols = cols_to_merge, rows = fila)
          openxlsx::addStyle(wb, hoja_nm, style = hdrStyle, rows = fila, cols = cols_to_merge, gridExpand = TRUE)
          write_clean_table(wb, hoja_nm, test_df_nac, startRow = fila + 1, startCol = col_inicio_tests)
          numeric_cols <- (col_inicio_tests + 1):(col_inicio_tests + ncol(test_df_nac) - 1)
          data_rows <- (fila + 2):(fila + 1 + nrow(test_df_nac))
          if(length(numeric_cols) > 0 && length(data_rows) > 0) {
            openxlsx::addStyle(wb, hoja_nm, style = testStyle, rows = data_rows, cols = numeric_cols, gridExpand = TRUE, stack = TRUE)
          }
        }
      }
    }
  }
  openxlsx::saveWorkbook(wb, filename, overwrite = TRUE)
  if (verbose) message("Reporte Excel de totales creado en: ", filename)
}

# ---------------------------------------------------------------------------- #
# --- Reporte espec\u00edfico para Ratios ---
# ---------------------------------------------------------------------------- #
generate_ratio_report <- function(hojas_list, filename, var, des, sufijo, decimales, designs, consolidated_df, nombre_indicador,
                                   lista_tests = NULL,
                                   snac = FALSE,
                                   mostrar_pct_fiable = FALSE,
                                   color_fiabilidad = FALSE,
                                   fuente = NULL,
                                  verbose = TRUE) {
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "1_Consolidado")
  write_clean_table(wb, "1_Consolidado", consolidated_df, startRow = 1, startCol = 1)

  num_fmt_string <- if (decimales > 0) paste0("#,##0.", paste(rep("0", decimales), collapse = "")) else "#,##0"
  numStyle <- openxlsx::createStyle(numFmt = num_fmt_string)
  testStyle <- openxlsx::createStyle(numFmt = "0.000")

  estilos <- list(numStyle, openxlsx::createStyle(numFmt = "#,##0"), numStyle, openxlsx::createStyle(numFmt = "#,##0"))
  titulos  <- c("Estimaci\u00f3n", "Poblaci\u00f3n expandida", "Error est\u00e1ndar", "Casos muestrales")
  metricas <- c("ratio", "N_pob", "se", "n_mues")
  met_suffix_list <- purrr::map(metricas, ~ paste0(.x, "_", sufijo))
  names(met_suffix_list) <- metricas
  total_row <- hojas_list[["nac"]]
  all_factor_levels <- get_all_levels(designs, des)
  bold_style <- createStyle(textDecoration = "bold")

  for (combo in names(hojas_list)) {
    if (snac && combo == "nac") next
    grp_des <- if (combo == "nac") character(0) else strsplit(combo, "__")[[1]]
    hoja_nm <- truncate_sheet_name(paste0("2_", combo), existing = names(wb))
    openxlsx::addWorksheet(wb, hoja_nm)

    openxlsx::writeData(wb, hoja_nm, nombre_indicador, startRow = 2, startCol = 1)
    openxlsx::addStyle(wb, hoja_nm, style = bold_style, rows = 2, cols = 1)
    openxlsx::writeData(wb, hoja_nm, "Tipo de c\u00e1lculo", startRow = 3, startCol = 1)

    df_combo_wide <- hojas_list[[combo]]

    fila <- 5
    max_col_metrics <- 0
    for (i in seq_along(titulos)) {
      metric_cols <- met_suffix_list[[metricas[i]]]
      block <- .make_continuous_block(df_combo_wide, combo, grp_des, metric_cols, sufijo, all_factor_levels, total_row)
      if (ncol(block) > max_col_metrics) max_col_metrics <- ncol(block)
      openxlsx::writeData(wb, hoja_nm, titulos[i], startRow = fila, startCol = 1)
      cols_to_merge <- 1:ncol(block)
      openxlsx::addStyle(wb, hoja_nm, style = hdrStyle, rows = fila, cols = cols_to_merge, gridExpand = TRUE)
      if (ncol(block) > 1) {
        openxlsx::mergeCells(wb, hoja_nm, cols = cols_to_merge, rows = fila)
      }
      write_clean_table(wb, hoja_nm, block, startRow = fila + 1, startCol = 1)
      if (combo != "nac") {
        merge_group_cells(wb, hoja_nm, block, grp_des, start_row = fila + 2)
      }
      numeric_cols <- (length(grp_des) + 1):ncol(block)
      data_rows <- (fila + 2):(fila + 1 + nrow(block))
      if(length(numeric_cols) > 0 && length(data_rows) > 0) {
        openxlsx::addStyle(wb, hoja_nm, style = estilos[[i]], rows = data_rows, cols = numeric_cols, gridExpand = TRUE, stack = TRUE)
      }
      if (i == 1 && color_fiabilidad) {
        block_for_color <- .make_continuous_block(df_combo_wide, combo, grp_des, metric_cols, sufijo, all_factor_levels, total_row)
        .apply_quality_colors(wb, hoja_nm, block_for_color, df_combo_wide, grp_des,
                              sufijo,
                              first_data_row = fila + 2L,
                              first_num_col  = length(grp_des) + 1L)
      }
      fila <- fila + nrow(block) + 3
    }

    # Nota de fiabilidad y fuente
    note_lines <- .build_quality_note(df_combo_wide, sufijo, grp_des, mostrar_pct = mostrar_pct_fiable)
    fila_nota <- fila + 1L
    fila_nota <- .write_quality_note(wb, hoja_nm, fila_nota, note_lines, max_col_metrics)
    .write_fuente(wb, hoja_nm, fila_nota + 1L, fuente, sufijo, max_col_metrics)

    if (!is.null(lista_tests) && (
          combo %in% names(lista_tests$intra_year) ||
          combo %in% names(lista_tests$against_last_year) ||
          combo %in% names(lista_tests$against_national))) {
      fila <- 5
      col_inicio_tests <- max_col_metrics + 3

      tests_combo <- lista_tests$intra_year[[combo]]
      for (sfx in names(tests_combo)) {
        test_df <- tests_combo[[sfx]]
        if (!is.null(test_df)) {
          titulo_test <- paste0("Test entre categor\u00edas a\u00f1o: ", sfx)
          openxlsx::writeData(wb, hoja_nm, titulo_test, startRow = fila, startCol = col_inicio_tests)
          cols_to_merge <- col_inicio_tests:(col_inicio_tests + ncol(test_df) - 1)
          openxlsx::mergeCells(wb, hoja_nm, cols = cols_to_merge, rows = fila)
          openxlsx::addStyle(wb, hoja_nm, style = hdrStyle, rows = fila, cols = cols_to_merge, gridExpand = TRUE)

          write_clean_table(wb, hoja_nm, test_df, startRow = fila + 1, startCol = col_inicio_tests)
          numeric_cols <- (col_inicio_tests + 1):(col_inicio_tests + ncol(test_df) - 1)
          data_rows <- (fila + 2):(fila + 1 + nrow(test_df))
          if(length(numeric_cols) > 0 && length(data_rows) > 0) {
            openxlsx::addStyle(wb, hoja_nm, style = testStyle, rows = data_rows, cols = numeric_cols, gridExpand = TRUE, stack = TRUE)
          }
          fila <- fila + nrow(test_df) + 3
        }
      }

      if (length(sufijo) > 1) {
        test_df_last <- lista_tests$against_last_year[[combo]]
        if (!is.null(test_df_last)) {
          titulo_test <- "Test contra \u00faltimo a\u00f1o:"
          openxlsx::writeData(wb, hoja_nm, titulo_test, startRow = fila, startCol = col_inicio_tests)
          cols_to_merge <- col_inicio_tests:(col_inicio_tests + ncol(test_df_last) - 1)
          openxlsx::mergeCells(wb, hoja_nm, cols = cols_to_merge, rows = fila)
          openxlsx::addStyle(wb, hoja_nm, style = hdrStyle, rows = fila, cols = cols_to_merge, gridExpand = TRUE)
          write_clean_table(wb, hoja_nm, test_df_last, startRow = fila + 1, startCol = col_inicio_tests)
          numeric_cols <- (col_inicio_tests + 1):(col_inicio_tests + ncol(test_df_last) - 1)
          data_rows <- (fila + 2):(fila + 1 + nrow(test_df_last))
          if(length(numeric_cols) > 0 && length(data_rows) > 0) {
            openxlsx::addStyle(wb, hoja_nm, style = testStyle, rows = data_rows, cols = numeric_cols, gridExpand = TRUE, stack = TRUE)
          }
          fila <- fila + nrow(test_df_last) + 3
        }

        test_df_nac <- lista_tests$against_national[[combo]]
        if (!is.null(test_df_nac)) {
          titulo_test <- "Test contra estimaci\u00f3n nacional:"
          openxlsx::writeData(wb, hoja_nm, titulo_test, startRow = fila, startCol = col_inicio_tests)
          cols_to_merge <- col_inicio_tests:(col_inicio_tests + ncol(test_df_nac) - 1)
          openxlsx::mergeCells(wb, hoja_nm, cols = cols_to_merge, rows = fila)
          openxlsx::addStyle(wb, hoja_nm, style = hdrStyle, rows = fila, cols = cols_to_merge, gridExpand = TRUE)
          write_clean_table(wb, hoja_nm, test_df_nac, startRow = fila + 1, startCol = col_inicio_tests)
          numeric_cols <- (col_inicio_tests + 1):(col_inicio_tests + ncol(test_df_nac) - 1)
          data_rows <- (fila + 2):(fila + 1 + nrow(test_df_nac))
          if(length(numeric_cols) > 0 && length(data_rows) > 0) {
            openxlsx::addStyle(wb, hoja_nm, style = testStyle, rows = data_rows, cols = numeric_cols, gridExpand = TRUE, stack = TRUE)
          }
        }
      }
    }
  }
  openxlsx::saveWorkbook(wb, filename, overwrite = TRUE)
  if (verbose) message("Reporte Excel de ratios creado en: ", filename)
}

# ---------------------------------------------------------------------------- #
# --- Reporte espec\u00edfico para Cuantiles ---
# ---------------------------------------------------------------------------- #
generate_quantile_report <- function(hojas_list, filename, var, des, sufijo, cuant, decimales, designs, consolidated_df, nombre_indicador,
                                      lista_tests = NULL,
                                      snac = FALSE,
                                      mostrar_pct_fiable = FALSE,
                                      color_fiabilidad = FALSE,
                                      fuente = NULL,
                                  verbose = TRUE) {
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "1_Consolidado")
  write_clean_table(wb, "1_Consolidado", consolidated_df, startRow = 1, startCol = 1)

  num_fmt_string <- if (decimales > 0) paste0("#,##0.", paste(rep("0", decimales), collapse = "")) else "#,##0"
  numStyle <- openxlsx::createStyle(numFmt = num_fmt_string)
  testStyle <- openxlsx::createStyle(numFmt = "0.000")

  prob_label <- formatC(cuant, format = "f", digits = 2)
  estilos <- list(numStyle, openxlsx::createStyle(numFmt = "#,##0"), numStyle, openxlsx::createStyle(numFmt = "#,##0"))
  titulos  <- c(paste0("Estimaci\u00f3n (p = ", prob_label, ")"), "Poblaci\u00f3n expandida", "Error est\u00e1ndar", "Casos muestrales")
  metricas <- c("cuantil", "N_pob", "se", "n_mues")
  met_suffix_list <- purrr::map(metricas, ~ paste0(.x, "_", sufijo))
  names(met_suffix_list) <- metricas
  total_row <- hojas_list[["nac"]]
  all_factor_levels <- get_all_levels(designs, des)
  bold_style <- createStyle(textDecoration = "bold")

  for (combo in names(hojas_list)) {
    if (snac && combo == "nac") next
    grp_des <- if (combo == "nac") character(0) else strsplit(combo, "__")[[1]]
    hoja_nm <- truncate_sheet_name(paste0("2_", combo), existing = names(wb))
    openxlsx::addWorksheet(wb, hoja_nm)

    openxlsx::writeData(wb, hoja_nm, nombre_indicador, startRow = 2, startCol = 1)
    openxlsx::addStyle(wb, hoja_nm, style = bold_style, rows = 2, cols = 1)
    openxlsx::writeData(wb, hoja_nm, "Tipo de c\u00e1lculo", startRow = 3, startCol = 1)

    df_combo_wide <- hojas_list[[combo]]

    fila <- 5
    max_col_metrics <- 0
    for (i in seq_along(titulos)) {
      metric_cols <- met_suffix_list[[metricas[i]]]
      block <- .make_continuous_block(df_combo_wide, combo, grp_des, metric_cols, sufijo, all_factor_levels, total_row)
      if (ncol(block) > max_col_metrics) max_col_metrics <- ncol(block)
      openxlsx::writeData(wb, hoja_nm, titulos[i], startRow = fila, startCol = 1)
      cols_to_merge <- 1:ncol(block)
      openxlsx::addStyle(wb, hoja_nm, style = hdrStyle, rows = fila, cols = cols_to_merge, gridExpand = TRUE)
      if (ncol(block) > 1) {
        openxlsx::mergeCells(wb, hoja_nm, cols = cols_to_merge, rows = fila)
      }
      write_clean_table(wb, hoja_nm, block, startRow = fila + 1, startCol = 1)
      if (combo != "nac") {
        merge_group_cells(wb, hoja_nm, block, grp_des, start_row = fila + 2)
      }
      numeric_cols <- (length(grp_des) + 1):ncol(block)
      data_rows <- (fila + 2):(fila + 1 + nrow(block))
      if(length(numeric_cols) > 0 && length(data_rows) > 0) {
        openxlsx::addStyle(wb, hoja_nm, style = estilos[[i]], rows = data_rows, cols = numeric_cols, gridExpand = TRUE, stack = TRUE)
      }
      if (i == 1 && color_fiabilidad) {
        block_for_color <- .make_continuous_block(df_combo_wide, combo, grp_des, metric_cols, sufijo, all_factor_levels, total_row)
        .apply_quality_colors(wb, hoja_nm, block_for_color, df_combo_wide, grp_des,
                              sufijo,
                              first_data_row = fila + 2L,
                              first_num_col  = length(grp_des) + 1L)
      }
      fila <- fila + nrow(block) + 3
    }

    # Nota de fiabilidad y fuente
    note_lines <- .build_quality_note(df_combo_wide, sufijo, grp_des, mostrar_pct = mostrar_pct_fiable)
    fila_nota <- fila + 1L
    fila_nota <- .write_quality_note(wb, hoja_nm, fila_nota, note_lines, max_col_metrics)
    .write_fuente(wb, hoja_nm, fila_nota + 1L, fuente, sufijo, max_col_metrics)

    if (!is.null(lista_tests) && (
          combo %in% names(lista_tests$intra_year) ||
          combo %in% names(lista_tests$against_last_year) ||
          combo %in% names(lista_tests$against_national))) {
      fila <- 5
      col_inicio_tests <- max_col_metrics + 3

      tests_combo <- lista_tests$intra_year[[combo]]
      for (sfx in names(tests_combo)) {
        test_df <- tests_combo[[sfx]]
        if (!is.null(test_df)) {
          titulo_test <- paste0("Test entre categor\u00edas a\u00f1o: ", sfx)
          openxlsx::writeData(wb, hoja_nm, titulo_test, startRow = fila, startCol = col_inicio_tests)
          cols_to_merge <- col_inicio_tests:(col_inicio_tests + ncol(test_df) - 1)
          openxlsx::mergeCells(wb, hoja_nm, cols = cols_to_merge, rows = fila)
          openxlsx::addStyle(wb, hoja_nm, style = hdrStyle, rows = fila, cols = cols_to_merge, gridExpand = TRUE)

          write_clean_table(wb, hoja_nm, test_df, startRow = fila + 1, startCol = col_inicio_tests)
          numeric_cols <- (col_inicio_tests + 1):(col_inicio_tests + ncol(test_df) - 1)
          data_rows <- (fila + 2):(fila + 1 + nrow(test_df))
          if(length(numeric_cols) > 0 && length(data_rows) > 0) {
            openxlsx::addStyle(wb, hoja_nm, style = testStyle, rows = data_rows, cols = numeric_cols, gridExpand = TRUE, stack = TRUE)
          }
          fila <- fila + nrow(test_df) + 3
        }
      }

      if (length(sufijo) > 1) {
        test_df_last <- lista_tests$against_last_year[[combo]]
        if (!is.null(test_df_last)) {
          titulo_test <- "Test contra \u00faltimo a\u00f1o:"
          openxlsx::writeData(wb, hoja_nm, titulo_test, startRow = fila, startCol = col_inicio_tests)
          cols_to_merge <- col_inicio_tests:(col_inicio_tests + ncol(test_df_last) - 1)
          openxlsx::mergeCells(wb, hoja_nm, cols = cols_to_merge, rows = fila)
          openxlsx::addStyle(wb, hoja_nm, style = hdrStyle, rows = fila, cols = cols_to_merge, gridExpand = TRUE)
          write_clean_table(wb, hoja_nm, test_df_last, startRow = fila + 1, startCol = col_inicio_tests)
          numeric_cols <- (col_inicio_tests + 1):(col_inicio_tests + ncol(test_df_last) - 1)
          data_rows <- (fila + 2):(fila + 1 + nrow(test_df_last))
          if(length(numeric_cols) > 0 && length(data_rows) > 0) {
            openxlsx::addStyle(wb, hoja_nm, style = testStyle, rows = data_rows, cols = numeric_cols, gridExpand = TRUE, stack = TRUE)
          }
          fila <- fila + nrow(test_df_last) + 3
        }

        test_df_nac <- lista_tests$against_national[[combo]]
        if (!is.null(test_df_nac)) {
          titulo_test <- "Test contra estimaci\u00f3n nacional:"
          openxlsx::writeData(wb, hoja_nm, titulo_test, startRow = fila, startCol = col_inicio_tests)
          cols_to_merge <- col_inicio_tests:(col_inicio_tests + ncol(test_df_nac) - 1)
          openxlsx::mergeCells(wb, hoja_nm, cols = cols_to_merge, rows = fila)
          openxlsx::addStyle(wb, hoja_nm, style = hdrStyle, rows = fila, cols = cols_to_merge, gridExpand = TRUE)
          write_clean_table(wb, hoja_nm, test_df_nac, startRow = fila + 1, startCol = col_inicio_tests)
          numeric_cols <- (col_inicio_tests + 1):(col_inicio_tests + ncol(test_df_nac) - 1)
          data_rows <- (fila + 2):(fila + 1 + nrow(test_df_nac))
          if(length(numeric_cols) > 0 && length(data_rows) > 0) {
            openxlsx::addStyle(wb, hoja_nm, style = testStyle, rows = data_rows, cols = numeric_cols, gridExpand = TRUE, stack = TRUE)
          }
        }
      }
    }
  }
  openxlsx::saveWorkbook(wb, filename, overwrite = TRUE)
  if (verbose) message("Reporte Excel de cuantiles creado en: ", filename)
}
