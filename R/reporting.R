# ---------------------------------------------------------------------------- #
# Archivo: reporting.R (VERSIÓN CON ESCRITURA DE TESTS DE SIGNIFICANCIA)
# ---------------------------------------------------------------------------- #

# --- Ayudante para fusionar celdas ---
merge_group_cells <- function(wb, sheet, df, grp_des, start_row, ref_df = NULL) {
  if (length(grp_des) == 0) return()
  values_df <- ref_df %||% df
  for (col_idx in seq_along(grp_des)) {
    col_name <- grp_des[col_idx]
    if (!col_name %in% names(values_df) || !col_name %in% names(df)) next
    runs <- rle(as.character(values_df[[col_name]]))
    run_lengths <- runs$lengths
    end_rows_r <- cumsum(run_lengths)
    start_rows_r <- end_rows_r - run_lengths + 1
    for (j in seq_along(run_lengths)) {
      if (run_lengths[j] > 1 && !grepl("Total", runs$values[j], ignore.case = TRUE)) {
        merge_rows <- start_row:(start_row + nrow(df) - 1)
        row_start <- merge_rows[start_rows_r[j]]
        row_end <- merge_rows[end_rows_r[j]]
        openxlsx::mergeCells(wb, sheet, cols = col_idx, rows = row_start:row_end)
      }
    }
  }
}

apply_significance_format <- function(wb, sheet, df, grp_cols, start_row, start_col, numeric_style) {
  if (is.null(df) || nrow(df) == 0) return()
  merge_reference <- attr(df, "_merge_reference")
  merge_cols <- intersect(grp_cols, names(merge_reference %||% df))
  if (length(merge_cols) > 0) {
    merge_group_cells(
      wb,
      sheet,
      df,
      merge_cols,
      start_row = start_row + 1,
      ref_df = merge_reference %||% df
    )
  }
  numeric_cols <- (start_col + 1):(start_col + ncol(df) - 1)
  data_rows <- (start_row + 1):(start_row + nrow(df))
  if (length(numeric_cols) > 0 && length(data_rows) > 0) {
    openxlsx::addStyle(
      wb,
      sheet,
      style = numeric_style,
      rows = data_rows,
      cols = numeric_cols,
      gridExpand = TRUE,
      stack = TRUE
    )
  }
}

# --- Obtener niveles de factores sin ordenar alfabéticamente ---
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
    return(joined_df)
  }
  hojas_list <- purrr::map(combos_nombres, combinar_por_combo)
  names(hojas_list) <- combos_nombres
  return(hojas_list)
}

# --- Reporte específico para Proporciones ---
generate_prop_report <- function(hojas_list, filename, var, des, sufijo, porcentaje, decimales, designs, consolidated_df, nombre_indicador, lista_tests = NULL) {
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
    grp_des <- if (combo == "nac") character(0) else strsplit(combo, "__")[[1]]
    hoja_nm <- truncate_sheet_name(paste0("2_", combo))
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
      fila <- fila + nrow(block) + 3
    }

    if (!is.null(lista_tests) && combo %in% names(lista_tests$intra_year)) {
      fila <- 5
      col_inicio_tests <- max_col_metrics + 3

      tests_combo <- lista_tests$intra_year[[combo]]
      for (sfx in names(tests_combo)) {
        test_df <- tests_combo[[sfx]]
        if (!is.null(test_df)) {
          titulo_test <- paste0("Test entre categorías año: ", sfx)
          openxlsx::writeData(wb, hoja_nm, titulo_test, startRow = fila, startCol = col_inicio_tests)
          cols_to_merge <- col_inicio_tests:(col_inicio_tests + ncol(test_df) - 1)
          openxlsx::mergeCells(wb, hoja_nm, cols = cols_to_merge, rows = fila)
          openxlsx::addStyle(wb, hoja_nm, style = hdrStyle, rows = fila, cols = col_inicio_tests)

          write_clean_table(wb, hoja_nm, test_df, startRow = fila + 1, startCol = col_inicio_tests)
          apply_significance_format(
            wb,
            hoja_nm,
            test_df,
            grp_des,
            start_row = fila + 1,
            start_col = col_inicio_tests,
            numeric_style = testStyle
          )
          fila <- fila + nrow(test_df) + 3
        }
      }

      if (length(sufijo) > 1) {
        test_df_last <- lista_tests$against_last_year[[combo]]
        if (!is.null(test_df_last)) {
          titulo_test <- "Test contra último año:"
          openxlsx::writeData(wb, hoja_nm, titulo_test, startRow = fila, startCol = col_inicio_tests)
          cols_to_merge <- col_inicio_tests:(col_inicio_tests + ncol(test_df_last) - 1)
          openxlsx::mergeCells(wb, hoja_nm, cols = cols_to_merge, rows = fila)
          openxlsx::addStyle(wb, hoja_nm, style = hdrStyle, rows = fila, cols = col_inicio_tests)
          write_clean_table(wb, hoja_nm, test_df_last, startRow = fila + 1, startCol = col_inicio_tests)
          apply_significance_format(
            wb,
            hoja_nm,
            test_df_last,
            grp_des,
            start_row = fila + 1,
            start_col = col_inicio_tests,
            numeric_style = testStyle
          )
          fila <- fila + nrow(test_df_last) + 3
        }

        test_df_nac <- lista_tests$against_national[[combo]]
        if (!is.null(test_df_nac)) {
          titulo_test <- "Test contra estimación nacional:"
          openxlsx::writeData(wb, hoja_nm, titulo_test, startRow = fila, startCol = col_inicio_tests)
          cols_to_merge <- col_inicio_tests:(col_inicio_tests + ncol(test_df_nac) - 1)
          openxlsx::mergeCells(wb, hoja_nm, cols = cols_to_merge, rows = fila)
          openxlsx::addStyle(wb, hoja_nm, style = hdrStyle, rows = fila, cols = col_inicio_tests)
          write_clean_table(wb, hoja_nm, test_df_nac, startRow = fila + 1, startCol = col_inicio_tests)
          apply_significance_format(
            wb,
            hoja_nm,
            test_df_nac,
            grp_des,
            start_row = fila + 1,
            start_col = col_inicio_tests,
            numeric_style = testStyle
          )
        }
      }



    }
  }
  openxlsx::saveWorkbook(wb, filename, overwrite = TRUE)
  message("Reporte Excel de proporciones creado en: ", filename)
}

# --- Reporte específico para Medias ---
generate_mean_report <- function(hojas_list, filename, var, des, sufijo, decimales, designs, consolidated_df, nombre_indicador, lista_tests = NULL) {
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
    grp_des <- if (combo == "nac") character(0) else strsplit(combo, "__")[[1]]
    hoja_nm <- truncate_sheet_name(paste0("2_", combo))
    openxlsx::addWorksheet(wb, hoja_nm)

    openxlsx::writeData(wb, hoja_nm, nombre_indicador, startRow = 2, startCol = 1)
    openxlsx::addStyle(wb, hoja_nm, style = bold_style, rows = 2, cols = 1)
    openxlsx::writeData(wb, hoja_nm, "Tipo de c\u00e1lculo", startRow = 3, startCol = 1)

    df_combo_wide <- hojas_list[[combo]]

    fila <- 5
    max_col_metrics <- 0
    for (i in seq_along(titulos)) {
      metric_name <- metricas[i]
      metric_cols <- met_suffix_list[[metric_name]]
      if (combo == "nac") {
        block <- df_combo_wide %>%
          select(nivel, all_of(metric_cols)) %>%
          rename_with(~ sufijo, all_of(metric_cols))
      } else {
        block_main <- df_combo_wide %>% select(all_of(c(grp_des, metric_cols)))
        tot_vals <- total_row %>% select(all_of(metric_cols))
        tot_line <- as.list(rep(".", length(grp_des)))
        names(tot_line) <- grp_des
        tot_line[[grp_des[1]]] <- "Total pais"
        tot_line_df <- bind_cols(as_tibble(tot_line), tot_vals)
        block_unordered <- bind_rows(block_main, tot_line_df) %>%
          rename_with(~ sufijo, all_of(metric_cols))
        factor_cols <- intersect(names(all_factor_levels), names(block_unordered))
        for(col in factor_cols) {
          if(col %in% names(block_unordered)) {
            current_levels <- all_factor_levels[[col]]
            if(any(grepl("Total", block_unordered[[col]], ignore.case = TRUE))) {
              total_label <- unique(grep("Total", block_unordered[[col]], value = TRUE, ignore.case = TRUE))
              current_levels <- c(current_levels, total_label)
            }
            block_unordered[[col]] <- factor(block_unordered[[col]], levels = unique(current_levels))
          }
        }
        block <- block_unordered %>% arrange(across(all_of(grp_des)))
      }
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
      fila <- fila + nrow(block) + 3
    }

    if (!is.null(lista_tests) && combo %in% names(lista_tests$intra_year)) {
      fila <- 5
      col_inicio_tests <- max_col_metrics + 3

      tests_combo <- lista_tests$intra_year[[combo]]
      for (sfx in names(tests_combo)) {
        test_df <- tests_combo[[sfx]]
        if (!is.null(test_df)) {
          titulo_test <- paste0("Test entre categorías año: ", sfx)
          openxlsx::writeData(wb, hoja_nm, titulo_test, startRow = fila, startCol = col_inicio_tests)
          cols_to_merge <- col_inicio_tests:(col_inicio_tests + ncol(test_df) - 1)
          openxlsx::mergeCells(wb, hoja_nm, cols = cols_to_merge, rows = fila)
          openxlsx::addStyle(wb, hoja_nm, style = hdrStyle, rows = fila, cols = col_inicio_tests)

          write_clean_table(wb, hoja_nm, test_df, startRow = fila + 1, startCol = col_inicio_tests)
          apply_significance_format(
            wb,
            hoja_nm,
            test_df,
            grp_des,
            start_row = fila + 1,
            start_col = col_inicio_tests,
            numeric_style = testStyle
          )
          fila <- fila + nrow(test_df) + 3
        }
      }

      if (length(sufijo) > 1) {
        test_df_last <- lista_tests$against_last_year[[combo]]
        if (!is.null(test_df_last)) {
          titulo_test <- "Test contra último año:"
          openxlsx::writeData(wb, hoja_nm, titulo_test, startRow = fila, startCol = col_inicio_tests)
          cols_to_merge <- col_inicio_tests:(col_inicio_tests + ncol(test_df_last) - 1)
          openxlsx::mergeCells(wb, hoja_nm, cols = cols_to_merge, rows = fila)
          openxlsx::addStyle(wb, hoja_nm, style = hdrStyle, rows = fila, cols = col_inicio_tests)
          write_clean_table(wb, hoja_nm, test_df_last, startRow = fila + 1, startCol = col_inicio_tests)
          apply_significance_format(
            wb,
            hoja_nm,
            test_df_last,
            grp_des,
            start_row = fila + 1,
            start_col = col_inicio_tests,
            numeric_style = testStyle
          )
          fila <- fila + nrow(test_df_last) + 3
        }

        test_df_nac <- lista_tests$against_national[[combo]]
        if (!is.null(test_df_nac)) {
          titulo_test <- "Test contra estimación nacional:"
          openxlsx::writeData(wb, hoja_nm, titulo_test, startRow = fila, startCol = col_inicio_tests)
          cols_to_merge <- col_inicio_tests:(col_inicio_tests + ncol(test_df_nac) - 1)
          openxlsx::mergeCells(wb, hoja_nm, cols = cols_to_merge, rows = fila)
          openxlsx::addStyle(wb, hoja_nm, style = hdrStyle, rows = fila, cols = col_inicio_tests)
          write_clean_table(wb, hoja_nm, test_df_nac, startRow = fila + 1, startCol = col_inicio_tests)
          apply_significance_format(
            wb,
            hoja_nm,
            test_df_nac,
            grp_des,
            start_row = fila + 1,
            start_col = col_inicio_tests,
            numeric_style = testStyle
          )
        }
      }



    }
  }
  openxlsx::saveWorkbook(wb, filename, overwrite = TRUE)
  message("Reporte Excel de medias creado en: ", filename)
}

# --- Reporte específico para Totales ---
generate_total_report <- function(hojas_list, filename, var, des, sufijo, decimales, designs, consolidated_df, nombre_indicador, lista_tests = NULL) {
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "1_Consolidado")
  write_clean_table(wb, "1_Consolidado", consolidated_df, startRow = 1, startCol = 1)

  num_fmt_string <- if (decimales > 0) paste0("#,##0.", paste(rep("0", decimales), collapse = "")) else "#,##0"
  numStyle <- openxlsx::createStyle(numFmt = num_fmt_string)
  countStyle <- openxlsx::createStyle(numFmt = "#,##0")
  testStyle <- openxlsx::createStyle(numFmt = "0.000")

  estilos <- list(numStyle, countStyle, numStyle, countStyle)
  titulos  <- c("Estimación", "Población expandida", "Error estándar", "Casos muestrales")
  metricas <- c("total", "N_pob", "se", "n_mues")
  met_suffix_list <- purrr::map(metricas, ~ paste0(.x, "_", sufijo))
  names(met_suffix_list) <- metricas
  total_row <- hojas_list[["nac"]]
  all_factor_levels <- get_all_levels(designs, des)
  bold_style <- createStyle(textDecoration = "bold")

  for (combo in names(hojas_list)) {
    grp_des <- if (combo == "nac") character(0) else strsplit(combo, "__")[[1]]
    hoja_nm <- truncate_sheet_name(paste0("2_", combo))
    openxlsx::addWorksheet(wb, hoja_nm)

    openxlsx::writeData(wb, hoja_nm, nombre_indicador, startRow = 2, startCol = 1)
    openxlsx::addStyle(wb, hoja_nm, style = bold_style, rows = 2, cols = 1)
    openxlsx::writeData(wb, hoja_nm, "Tipo de cálculo", startRow = 3, startCol = 1)

    df_combo_wide <- hojas_list[[combo]]

    fila <- 5
    max_col_metrics <- 0
    for (i in seq_along(titulos)) {
      metric_name <- metricas[i]
      metric_cols <- met_suffix_list[[metric_name]]
      if (combo == "nac") {
        block <- df_combo_wide %>%
          select(nivel, all_of(metric_cols)) %>%
          rename_with(~ sufijo, all_of(metric_cols))
      } else {
        block_main <- df_combo_wide %>% select(all_of(c(grp_des, metric_cols)))
        tot_vals <- total_row %>% select(all_of(metric_cols))
        tot_line <- as.list(rep(".", length(grp_des)))
        names(tot_line) <- grp_des
        tot_line[[grp_des[1]]] <- "Total pais"
        tot_line_df <- bind_cols(as_tibble(tot_line), tot_vals)
        block_unordered <- bind_rows(block_main, tot_line_df) %>%
          rename_with(~ sufijo, all_of(metric_cols))
        factor_cols <- intersect(names(all_factor_levels), names(block_unordered))
        for(col in factor_cols) {
          if(col %in% names(block_unordered)) {
            current_levels <- all_factor_levels[[col]]
            if(any(grepl("Total", block_unordered[[col]], ignore.case = TRUE))) {
              total_label <- unique(grep("Total", block_unordered[[col]], value = TRUE, ignore.case = TRUE))
              current_levels <- c(current_levels, total_label)
            }
            block_unordered[[col]] <- factor(block_unordered[[col]], levels = unique(current_levels))
          }
        }
        block <- block_unordered %>% arrange(across(all_of(grp_des)))
      }
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
      fila <- fila + nrow(block) + 3
    }

    if (!is.null(lista_tests) && combo %in% names(lista_tests$intra_year)) {
      fila <- 5
      col_inicio_tests <- max_col_metrics + 3

      tests_combo <- lista_tests$intra_year[[combo]]
      for (sfx in names(tests_combo)) {
        test_df <- tests_combo[[sfx]]
        if (!is.null(test_df)) {
          titulo_test <- paste0("Test entre categorías año: ", sfx)
          openxlsx::writeData(wb, hoja_nm, titulo_test, startRow = fila, startCol = col_inicio_tests)
          cols_to_merge <- col_inicio_tests:(col_inicio_tests + ncol(test_df) - 1)
          openxlsx::mergeCells(wb, hoja_nm, cols = cols_to_merge, rows = fila)
          openxlsx::addStyle(wb, hoja_nm, style = hdrStyle, rows = fila, cols = col_inicio_tests)

          write_clean_table(wb, hoja_nm, test_df, startRow = fila + 1, startCol = col_inicio_tests)
          apply_significance_format(
            wb,
            hoja_nm,
            test_df,
            grp_des,
            start_row = fila + 1,
            start_col = col_inicio_tests,
            numeric_style = testStyle
          )
          fila <- fila + nrow(test_df) + 3
        }
      }

      if (length(sufijo) > 1) {
        test_df_last <- lista_tests$against_last_year[[combo]]
        if (!is.null(test_df_last)) {
          titulo_test <- "Test contra último año:"
          openxlsx::writeData(wb, hoja_nm, titulo_test, startRow = fila, startCol = col_inicio_tests)
          cols_to_merge <- col_inicio_tests:(col_inicio_tests + ncol(test_df_last) - 1)
          openxlsx::mergeCells(wb, hoja_nm, cols = cols_to_merge, rows = fila)
          openxlsx::addStyle(wb, hoja_nm, style = hdrStyle, rows = fila, cols = col_inicio_tests)
          write_clean_table(wb, hoja_nm, test_df_last, startRow = fila + 1, startCol = col_inicio_tests)
          apply_significance_format(
            wb,
            hoja_nm,
            test_df_last,
            grp_des,
            start_row = fila + 1,
            start_col = col_inicio_tests,
            numeric_style = testStyle
          )
          fila <- fila + nrow(test_df_last) + 3
        }

        test_df_nac <- lista_tests$against_national[[combo]]
        if (!is.null(test_df_nac)) {
          titulo_test <- "Test contra estimación nacional:"
          openxlsx::writeData(wb, hoja_nm, titulo_test, startRow = fila, startCol = col_inicio_tests)
          cols_to_merge <- col_inicio_tests:(col_inicio_tests + ncol(test_df_nac) - 1)
          openxlsx::mergeCells(wb, hoja_nm, cols = cols_to_merge, rows = fila)
          openxlsx::addStyle(wb, hoja_nm, style = hdrStyle, rows = fila, cols = col_inicio_tests)
          write_clean_table(wb, hoja_nm, test_df_nac, startRow = fila + 1, startCol = col_inicio_tests)
          apply_significance_format(
            wb,
            hoja_nm,
            test_df_nac,
            grp_des,
            start_row = fila + 1,
            start_col = col_inicio_tests,
            numeric_style = testStyle
          )
        }
      }



    }
  }
  openxlsx::saveWorkbook(wb, filename, overwrite = TRUE)
  message("Reporte Excel de totales creado en: ", filename)
}

# --- Reporte específico para Ratios ---
generate_ratio_report <- function(hojas_list, filename, var, des, sufijo, decimales, designs, consolidated_df, nombre_indicador, lista_tests = NULL) {
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "1_Consolidado")
  write_clean_table(wb, "1_Consolidado", consolidated_df, startRow = 1, startCol = 1)

  num_fmt_string <- if (decimales > 0) paste0("#,##0.", paste(rep("0", decimales), collapse = "")) else "#,##0"
  numStyle <- openxlsx::createStyle(numFmt = num_fmt_string)
  testStyle <- openxlsx::createStyle(numFmt = "0.000")

  estilos <- list(numStyle, openxlsx::createStyle(numFmt = "#,##0"), numStyle, openxlsx::createStyle(numFmt = "#,##0"))
  titulos  <- c("Estimación", "Población expandida", "Error estándar", "Casos muestrales")
  metricas <- c("ratio", "N_pob", "se", "n_mues")
  met_suffix_list <- purrr::map(metricas, ~ paste0(.x, "_", sufijo))
  names(met_suffix_list) <- metricas
  total_row <- hojas_list[["nac"]]
  all_factor_levels <- get_all_levels(designs, des)
  bold_style <- createStyle(textDecoration = "bold")

  for (combo in names(hojas_list)) {
    grp_des <- if (combo == "nac") character(0) else strsplit(combo, "__")[[1]]
    hoja_nm <- truncate_sheet_name(paste0("2_", combo))
    openxlsx::addWorksheet(wb, hoja_nm)

    openxlsx::writeData(wb, hoja_nm, nombre_indicador, startRow = 2, startCol = 1)
    openxlsx::addStyle(wb, hoja_nm, style = bold_style, rows = 2, cols = 1)
    openxlsx::writeData(wb, hoja_nm, "Tipo de cálculo", startRow = 3, startCol = 1)

    df_combo_wide <- hojas_list[[combo]]

    fila <- 5
    max_col_metrics <- 0
    for (i in seq_along(titulos)) {
      metric_name <- metricas[i]
      metric_cols <- met_suffix_list[[metric_name]]
      if (combo == "nac") {
        block <- df_combo_wide %>%
          select(nivel, all_of(metric_cols)) %>%
          rename_with(~ sufijo, all_of(metric_cols))
      } else {
        block_main <- df_combo_wide %>% select(all_of(c(grp_des, metric_cols)))
        tot_vals <- total_row %>% select(all_of(metric_cols))
        tot_line <- as.list(rep(".", length(grp_des)))
        names(tot_line) <- grp_des
        tot_line[[grp_des[1]]] <- "Total pais"
        tot_line_df <- bind_cols(as_tibble(tot_line), tot_vals)
        block_unordered <- bind_rows(block_main, tot_line_df) %>%
          rename_with(~ sufijo, all_of(metric_cols))
        factor_cols <- intersect(names(all_factor_levels), names(block_unordered))
        for(col in factor_cols) {
          if(col %in% names(block_unordered)) {
            current_levels <- all_factor_levels[[col]]
            if(any(grepl("Total", block_unordered[[col]], ignore.case = TRUE))) {
              total_label <- unique(grep("Total", block_unordered[[col]], value = TRUE, ignore.case = TRUE))
              current_levels <- c(current_levels, total_label)
            }
            block_unordered[[col]] <- factor(block_unordered[[col]], levels = unique(current_levels))
          }
        }
        block <- block_unordered %>% arrange(across(all_of(grp_des)))
      }
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
      fila <- fila + nrow(block) + 3
    }

    if (!is.null(lista_tests) && combo %in% names(lista_tests$intra_year)) {
      fila <- 5
      col_inicio_tests <- max_col_metrics + 3

      tests_combo <- lista_tests$intra_year[[combo]]
      for (sfx in names(tests_combo)) {
        test_df <- tests_combo[[sfx]]
        if (!is.null(test_df)) {
          titulo_test <- paste0("Test entre categorías año: ", sfx)
          openxlsx::writeData(wb, hoja_nm, titulo_test, startRow = fila, startCol = col_inicio_tests)
          cols_to_merge <- col_inicio_tests:(col_inicio_tests + ncol(test_df) - 1)
          openxlsx::mergeCells(wb, hoja_nm, cols = cols_to_merge, rows = fila)
          openxlsx::addStyle(wb, hoja_nm, style = hdrStyle, rows = fila, cols = col_inicio_tests)

          write_clean_table(wb, hoja_nm, test_df, startRow = fila + 1, startCol = col_inicio_tests)
          apply_significance_format(
            wb,
            hoja_nm,
            test_df,
            grp_des,
            start_row = fila + 1,
            start_col = col_inicio_tests,
            numeric_style = testStyle
          )
          fila <- fila + nrow(test_df) + 3
        }
      }

      if (length(sufijo) > 1) {
        test_df_last <- lista_tests$against_last_year[[combo]]
        if (!is.null(test_df_last)) {
          titulo_test <- "Test contra último año:"
          openxlsx::writeData(wb, hoja_nm, titulo_test, startRow = fila, startCol = col_inicio_tests)
          cols_to_merge <- col_inicio_tests:(col_inicio_tests + ncol(test_df_last) - 1)
          openxlsx::mergeCells(wb, hoja_nm, cols = cols_to_merge, rows = fila)
          openxlsx::addStyle(wb, hoja_nm, style = hdrStyle, rows = fila, cols = col_inicio_tests)
          write_clean_table(wb, hoja_nm, test_df_last, startRow = fila + 1, startCol = col_inicio_tests)
          apply_significance_format(
            wb,
            hoja_nm,
            test_df_last,
            grp_des,
            start_row = fila + 1,
            start_col = col_inicio_tests,
            numeric_style = testStyle
          )
          fila <- fila + nrow(test_df_last) + 3
        }

        test_df_nac <- lista_tests$against_national[[combo]]
        if (!is.null(test_df_nac)) {
          titulo_test <- "Test contra estimación nacional:"
          openxlsx::writeData(wb, hoja_nm, titulo_test, startRow = fila, startCol = col_inicio_tests)
          cols_to_merge <- col_inicio_tests:(col_inicio_tests + ncol(test_df_nac) - 1)
          openxlsx::mergeCells(wb, hoja_nm, cols = cols_to_merge, rows = fila)
          openxlsx::addStyle(wb, hoja_nm, style = hdrStyle, rows = fila, cols = col_inicio_tests)
          write_clean_table(wb, hoja_nm, test_df_nac, startRow = fila + 1, startCol = col_inicio_tests)
          apply_significance_format(
            wb,
            hoja_nm,
            test_df_nac,
            grp_des,
            start_row = fila + 1,
            start_col = col_inicio_tests,
            numeric_style = testStyle
          )
        }
      }



    }
  }
  openxlsx::saveWorkbook(wb, filename, overwrite = TRUE)
  message("Reporte Excel de ratios creado en: ", filename)
}

# --- Reporte específico para Cuantiles ---
generate_quantile_report <- function(hojas_list, filename, var, des, sufijo, cuant, decimales, designs, consolidated_df, nombre_indicador, lista_tests = NULL) {
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "1_Consolidado")
  write_clean_table(wb, "1_Consolidado", consolidated_df, startRow = 1, startCol = 1)

  num_fmt_string <- if (decimales > 0) paste0("#,##0.", paste(rep("0", decimales), collapse = "")) else "#,##0"
  numStyle <- openxlsx::createStyle(numFmt = num_fmt_string)
  testStyle <- openxlsx::createStyle(numFmt = "0.000")

  prob_label <- formatC(cuant, format = "f", digits = 2)
  estilos <- list(numStyle, openxlsx::createStyle(numFmt = "#,##0"), numStyle, openxlsx::createStyle(numFmt = "#,##0"))
  titulos  <- c(paste0("Estimación (p = ", prob_label, ")"), "Población expandida", "Error estándar", "Casos muestrales")
  metricas <- c("cuantil", "N_pob", "se", "n_mues")
  met_suffix_list <- purrr::map(metricas, ~ paste0(.x, "_", sufijo))
  names(met_suffix_list) <- metricas
  total_row <- hojas_list[["nac"]]
  all_factor_levels <- get_all_levels(designs, des)
  bold_style <- createStyle(textDecoration = "bold")

  for (combo in names(hojas_list)) {
    grp_des <- if (combo == "nac") character(0) else strsplit(combo, "__")[[1]]
    hoja_nm <- truncate_sheet_name(paste0("2_", combo))
    openxlsx::addWorksheet(wb, hoja_nm)

    openxlsx::writeData(wb, hoja_nm, nombre_indicador, startRow = 2, startCol = 1)
    openxlsx::addStyle(wb, hoja_nm, style = bold_style, rows = 2, cols = 1)
    openxlsx::writeData(wb, hoja_nm, "Tipo de cálculo", startRow = 3, startCol = 1)

    df_combo_wide <- hojas_list[[combo]]

    fila <- 5
    max_col_metrics <- 0
    for (i in seq_along(titulos)) {
      metric_name <- metricas[i]
      metric_cols <- met_suffix_list[[metric_name]]
      if (combo == "nac") {
        block <- df_combo_wide %>%
          select(nivel, all_of(metric_cols)) %>%
          rename_with(~ sufijo, all_of(metric_cols))
      } else {
        block_main <- df_combo_wide %>% select(all_of(c(grp_des, metric_cols)))
        tot_vals <- total_row %>% select(all_of(metric_cols))
        tot_line <- as.list(rep(".", length(grp_des)))
        names(tot_line) <- grp_des
        tot_line[[grp_des[1]]] <- "Total pais"
        tot_line_df <- bind_cols(as_tibble(tot_line), tot_vals)
        block_unordered <- bind_rows(block_main, tot_line_df) %>%
          rename_with(~ sufijo, all_of(metric_cols))
        factor_cols <- intersect(names(all_factor_levels), names(block_unordered))
        for(col in factor_cols) {
          if(col %in% names(block_unordered)) {
            current_levels <- all_factor_levels[[col]]
            if(any(grepl("Total", block_unordered[[col]], ignore.case = TRUE))) {
              total_label <- unique(grep("Total", block_unordered[[col]], value = TRUE, ignore.case = TRUE))
              current_levels <- c(current_levels, total_label)
            }
            block_unordered[[col]] <- factor(block_unordered[[col]], levels = unique(current_levels))
          }
        }
        block <- block_unordered %>% arrange(across(all_of(grp_des)))
      }
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
      fila <- fila + nrow(block) + 3
    }

    if (!is.null(lista_tests) && combo %in% names(lista_tests$intra_year)) {
      fila <- 5
      col_inicio_tests <- max_col_metrics + 3

      tests_combo <- lista_tests$intra_year[[combo]]
      for (sfx in names(tests_combo)) {
        test_df <- tests_combo[[sfx]]
        if (!is.null(test_df)) {
          titulo_test <- paste0("Test entre categorías año: ", sfx)
          openxlsx::writeData(wb, hoja_nm, titulo_test, startRow = fila, startCol = col_inicio_tests)
          cols_to_merge <- col_inicio_tests:(col_inicio_tests + ncol(test_df) - 1)
          openxlsx::mergeCells(wb, hoja_nm, cols = cols_to_merge, rows = fila)
          openxlsx::addStyle(wb, hoja_nm, style = hdrStyle, rows = fila, cols = col_inicio_tests)

          write_clean_table(wb, hoja_nm, test_df, startRow = fila + 1, startCol = col_inicio_tests)
          apply_significance_format(
            wb,
            hoja_nm,
            test_df,
            grp_des,
            start_row = fila + 1,
            start_col = col_inicio_tests,
            numeric_style = testStyle
          )
          fila <- fila + nrow(test_df) + 3
        }
      }

      if (length(sufijo) > 1) {
        test_df_last <- lista_tests$against_last_year[[combo]]
        if (!is.null(test_df_last)) {
          titulo_test <- "Test contra último año:"
          openxlsx::writeData(wb, hoja_nm, titulo_test, startRow = fila, startCol = col_inicio_tests)
          cols_to_merge <- col_inicio_tests:(col_inicio_tests + ncol(test_df_last) - 1)
          openxlsx::mergeCells(wb, hoja_nm, cols = cols_to_merge, rows = fila)
          openxlsx::addStyle(wb, hoja_nm, style = hdrStyle, rows = fila, cols = col_inicio_tests)
          write_clean_table(wb, hoja_nm, test_df_last, startRow = fila + 1, startCol = col_inicio_tests)
          apply_significance_format(
            wb,
            hoja_nm,
            test_df_last,
            grp_des,
            start_row = fila + 1,
            start_col = col_inicio_tests,
            numeric_style = testStyle
          )
          fila <- fila + nrow(test_df_last) + 3
        }

        test_df_nac <- lista_tests$against_national[[combo]]
        if (!is.null(test_df_nac)) {
          titulo_test <- "Test contra estimación nacional:"
          openxlsx::writeData(wb, hoja_nm, titulo_test, startRow = fila, startCol = col_inicio_tests)
          cols_to_merge <- col_inicio_tests:(col_inicio_tests + ncol(test_df_nac) - 1)
          openxlsx::mergeCells(wb, hoja_nm, cols = cols_to_merge, rows = fila)
          openxlsx::addStyle(wb, hoja_nm, style = hdrStyle, rows = fila, cols = col_inicio_tests)
          write_clean_table(wb, hoja_nm, test_df_nac, startRow = fila + 1, startCol = col_inicio_tests)
          apply_significance_format(
            wb,
            hoja_nm,
            test_df_nac,
            grp_des,
            start_row = fila + 1,
            start_col = col_inicio_tests,
            numeric_style = testStyle
          )
        }
      }



    }
  }
  openxlsx::saveWorkbook(wb, filename, overwrite = TRUE)
  message("Reporte Excel de cuantiles creado en: ", filename)
}
