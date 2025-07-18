# ---------------------------------------------------------------------------- #
# Archivo: reporting.R (VERSIÓN CON CORRECCIONES PARA CHECK)
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
      fill_list_N <- purrr::map(sufijo, ~ 0) %>% set_names(paste0("N_pob_", sufijo))
      fill_list_media <- purrr::map(sufijo, ~ NA_real_) %>% set_names(paste0("media_", sufijo))
      final_fill_list <- if(type == "prop") c(fill_list_prop, fill_list_n, fill_list_N) else c(fill_list_media, fill_list_n, fill_list_N)
      grouping_vars <- if (type == "mean") "variable" else NULL
      joined_df <- joined_df %>%
        group_by(across(any_of(grouping_vars))) %>%
        tidyr::complete(!!!all_levels_to_use, fill = final_fill_list) %>%
        ungroup() %>%
        mutate(nivel = ifelse(is.na(nivel) & length(grp_des) > 0, paste(!!!rlang::syms(grp_des), sep = "-"), nivel))
    }
    return(joined_df)
  }
  hojas_list <- purrr::map(combos_nombres, combinar_por_combo)
  names(hojas_list) <- combos_nombres
  return(hojas_list)
}

# --- Reporte específico para Proporciones ---
generate_prop_report <- function(hojas_list, filename, var, des, sufijo, porcentaje, decimales, designs, consolidated_df) {
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "1_Consolidado")
  write_clean_table(wb, "1_Consolidado", consolidated_df, startRow = 1, startCol = 1)
  countStyle <- openxlsx::createStyle(numFmt = "#,##0")
  percent_fmt_string <- if (decimales > 0) paste0("0.", paste(rep("0", decimales), collapse = "")) else "0"
  percentStyle <- openxlsx::createStyle(numFmt = if (porcentaje) paste0(percent_fmt_string, "%") else percent_fmt_string)
  estilos <- list(percentStyle, countStyle, percentStyle, countStyle)

  # ## CORRECCIÓN ASCII ##
  titulos  <- c("Estimaci\u00f3n", "Poblaci\u00f3n expandida", "Error est\u00e1ndar", "Casos muestrales")
  metricas <- c("prop", "N_pob", "se", "n_mues")
  met_suffix_list <- purrr::map(metricas, ~ paste0(.x, "_", sufijo))
  names(met_suffix_list) <- metricas
  all_factor_levels <- get_all_levels(designs, c(var, des))
  bold_style <- createStyle(textDecoration = "bold")

  make_prop_block <- function(df_wide, grp_des, metric_cols, factor_levels_list) {
    base_tbl <- df_wide %>% select(all_of(c(grp_des, var, metric_cols)))
    subtot <- base_tbl %>%
      group_by(across(all_of(grp_des))) %>%
      summarise(
        !!sym(var) := "Total",
        across(all_of(metric_cols), ~ case_when(
          startsWith(cur_column(), "prop_") ~ 1,
          startsWith(cur_column(), "se_")   ~ 0,
          TRUE ~ sum(.x, na.rm = TRUE)
        )),
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
    openxlsx::writeData(wb, hoja_nm, "Nombre indicador", startRow = 2, startCol = 1)
    openxlsx::addStyle(wb, hoja_nm, style = bold_style, rows = 2, cols = 1)
    openxlsx::writeData(wb, hoja_nm, "Tipo de c\u00e1lculo", startRow = 3, startCol = 1)
    df_combo_wide <- hojas_list[[combo]]
    fila <- 5
    for (i in seq_along(titulos)) {
      block <- make_prop_block(df_combo_wide, grp_des, met_suffix_list[[metricas[i]]], all_factor_levels)
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
  }
  openxlsx::saveWorkbook(wb, filename, overwrite = TRUE)
  message("Reporte Excel de proporciones creado en: ", filename)
}

# --- Reporte específico para Medias ---
generate_mean_report <- function(hojas_list, filename, var, des, sufijo, decimales, designs, consolidated_df) {
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "1_Consolidado")
  write_clean_table(wb, "1_Consolidado", consolidated_df, startRow = 1, startCol = 1)
  num_fmt_string <- if (decimales > 0) paste0("#,##0.", paste(rep("0", decimales), collapse = "")) else "#,##0"
  numStyle <- openxlsx::createStyle(numFmt = num_fmt_string)
  estilos <- list(numStyle, openxlsx::createStyle(numFmt = "#,##0"), numStyle, openxlsx::createStyle(numFmt = "#,##0"))

  # ## CORRECCIÓN ASCII ##
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
    openxlsx::writeData(wb, hoja_nm, "Nombre indicador", startRow = 2, startCol = 1)
    openxlsx::addStyle(wb, hoja_nm, style = bold_style, rows = 2, cols = 1)
    openxlsx::writeData(wb, hoja_nm, "Tipo de c\u00e1lculo", startRow = 3, startCol = 1)
    df_combo_wide <- hojas_list[[combo]]
    fila <- 5
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
  }
  openxlsx::saveWorkbook(wb, filename, overwrite = TRUE)
  message("Reporte Excel de medias creado en: ", filename)
}
