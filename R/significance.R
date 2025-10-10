# ---------------------------------------------------------------------------- #
# Archivo: significance.R (VERSIÓN FINAL CON MÁXIMA ROBUSTEZ)
# ---------------------------------------------------------------------------- #

# --- Helpers Seguros ---

.perform_t_test <- function(est1, se1, gl1, est2, se2, gl2) {
  if (anyNA(c(est1, se1, gl1, est2, se2, gl2)) || !is.finite(se1) || !is.finite(se2)) return(NA_real_)
  se_diff <- sqrt(se1^2 + se2^2)
  if (!is.finite(se_diff) || se_diff < 1e-9) return(1)
  t_stat <- (est1 - est2) / se_diff
  df <- suppressWarnings(min(gl1, gl2, na.rm = TRUE))
  if (is.na(df) || !is.finite(df) || df <= 0) return(NA_real_)
  2 * stats::pt(abs(t_stat), df = df, lower.tail = FALSE)
}

.calculate_intra_year_tests <- function(df, main_var, group_vars = character(0), est_col, se_col, gl_col) {
  df_clean <- df %>% dplyr::filter(!is.na(.data[[main_var]]))
  if (length(group_vars) > 0) {
    df_clean <- df_clean %>%
      dplyr::filter(!dplyr::if_any(dplyr::all_of(group_vars), ~ is.na(.x) | .x == ""))
  }
  if (nrow(df_clean) < 2) return(NULL)

  id_cols <- unique(c(group_vars, main_var))
  display_keys <- df_clean %>%
    dplyr::select(dplyr::any_of(id_cols)) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ as.character(.x)))

  if (ncol(display_keys) == 0) return(NULL)

  column_labels <- purrr::pmap_chr(display_keys, function(...) {
    parts <- as.character(c(...))
    parts <- parts[!is.na(parts) & parts != ""]
    if (length(parts) == 0) return(main_var)
    paste(parts, collapse = "\n")
  }) %>%
    make.unique()

  pairs <- utils::combn(seq_len(nrow(df_clean)), 2, simplify = FALSE)

  p_values <- purrr::map_dbl(pairs, ~ .perform_t_test(
    est1 = df_clean[[est_col]][.x[1]], se1 = df_clean[[se_col]][.x[1]], gl1 = df_clean[[gl_col]][.x[1]],
    est2 = df_clean[[est_col]][.x[2]], se2 = df_clean[[se_col]][.x[2]], gl2 = df_clean[[gl_col]][.x[2]]
  ))

  mat <- matrix(
    NA_real_,
    nrow = nrow(df_clean),
    ncol = nrow(df_clean),
    dimnames = list(column_labels, column_labels)
  )
  for (i in seq_along(pairs)) {
    p <- pairs[[i]]
    mat[p[2], p[1]] <- p_values[i]
  }
  diag(mat) <- 0

  mat_df <- as.data.frame(mat, check.names = FALSE)
  mat_df <- dplyr::bind_cols(display_keys, mat_df)
  names(mat_df)[(ncol(display_keys) + 1):ncol(mat_df)] <- column_labels

  if (length(group_vars) > 0) {
    for (grp in group_vars) {
      grp_values <- mat_df[[grp]]
      if (length(grp_values) > 1) {
        dup_idx <- c(FALSE, grp_values[-1] == grp_values[-length(grp_values)])
        mat_df[[grp]][dup_idx] <- ""
      }
    }
  }

  mat_df
}

# ## CORRECCIÓN ##: Helpers robustecidos para manejar cualquier tipo de columna
.drop_all_na_or_empty_key_rows <- function(df, key_cols) {
  if (length(key_cols) == 0 || is.null(df) || nrow(df) == 0) return(df)
  df %>%
    dplyr::filter(
      !dplyr::if_all(
        dplyr::all_of(key_cols),
        ~ is.na(as.character(.x)) | as.character(.x) == ""
      )
    )
}

.filter_out_totals <- function(df, cols) {
  if (length(cols) == 0 || is.null(df) || nrow(df) == 0) return(df)
  df %>%
    dplyr::filter(
      !dplyr::if_any(dplyr::all_of(cols), ~ grepl("^Total", as.character(.x), ignore.case = TRUE))
    )
}


# --- Orquestador Principal ---

calculate_significance <- function(hojas_list, sufijo, type, main_var_prop, des_vars) {
  if (is.null(des_vars)) return(NULL)
  est_prefix <- switch(type,
    prop = "prop",
    mean = "media",
    total = "total",
    quantile = "cuantil",
    ratio = "ratio"
  )
  se_prefix  <- "se"
  gl_prefix  <- "gl"

  all_tests <- list()

  # --- Test 1: Comparaciones Intra-Anuales ---
  intra_year_list <- purrr::map(names(hojas_list), function(combo) {
    if (combo == "nac") return(NULL)
    grp_des <- strsplit(combo, "__", fixed = TRUE)[[1]]
    if (length(grp_des) > 1) return(NULL)

    main_cmp_var <- if (type == "prop") main_var_prop else grp_des[1]

    purrr::map(sufijo, function(sfx) {
      df <- hojas_list[[combo]] %>% .filter_out_totals(c(main_cmp_var, main_var_prop))
      if (is.null(df) || nrow(df) < 2) return(NULL)

      est_col <- paste0(est_prefix, "_", sfx); se_col  <- paste0(se_prefix,  "_", sfx); gl_col  <- paste0(gl_prefix,  "_", sfx)
      if (!all(c(est_col, se_col, gl_col) %in% names(df))) return(NULL)

      .calculate_intra_year_tests(df, main_cmp_var, grp_des, est_col, se_col, gl_col)
    }) %>% rlang::set_names(sufijo)
  }) %>% rlang::set_names(names(hojas_list))
  all_tests$intra_year <- purrr::compact(intra_year_list)

  if (length(sufijo) < 2) return(all_tests)

  key_cols_for_test <- if (type == "prop") unique(c(main_var_prop, des_vars)) else unique(des_vars)

  # --- Test 2: Contra Último Año ---
  against_last_year_list <- purrr::map(names(hojas_list), function(combo) {
    if (combo == "nac") return(NULL)
    df <- hojas_list[[combo]] %>% .drop_all_na_or_empty_key_rows(key_cols_for_test) %>% .filter_out_totals(key_cols_for_test)
    if (is.null(df) || nrow(df) == 0) return(NULL)

    last_sfx <- sufijo[length(sufijo)]

    comparisons <- purrr::map(sufijo[-length(sufijo)], function(sfx) {
      p_values <- purrr::map_dbl(seq_len(nrow(df)), ~ .perform_t_test(
        est1 = df[[paste0(est_prefix, "_", last_sfx)]][.x], se1  = df[[paste0(se_prefix,  "_", last_sfx)]][.x], gl1  = df[[paste0(gl_prefix,  "_", last_sfx)]][.x],
        est2 = df[[paste0(est_prefix, "_", sfx)]][.x],      se2  = df[[paste0(se_prefix,  "_", sfx)]][.x],      gl2  = df[[paste0(gl_prefix,  "_", sfx)]][.x]
      ))
      tibble::tibble(!!paste0("p_value_", sfx) := p_values)
    })

    df_keys <- dplyr::select(df, dplyr::any_of(key_cols_for_test))
    dplyr::bind_cols(df_keys, !!!purrr::compact(comparisons))
  }) %>% rlang::set_names(names(hojas_list))
  all_tests$against_last_year <- purrr::compact(against_last_year_list)

  # --- Test 3: Contra Nacional ---
  against_national_list <- purrr::map(names(hojas_list), function(combo) {
    if (combo == "nac") return(NULL)
    df <- hojas_list[[combo]] %>% .drop_all_na_or_empty_key_rows(key_cols_for_test) %>% .filter_out_totals(key_cols_for_test)
    df_nac_base <- hojas_list[["nac"]]
    if (is.null(df) || nrow(df) == 0 || is.null(df_nac_base) || nrow(df_nac_base) == 0) return(NULL)

    comparisons <- purrr::map(sufijo, function(sfx) {
      est_col <- paste0(est_prefix, "_", sfx); se_col  <- paste0(se_prefix,  "_", sfx); gl_col  <- paste0(gl_prefix,  "_", sfx)

      p_values <- purrr::map_dbl(seq_len(nrow(df)), ~ {
        df_nac <- if (type == "prop") df_nac_base %>% dplyr::filter(.data[[main_var_prop]] == as.character(df[[main_var_prop]][.x])) else df_nac_base
        if (nrow(df_nac) != 1) return(NA_real_)
        .perform_t_test(
          est1 = df[[est_col]][.x], se1 = df[[se_col]][.x], gl1 = df[[gl_col]][.x],
          est2 = df_nac[[est_col]][1], se2 = df_nac[[se_col]][1], gl2 = df_nac[[gl_col]][1]
        )
      })
      tibble::tibble(!!paste0("p_value_", sfx) := p_values)
    })

    df_keys <- dplyr::select(df, dplyr::any_of(key_cols_for_test))
    dplyr::bind_cols(df_keys, !!!purrr::compact(comparisons))
  }) %>% rlang::set_names(names(hojas_list))
  all_tests$against_national <- purrr::compact(against_national_list)

  return(all_tests)
}
