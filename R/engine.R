# ---------------------------------------------------------------------------- #
# Archivo: engine.R (VERSIÓN CON NUEVOS CRITERIOS DE FIABILIDAD)
# ---------------------------------------------------------------------------- #

#' @title Internal calculation engine for survey estimates
#' @description This internal function performs the statistical calculations.
#' @noRd
calculate_estimates <- function(dsgn,
                                var, des, filt, rm_na_var, type = c("prop", "mean"),
                                psu_var, strata_var, weight_var, multi_des, es_var_estudio) {

  type <- match.arg(type)

  # --- Pre-procesamiento ---
  if (!is.null(filt) && nzchar(filt)) {
    dsgn <- dsgn %>% srvyr::filter(!!rlang::parse_expr(filt))
  }
  if (rm_na_var) {
    dsgn <- dsgn %>% srvyr::filter(!is.na(.data[[var]]))
  }

  processed_vars <- dsgn$variables
  if (type == "prop") {
    vars_to_factor <- c(var, des)
    processed_vars <- processed_vars %>%
      dplyr::mutate(dplyr::across(dplyr::all_of(vars_to_factor), ~haven::as_factor(.)))
  } else {
    if (!is.null(des)) {
      processed_vars <- processed_vars %>%
        dplyr::mutate(dplyr::across(dplyr::all_of(des), ~haven::as_factor(.)))
    }
  }
  dsgn$variables <- processed_vars

  base_df <- dsgn$variables %>%
    mutate(.w = .data[[weight_var]], .psu = .data[[psu_var]], .str = .data[[strata_var]])

  # --- Función de cálculo interna ---
  calc_tabla <- function(grp_des) {
    if (type == "prop") {
      grp_vars <- c(grp_des, var)
      est <- dsgn %>% group_by(across(all_of(grp_vars))) %>% summarise(prop = survey_prop(vartype = "se"), .groups = "drop") %>% rename(se = prop_se)
    } else {
      grp_vars <- grp_des
      est <- dsgn %>% group_by(across(all_of(grp_vars))) %>% summarise(media = survey_mean(.data[[var]], vartype = c("se", "cv"), na.rm = TRUE), .groups = "drop") %>% rename(se = media_se, cv = media_cv)
    }
    tam_group_vars <- if(type == "prop") c(grp_des, var) else grp_des
    tam <- base_df %>% group_by(across(all_of(tam_group_vars))) %>% summarise(n_mues = n(), N_pob  = sum(.w), gl = n_distinct(.psu) - n_distinct(.str), .groups = "drop")
    out <- if (length(tam_group_vars) == 0) bind_cols(est, tam) else left_join(est, tam, by = tam_group_vars)

    # -------------------------------------------------------------------------- #
    # ## NUEVA LÓGICA DE FIABILIDAD ##
    # -------------------------------------------------------------------------- #
    if (type == "prop") {
      # Inferencia de si la variable es dicotómica
      out <- out %>%
        group_by(across(all_of(grp_des))) %>%
        mutate(
          n_universo = sum(n_mues),
          n_niveles = n_distinct(.data[[var]], na.rm = TRUE)
        ) %>%
        ungroup() %>%
        mutate(
          se_umbral = if_else(prop < 0.5, (prop^(2/3))/9, ((1-prop)^(2/3))/9),
          fiabilidad = case_when(
            gl <= 9 ~ "No Fiable",
            n_niveles == 2 & n_universo < 30 & es_var_estudio == FALSE ~ "No Fiable",
            n_niveles != 2 & n_mues < 30 & es_var_estudio == FALSE ~ "No Fiable",
            se > se_umbral ~ "Poco Fiable",
            TRUE ~ "Fiable"
          )
        )
    } else { # type == "mean"
      out <- out %>%
        mutate(
          variable = var, # Aseguramos que la columna 'variable' exista
          fiabilidad = case_when(
            gl <= 9 ~ "No Fiable",
            n_mues < 30 & es_var_estudio == FALSE ~ "No Fiable",
            cv > 0.30 ~ "No Fiable",
            cv > 0.20 ~ "Poco Fiable",
            TRUE ~ "Fiable"
          )
        )
    }

    if (!is.null(des)) {
      missing_des_cols <- setdiff(des, names(out))
      if (length(missing_des_cols) > 0) {
        na_cols <- purrr::map(missing_des_cols, ~ NA_character_) %>% rlang::set_names(missing_des_cols) %>% as_tibble()
        out <- bind_cols(out, na_cols)
      }
    }
    key_cols <- if(type == "prop") c(var, des) else c("variable", des)
    out %>%
      mutate(nivel = if (length(grp_des) == 0) "Nacional" else paste(grp_des, collapse = "-")) %>%
      relocate(any_of(key_cols), nivel) %>%
      arrange(across(all_of(grp_des)))
  }

  # --- Lógica de generación de combinaciones ---
  combos <- list(character(0))
  if (!is.null(des)) {
    if (multi_des) {
      for (i in 1:length(des)) {
        combos <- c(combos, utils::combn(des, i, simplify = FALSE))
      }
    } else {
      combos_simples <- purrr::map(des, ~ as.character(.x))
      combos <- c(combos, combos_simples)
    }
  }

  tablas_loc <- purrr::map(combos, calc_tabla)

  names(tablas_loc) <- purrr::map_chr(combos, ~ if (length(.x) == 0) "nac" else paste(.x, collapse = "__"))

  return(tablas_loc)
}
