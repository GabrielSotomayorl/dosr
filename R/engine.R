# ---------------------------------------------------------------------------- #
# Archivo: engine.R (VERSIÓN CON CÁLCULO DE PORCENTAJES)
# ---------------------------------------------------------------------------- #

#' @title Internal calculation engine for survey estimates
#' @description This internal function performs the statistical calculations.
#' @noRd
calculate_estimates <- function(dsgn,
                               var, des, filt, rm_na_var, type = c("prop", "mean", "quantile", "total", "ratio"),
                                psu_var, strata_var, weight_var, multi_des, es_var_estudio,
                                porcentaje = FALSE,
                                quantile_prob = 0.5,
                                ratio_vars = NULL) {

  type <- match.arg(type)

  numerator_var <- NULL
  denominator_var <- NULL
  if (type == "ratio") {
    if (is.null(ratio_vars) || !all(c("num", "den") %in% names(ratio_vars))) {
      stop("Para estimar razones debe proporcionarse 'ratio_vars' con elementos 'num' y 'den'.", call. = FALSE)
    }
    numerator_var <- ratio_vars[["num"]]
    denominator_var <- ratio_vars[["den"]]
    stopifnot(
      "El numerador debe ser un string" = is.character(numerator_var) && length(numerator_var) == 1 && nzchar(numerator_var),
      "El denominador debe ser un string" = is.character(denominator_var) && length(denominator_var) == 1 && nzchar(denominator_var)
    )
  }

  analysis_vars <- if (type == "ratio") unique(c(numerator_var, denominator_var)) else var

  # --- Pre-procesamiento ---
  if (!is.null(filt) && nzchar(filt)) {
    dsgn <- dsgn %>% srvyr::filter(!!rlang::parse_expr(filt))
  }
  if (rm_na_var) {
    if (!is.null(analysis_vars) && length(analysis_vars) > 0) {
      dsgn <- dsgn %>% srvyr::filter(dplyr::if_all(dplyr::all_of(analysis_vars), ~ !is.na(.x)))
    }
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
      est <- dsgn %>%
        group_by(across(all_of(grp_vars))) %>%
        summarise(prop = survey_prop(vartype = "se"), .groups = "drop") %>%
        rename(se = prop_se)

      if (porcentaje) {
        est <- est %>% mutate(
          prop = prop * 100,
          se = se * 100
        )
      }

    } else if (type == "mean") {
      grp_vars <- grp_des
      est <- dsgn %>% group_by(across(all_of(grp_vars))) %>% summarise(media = survey_mean(.data[[var]], vartype = c("se", "cv"), na.rm = TRUE), .groups = "drop") %>% rename(se = media_se, cv = media_cv)
    } else if (type == "total") {
      grp_vars <- grp_des
      est <- dsgn %>%
        group_by(across(all_of(grp_vars))) %>%
        summarise(total = survey_total(.data[[var]], vartype = c("se", "cv"), na.rm = TRUE), .groups = "drop") %>%
        rename(se = total_se, cv = total_cv)
    } else if (type == "ratio") {
      grp_vars <- grp_des
      num_formula <- rlang::new_formula(NULL, rlang::sym(numerator_var))
      den_formula <- rlang::new_formula(NULL, rlang::sym(denominator_var))
      est <- dsgn %>%
        group_by(across(all_of(grp_vars))) %>%
        summarise(
          ratio = survey_ratio(
            numerator = !!num_formula,
            denominator = !!den_formula,
            vartype = c("se", "cv"),
            na.rm = TRUE
          ),
          .groups = "drop"
        ) %>%
        rename(se = ratio_se, cv = ratio_cv)
    } else {
      grp_vars <- grp_des
      est <- tryCatch(
        dsgn %>%
          group_by(across(all_of(grp_vars))) %>%
          summarise(
            cuantil = survey_quantile(.data[[var]], quantile_prob, vartype = "se", na.rm = TRUE),
            .groups = "drop"
          ),
        error = function(err) {
          msg <- paste0(
            "No se pudo calcular el cuantil para la combinación solicitada (",
            paste(grp_vars, collapse = ", "),
            "): ",
            conditionMessage(err)
          )

          rlang::warn(paste0(msg, " Se devolverá el cuantil sin error estándar (SE = NA)."))

          fallback <- tryCatch(
            dsgn %>%
              group_by(across(all_of(grp_vars))) %>%
              summarise(
                cuantil = survey_quantile(.data[[var]], quantile_prob, vartype = NULL, na.rm = TRUE),
                .groups = "drop"
              ),
            error = function(inner_err) {
              rlang::warn(
                paste0(
                  "Tampoco se pudo obtener el cuantil puntual para la combinación (",
                  paste(grp_vars, collapse = ", "),
                  "): ",
                  conditionMessage(inner_err)
                )
              )

              if (length(grp_vars) == 0) {
                tibble::tibble(cuantil = NA_real_)
              } else {
                base_df %>%
                  dplyr::distinct(across(all_of(grp_vars))) %>%
                  dplyr::mutate(cuantil = NA_real_)
              }
            }
          )

          fallback
        }
      )

      se_col <- grep("_se$", names(est), value = TRUE)
      if (length(se_col) == 1) {
        est <- est %>% dplyr::rename(se = dplyr::all_of(se_col))
      } else if (!"se" %in% names(est)) {
        est <- est %>% dplyr::mutate(se = NA_real_)
      }

      if (!"cuantil" %in% names(est)) {
        candidate_cols <- setdiff(names(est), c(grp_vars, "se", se_col))
        candidate_cols <- candidate_cols[vapply(candidate_cols, function(.col) {
          is.numeric(est[[.col]]) && !all(is.na(est[[.col]]))
        }, logical(1))]

        if (length(candidate_cols) >= 1) {
          est <- est %>% dplyr::rename(cuantil = dplyr::all_of(candidate_cols[1]))
        } else {
          est <- est %>% dplyr::mutate(cuantil = NA_real_)
        }
      }

      est <- est %>%
        dplyr::mutate(cv = dplyr::case_when(
          is.finite(cuantil) & cuantil != 0 ~ se / abs(cuantil),
          TRUE ~ NA_real_
        ))
    }

    tam_group_vars <- if(type == "prop") c(grp_des, var) else grp_des
    tam <- base_df %>% group_by(across(all_of(tam_group_vars))) %>% summarise(n_mues = n(), N_pob  = sum(.w), gl = n_distinct(.psu) - n_distinct(.str), .groups = "drop")
    out <- if (length(tam_group_vars) == 0) bind_cols(est, tam) else left_join(est, tam, by = tam_group_vars)

    if (type == "prop") {
      out <- out %>%
        group_by(across(all_of(grp_des))) %>%
        mutate(
          n_universo = sum(n_mues),
          n_niveles = n_distinct(.data[[var]], na.rm = TRUE)
        ) %>%
        ungroup() %>%
        mutate(
          prop_val = if(porcentaje) prop / 100 else prop,
          se_umbral_prop = if_else(prop_val < 0.5, (prop_val^(2/3))/9, ((1-prop_val)^(2/3))/9),
          se_umbral = if (porcentaje) se_umbral_prop * 100 else se_umbral_prop,
          fiabilidad = case_when(
            n_mues == 0 ~ "Sin casos",
            gl <= 9 ~ "No Fiable",
            n_niveles == 2 & n_universo < 30 & es_var_estudio == FALSE ~ "No Fiable",
            n_niveles != 2 & n_mues < 30 & es_var_estudio == FALSE ~ "No Fiable",
            se > se_umbral ~ "Poco Fiable",
            TRUE ~ "Fiable"
          )
        )
    } else if (type == "mean") {
      out <- out %>%
        mutate(
          variable = var,
          fiabilidad = case_when(
            n_mues == 0 ~ "Sin casos",
            gl <= 9 ~ "No Fiable",
            n_mues < 30 & es_var_estudio == FALSE ~ "No Fiable",
            cv > 0.30 ~ "No Fiable",
            cv > 0.20 ~ "Poco Fiable",
            TRUE ~ "Fiable"
          )
        )
    } else if (type == "total") {
      out <- out %>%
        mutate(
          variable = var,
          cv = dplyr::case_when(
            is.finite(total) & total != 0 ~ se / abs(total),
            TRUE ~ cv
          ),
          fiabilidad = case_when(
            n_mues == 0 ~ "Sin casos",
            gl <= 9 ~ "No Fiable",
            n_mues < 30 & es_var_estudio == FALSE ~ "No Fiable",
            is.na(cv) ~ NA_character_,
            cv > 0.30 ~ "No Fiable",
            cv > 0.20 ~ "Poco Fiable",
            TRUE ~ "Fiable"
          )
        )
    } else if (type == "ratio") {
      out <- out %>%
        mutate(
          variable = var,
          fiabilidad = case_when(
            n_mues == 0 ~ "Sin casos",
            gl <= 9 ~ "No Fiable",
            n_mues < 30 & es_var_estudio == FALSE ~ "No Fiable",
            is.na(cv) ~ NA_character_,
            cv > 0.30 ~ "No Fiable",
            cv > 0.20 ~ "Poco Fiable",
            TRUE ~ "Fiable"
          )
        )
    } else {
      out <- out %>%
        mutate(
          variable = var,
          fiabilidad = case_when(
            n_mues == 0 ~ "Sin casos",
            gl <= 9 ~ "No Fiable",
            n_mues < 30 & es_var_estudio == FALSE ~ "No Fiable",
            is.na(cv) ~ NA_character_,
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
