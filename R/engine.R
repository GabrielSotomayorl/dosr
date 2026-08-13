# ---------------------------------------------------------------------------- #
# Archivo: engine.R (VERSIÓN CON CÁLCULO DE PORCENTAJES)
# ---------------------------------------------------------------------------- #

#' @title Internal calculation engine for survey estimates
#' @description This internal function performs the statistical calculations.
#' @noRd
calculate_estimates <- function(dsgn,
                               var, des, filt, rm_na_var, rm_na_des = FALSE, type = c("prop", "mean", "quantile", "total", "ratio"),
                                psu_var, strata_var, weight_var, multi_des, es_var_estudio,
                                porcentaje = FALSE,
                                quantile_prob = 0.5,
                                ratio_vars = NULL,
                                cv_umbral_alto = 0.30,
                                cv_umbral_medio = 0.20,
                                n_minimo = 30,
                                nivel_confianza = 0.95,
                                universo_crit = FALSE,
                                par_combos = FALSE) {

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
  # Use the survey design internals instead of looking up the original design
  # columns in `variables`.  With `nest = TRUE`, survey recodes first-stage PSU
  # identifiers so they are unique across strata.  The internal vectors also
  # exist for designs created without explicit `ids` or `strata` columns.
  if (is.null(dsgn$cluster) || is.null(dsgn$strata)) {
    stop(
      "El dise\u00f1o no contiene la estructura de conglomerados y estratos requerida. ",
      "Actualmente se admiten dise\u00f1os creados con srvyr::as_survey_design().",
      call. = FALSE
    )
  }
  base_df <- dsgn$variables %>%
    mutate(
      .w   = as.numeric(stats::weights(dsgn, type = "sampling")),
      .psu = dsgn$cluster[[1L]],
      .str = dsgn$strata[[1L]]
    )

  # Capture the direct back-end helpers in this closure so future/furrr exports
  # them together with calc_tabla when combinations run in separate workers.
  # Rebind the helpers in this call environment and clone the two entry points
  # into it. This makes the complete backend self-contained when furrr sends
  # calc_tabla to separate R sessions.
  .survey_domain_groups <- .survey_domain_groups
  .survey_restore_groups <- .survey_restore_groups
  survey_direct_prop <- rlang::new_function(
    formals(.survey_direct_prop), body(.survey_direct_prop), env = environment()
  )
  survey_direct_scalar <- rlang::new_function(
    formals(.survey_direct_scalar), body(.survey_direct_scalar), env = environment()
  )

  # --- Función de cálculo interna ---
  calc_tabla <- function(grp_des) {
    dsgn_loc <- dsgn
    base_df_loc <- base_df
    if (rm_na_des && length(grp_des) > 0) {
      dsgn_loc <- dsgn_loc %>% srvyr::filter(dplyr::if_all(dplyr::all_of(grp_des), ~ !is.na(.x)))
      base_df_loc <- base_df_loc %>% dplyr::filter(dplyr::if_all(dplyr::all_of(grp_des), ~ !is.na(.x)))
    }
    totals_info <- NULL
    if (type == "prop") {
      grp_vars <- c(grp_des, var)
      est <- survey_direct_prop(dsgn_loc, grp_des, var)

      if (porcentaje) {
        est <- est %>% mutate(
          prop = prop * 100,
          se = se * 100
        )
      }

      tam_num <- base_df_loc %>%
        group_by(across(all_of(grp_vars))) %>%
        summarise(
          n_mues = dplyr::n(),
          N_pob = sum(.w),
          .groups = "drop"
        )

      gl_base <- if (length(grp_des) == 0) {
        base_df_loc %>%
          summarise(
            gl = n_distinct(.psu) - n_distinct(.str)
          )
      } else {
        base_df_loc %>%
          group_by(across(all_of(grp_des))) %>%
          summarise(
            gl = n_distinct(.psu) - n_distinct(.str),
            .groups = "drop"
          )
      }

      out <- est %>%
        dplyr::left_join(tam_num, by = grp_vars) %>%
        mutate(
          n_mues = as.integer(dplyr::coalesce(n_mues, 0L)),
          N_pob = dplyr::coalesce(N_pob, 0)
        )

      out <- if (length(grp_des) == 0) {
        out %>%
          mutate(gl = gl_base$gl[1])
      } else {
        out %>% dplyr::left_join(gl_base, by = grp_des)
      }

      out <- out %>%
        mutate(gl = as.numeric(gl))

    } else if (type == "mean") {
      grp_vars <- grp_des
      est <- survey_direct_scalar(dsgn_loc, grp_vars, "mean", variable = var) %>%
        dplyr::rename(media = dplyr::all_of("estimate"))
    } else if (type == "total") {
      grp_vars <- grp_des
      est <- survey_direct_scalar(dsgn_loc, grp_vars, "total", variable = var) %>%
        dplyr::rename(total = dplyr::all_of("estimate"))
    } else if (type == "ratio") {
      grp_vars <- grp_des
      est <- survey_direct_scalar(
        dsgn_loc, grp_vars, "ratio",
        numerator = numerator_var, denominator = denominator_var
      ) %>% dplyr::rename(ratio = dplyr::all_of("estimate"))
    } else {
      grp_vars <- grp_des
      est <- tryCatch(
        dsgn_loc %>%
          group_by(across(all_of(grp_vars))) %>%
          summarise(
            cuantil = survey_quantile(.data[[var]], quantile_prob, vartype = "se", level = nivel_confianza, na.rm = TRUE),
            .groups = "drop"
          ),
        error = function(err) {
          msg <- paste0(
            "No se pudo calcular el cuantil para la combinaci\u00f3n solicitada (",
            paste(grp_vars, collapse = ", "),
            "): ",
            conditionMessage(err)
          )

          rlang::warn(paste0(msg, " Se devolver\u00e1 el cuantil sin error est\u00e1ndar (SE = NA)."))

          fallback <- tryCatch(
            dsgn_loc %>%
              group_by(across(all_of(grp_vars))) %>%
              summarise(
                cuantil = survey_quantile(.data[[var]], quantile_prob, vartype = NULL, na.rm = TRUE),
                .groups = "drop"
              ),
            error = function(inner_err) {
              rlang::warn(
                paste0(
                  "Tampoco se pudo obtener el cuantil puntual para la combinaci\u00f3n (",
                  paste(grp_vars, collapse = ", "),
                  "): ",
                  conditionMessage(inner_err)
                )
              )

              if (length(grp_vars) == 0) {
                tibble::tibble(cuantil = NA_real_)
              } else {
                base_df_loc %>%
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

    if (type != "prop") {
      tam_group_vars <- grp_des
      tam <- base_df_loc %>%
        group_by(across(all_of(tam_group_vars))) %>%
        summarise(
          n_mues = n(),
          N_pob  = sum(.w),
          gl     = n_distinct(.psu) - n_distinct(.str),
          .groups = "drop"
        )
      out <- if (length(tam_group_vars) == 0) {
        bind_cols(est, tam)
      } else {
        dplyr::left_join(est, tam, by = tam_group_vars)
      }
    }

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
            is.na(gl) | gl <= 9 ~ "No Fiable (gl)",
            universo_crit & n_universo < n_minimo & !es_var_estudio ~ "No Fiable (muestra)",
            !universo_crit & n_niveles == 2 & n_universo < n_minimo & !es_var_estudio ~ "No Fiable (muestra)",
            !universo_crit & n_niveles != 2 & n_mues < n_minimo & !es_var_estudio ~ "No Fiable (muestra)",
            is.na(se) | is.na(se_umbral) ~ NA_character_,
            se > se_umbral ~ "Poco Fiable (EE)",
            TRUE ~ "Fiable"
          )
        )
    } else if (type == "mean") {
      out <- out %>%
        mutate(
          variable = var,
          fiabilidad = case_when(
            n_mues == 0 ~ "Sin casos",
            is.na(gl) | gl <= 9 ~ "No Fiable (gl)",
            n_mues < n_minimo & es_var_estudio == FALSE ~ "No Fiable (muestra)",
            !is.finite(cv) ~ NA_character_,
            cv > cv_umbral_alto ~ "No Fiable (CV)",
            cv > cv_umbral_medio ~ "Poco Fiable (CV)",
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
            is.na(gl) | gl <= 9 ~ "No Fiable (gl)",
            n_mues < n_minimo & es_var_estudio == FALSE ~ "No Fiable (muestra)",
            is.na(cv) ~ NA_character_,
            cv > cv_umbral_alto ~ "No Fiable (CV)",
            cv > cv_umbral_medio ~ "Poco Fiable (CV)",
            TRUE ~ "Fiable"
          )
        )
    } else if (type == "ratio") {
      out <- out %>%
        mutate(
          variable = var,
          fiabilidad = case_when(
            n_mues == 0 ~ "Sin casos",
            is.na(gl) | gl <= 9 ~ "No Fiable (gl)",
            n_mues < n_minimo & es_var_estudio == FALSE ~ "No Fiable (muestra)",
            !is.finite(cv) ~ NA_character_,
            cv > cv_umbral_alto ~ "No Fiable (CV)",
            cv > cv_umbral_medio ~ "Poco Fiable (CV)",
            TRUE ~ "Fiable"
          )
        )
    } else {
      out <- out %>%
        mutate(
          variable = var,
          fiabilidad = case_when(
            n_mues == 0 ~ "Sin casos",
            is.na(gl) | gl <= 9 ~ "No Fiable (gl)",
            n_mues < n_minimo & es_var_estudio == FALSE ~ "No Fiable (muestra)",
            !is.finite(cv) ~ NA_character_,
            cv > cv_umbral_alto ~ "No Fiable (CV)",
            cv > cv_umbral_medio ~ "Poco Fiable (CV)",
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

    if (rm_na_des && length(grp_des) > 0) {
      totals_info <- tryCatch({
        if (type == "mean") {
          total_est <- survey_direct_scalar(
            dsgn_loc, character(), "mean", variable = var
          ) %>%
            dplyr::rename(media = dplyr::all_of("estimate"))
          total_tam <- base_df_loc %>%
            summarise(
              n_mues = n(),
              N_pob  = sum(.w),
              gl     = n_distinct(.psu) - n_distinct(.str)
            )
          bind_cols(total_est, total_tam) %>%
            mutate(
              variable = var,
              fiabilidad = case_when(
                n_mues == 0 ~ "Sin casos",
                is.na(gl) | gl <= 9 ~ "No Fiable (gl)",
                n_mues < n_minimo & es_var_estudio == FALSE ~ "No Fiable (muestra)",
                !is.finite(cv) ~ NA_character_,
                cv > cv_umbral_alto ~ "No Fiable (CV)",
                cv > cv_umbral_medio ~ "Poco Fiable (CV)",
                TRUE ~ "Fiable"
              )
            ) %>%
            relocate(variable)
        } else if (type == "total") {
          total_est <- survey_direct_scalar(
            dsgn_loc, character(), "total", variable = var
          ) %>%
            dplyr::rename(total = dplyr::all_of("estimate"))
          total_tam <- base_df_loc %>%
            summarise(
              n_mues = n(),
              N_pob  = sum(.w),
              gl     = n_distinct(.psu) - n_distinct(.str)
            )
          bind_cols(total_est, total_tam) %>%
            mutate(
              variable = var,
              cv = dplyr::case_when(
                is.finite(total) & total != 0 ~ se / abs(total),
                TRUE ~ cv
              ),
              fiabilidad = case_when(
                n_mues == 0 ~ "Sin casos",
                is.na(gl) | gl <= 9 ~ "No Fiable (gl)",
                n_mues < n_minimo & es_var_estudio == FALSE ~ "No Fiable (muestra)",
                is.na(cv) ~ NA_character_,
                cv > cv_umbral_alto ~ "No Fiable (CV)",
                cv > cv_umbral_medio ~ "Poco Fiable (CV)",
                TRUE ~ "Fiable"
              )
            ) %>%
            relocate(variable)
        } else if (type == "ratio") {
          total_est <- survey_direct_scalar(
            dsgn_loc, character(), "ratio",
            numerator = numerator_var, denominator = denominator_var
          ) %>%
            dplyr::rename(ratio = dplyr::all_of("estimate"))
          total_tam <- base_df_loc %>%
            summarise(
              n_mues = n(),
              N_pob  = sum(.w),
              gl     = n_distinct(.psu) - n_distinct(.str)
            )
          bind_cols(total_est, total_tam) %>%
            mutate(
              variable = var,
              fiabilidad = case_when(
                n_mues == 0 ~ "Sin casos",
                is.na(gl) | gl <= 9 ~ "No Fiable (gl)",
                n_mues < n_minimo & es_var_estudio == FALSE ~ "No Fiable (muestra)",
                !is.finite(cv) ~ NA_character_,
                cv > cv_umbral_alto ~ "No Fiable (CV)",
                cv > cv_umbral_medio ~ "Poco Fiable (CV)",
                TRUE ~ "Fiable"
              )
            ) %>%
            relocate(variable)
        } else if (type == "quantile") {
          total_est <- tryCatch(
            dsgn_loc %>%
              summarise(
                cuantil = survey_quantile(.data[[var]], quantile_prob, vartype = "se", level = nivel_confianza, na.rm = TRUE),
                .groups = "drop"
              ),
            error = function(err) {
              msg <- paste0(
                "No se pudo calcular el cuantil nacional filtrado para la combinaci\u00f3n (",
                paste(grp_des, collapse = ", "),
                "): ",
                conditionMessage(err)
              )
              rlang::warn(paste0(msg, " Se devolver\u00e1 el cuantil sin error est\u00e1ndar (SE = NA)."))
              fallback <- tryCatch(
                dsgn_loc %>%
                  summarise(
                    cuantil = survey_quantile(.data[[var]], quantile_prob, vartype = NULL, na.rm = TRUE),
                    .groups = "drop"
                  ),
                error = function(inner_err) {
                  rlang::warn(
                    paste0(
                      "Tampoco se pudo obtener el cuantil puntual nacional filtrado (",
                      paste(grp_des, collapse = ", "),
                      "): ",
                      conditionMessage(inner_err)
                    )
                  )
                  tibble::tibble(cuantil = NA_real_)
                }
              )
              fallback$se <- NA_real_
              fallback
            }
          )
          se_col <- grep("_se$", names(total_est), value = TRUE)
          if (length(se_col) == 1) {
            total_est <- total_est %>% dplyr::rename(se = dplyr::all_of(se_col))
          } else if (!"se" %in% names(total_est)) {
            total_est <- total_est %>% dplyr::mutate(se = NA_real_)
          }
          if (!"cuantil" %in% names(total_est)) {
            candidate_cols <- setdiff(names(total_est), c("se", se_col))
            candidate_cols <- candidate_cols[vapply(candidate_cols, function(.col) {
              is.numeric(total_est[[.col]]) && !all(is.na(total_est[[.col]]))
            }, logical(1))]
            if (length(candidate_cols) >= 1) {
              total_est <- total_est %>% dplyr::rename(cuantil = dplyr::all_of(candidate_cols[1]))
            } else {
              total_est <- total_est %>% dplyr::mutate(cuantil = NA_real_)
            }
          }
          total_est <- total_est %>%
            mutate(cv = dplyr::case_when(
              is.finite(cuantil) & cuantil != 0 ~ se / abs(cuantil),
              TRUE ~ NA_real_
            ))
          total_tam <- base_df_loc %>%
            summarise(
              n_mues = n(),
              N_pob  = sum(.w),
              gl     = n_distinct(.psu) - n_distinct(.str)
            )
          bind_cols(total_est, total_tam) %>%
            mutate(
              variable = var,
              fiabilidad = case_when(
                n_mues == 0 ~ "Sin casos",
                is.na(gl) | gl <= 9 ~ "No Fiable (gl)",
                n_mues < n_minimo & es_var_estudio == FALSE ~ "No Fiable (muestra)",
                !is.finite(cv) ~ NA_character_,
                cv > cv_umbral_alto ~ "No Fiable (CV)",
                cv > cv_umbral_medio ~ "Poco Fiable (CV)",
                TRUE ~ "Fiable"
              )
            ) %>%
            relocate(variable)
        } else {
          NULL
        }
      }, error = function(e) {
        rlang::warn(conditionMessage(e))
        NULL
      })
    }

    key_cols <- if(type == "prop") c(var, des) else c("variable", des)
    out %>%
      mutate(nivel = if (length(grp_des) == 0) "Nacional" else paste(grp_des, collapse = "-")) %>%
      relocate(any_of(key_cols), nivel) %>%
      arrange(across(all_of(grp_des))) %>%
      { if (!is.null(totals_info)) `attr<-`(., "totals_filtered", totals_info) else . }
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

  tablas_loc <- if (par_combos) {
    furrr::future_map(combos, calc_tabla,
                      .options = furrr::furrr_options(seed = FALSE))
  } else {
    purrr::map(combos, calc_tabla)
  }

  names(tablas_loc) <- purrr::map_chr(combos, ~ if (length(.x) == 0) "nac" else paste(.x, collapse = "__"))

  return(tablas_loc)
}
