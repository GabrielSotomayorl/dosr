# ---------------------------------------------------------------------------- #
# Archivo: engine.R (VERSIÓN OPTIMIZADA)
# ---------------------------------------------------------------------------- #

#' @title Internal calculation engine for survey estimates
#' @description This internal function performs the statistical calculations.
#' @noRd
calculate_estimates <- function(dsgn,
                                var, des, filt, rm_na_var, type = c("prop", "mean"),
                                psu_var, strata_var, weight_var) {

  type <- match.arg(type)

  # -------------------------------------------------------------------------- #
  # ## OPTIMIZACIÓN: PASO 1 - PRE-PROCESAMIENTO CENTRALIZADO ##
  # Todo el trabajo de preparación del diseño se realiza UNA SOLA VEZ aquí.
  # -------------------------------------------------------------------------- #

  # 1.1 Aplicar el filtro del usuario, si existe.
  if (!is.null(filt) && nzchar(filt)) {
    dsgn <- dsgn %>% srvyr::filter(!!rlang::parse_expr(filt))
  }

  # 1.2 Remover NAs de la variable de interés para asegurar consistencia.
  if (rm_na_var) {
    dsgn <- dsgn %>% srvyr::filter(!is.na(.data[[var]]))
  }

  # 1.3 Convertir todas las variables categóricas a factor una sola vez.
  if (type == "prop") {
    vars_to_factor <- c(var, des)
    dsgn <- dsgn %>% srvyr::mutate(across(all_of(vars_to_factor), haven::as_factor))
  } else { # type == "mean"
    if (!is.null(des)) {
      dsgn <- dsgn %>% srvyr::mutate(across(all_of(des), haven::as_factor))
    }
  }

  # 1.4 Crear el data.frame base para conteos a partir del diseño YA PROCESADO.
  # Esto asegura que los conteos (n_mues, N_pob) reflejen los datos filtrados.
  base_df <- dsgn$variables %>%
    mutate(.w   = .data[[weight_var]],
           .psu = .data[[psu_var]],
           .str = .data[[strata_var]])

  # A partir de aquí, 'dsgn' es el objeto optimizado y 'base_df' es su data.frame correspondiente.

  # -------------------------------------------------------------------------- #
  # ## FUNCIÓN DE CÁLCULO INTERNA (AHORA MÁS LIGERA) ##
  # Esta función se llama en cada iteración del bucle, pero ya no realiza
  # pre-procesamiento. Opera sobre el 'dsgn' y 'base_df' ya optimizados.
  # -------------------------------------------------------------------------- #
  calc_tabla <- function(grp_des) {
    # El cálculo estadístico principal usa el 'dsgn' pre-procesado.
    if (type == "prop") {
      grp_vars <- c(grp_des, var)
      est <- dsgn %>%
        group_by(across(all_of(grp_vars))) %>%
        summarise(prop = survey_prop(vartype = "se"), .groups = "drop") %>%
        rename(se = prop_se)
    } else {
      grp_vars <- grp_des
      est <- dsgn %>%
        group_by(across(all_of(grp_vars))) %>%
        summarise(media = survey_mean(.data[[var]], vartype = c("se", "cv"), na.rm = TRUE), .groups = "drop") %>%
        rename(se = media_se, cv = media_cv)
    }

    # Los conteos usan el 'base_df' pre-procesado.
    tam_group_vars <- if(type == "prop") c(grp_des, var) else grp_des
    tam <- base_df %>%
      group_by(across(all_of(tam_group_vars))) %>%
      summarise(n_mues = n(), N_pob  = sum(.w), gl = n_distinct(.psu) - n_distinct(.str), .groups = "drop")

    # La lógica de unión y fiabilidad no cambia.
    out <- if (length(tam_group_vars) == 0) bind_cols(est, tam) else left_join(est, tam, by = tam_group_vars)

    if (type == "prop") {
      out <- out %>% mutate(
        se_aceptable = ifelse(prop < .5, (prop^(2/3))/9, ((1 - prop)^(2/3))/9),
        fiabilidad = case_when(
          n_mues == 0 ~ "Sin casos", n_mues < 60 ~ "No fiable - n_mues < 60",
          gl < 9 ~ "No fiable - gl < 9", prop < .5 & se < se_aceptable ~ "Fiable",
          prop < .5 & se >= se_aceptable ~ "Poco fiable - se",
          prop >=.5 & se < se_aceptable ~ "Fiable", TRUE ~ "Poco fiable - se"
        )
      )
    } else {
      out <- out %>% mutate(
        variable = var,
        fiabilidad = case_when(
          is.na(cv) ~ "Sin casos", n_mues < 60  ~ "No fiable - n_mues < 60",
          gl < 9 ~ "No fiable - gl < 9", cv <=  .15 ~ "Fiable",
          cv <=  .30 ~ "Poco fiable - cv", TRUE ~ "No fiable - cv > 0.30"
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

  # -------------------------------------------------------------------------- #
  # ## EJECUCIÓN DEL BUCLE ##
  # La lógica del bucle no cambia, pero ahora es mucho más eficiente
  # porque 'calc_tabla' es más rápida.
  # -------------------------------------------------------------------------- #

  combos <- list(character(0))
  if (!is.null(des)) {
    for (i in 1:length(des)) {
      combos <- c(combos, utils::combn(des, i, simplify = FALSE))
    }
  }

  tablas_loc <- purrr::map(combos, calc_tabla)

  names(tablas_loc) <- purrr::map_chr(combos, ~ if (length(.x) == 0) "nac" else paste(.x, collapse = "__"))

  return(tablas_loc)
}
