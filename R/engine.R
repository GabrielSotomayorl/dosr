# ---------------------------------------------------------------------------- #
# Archivo: engine.R (VERSIÓN CON LÓGICA DE multi_des)
# ---------------------------------------------------------------------------- #

#' @title Internal calculation engine for survey estimates
#' @description This internal function performs the statistical calculations.
#' @noRd
calculate_estimates <- function(dsgn,
                                var, des, filt, rm_na_var, type = c("prop", "mean"),
                                psu_var, strata_var, weight_var, multi_des) {

  type <- match.arg(type)

  # --- Pre-procesamiento (la versión que ya tenías, que era correcta) ---
  if (!is.null(filt) && nzchar(filt)) {
    dsgn <- dsgn %>% srvyr::filter(!!rlang::parse_expr(filt))
  }
  if (rm_na_var) {
    dsgn <- dsgn %>% srvyr::filter(!is.na(.data[[var]]))
  }

  # Usamos la versión de mutate sobre el data.frame extraído, ya que es una buena práctica
  # aunque no represente una gran ganancia de tiempo en este caso.
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

  # --- Función de cálculo interna (sin cambios) ---
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
  # ## LÓGICA DE GENERACIÓN DE COMBINACIONES (MODIFICADA) ##
  # -------------------------------------------------------------------------- #

  combos <- list(character(0)) # Siempre incluir el nivel nacional

  if (!is.null(des)) {
    if (multi_des) {
      # Comportamiento anterior: generar todas las combinaciones posibles
      for (i in 1:length(des)) {
        combos <- c(combos, utils::combn(des, i, simplify = FALSE))
      }
    } else {
      # Nuevo comportamiento: solo desagregaciones simples
      combos_simples <- purrr::map(des, ~ as.character(.x))
      combos <- c(combos, combos_simples)
    }
  }

  tablas_loc <- purrr::map(combos, calc_tabla)

  names(tablas_loc) <- purrr::map_chr(combos, ~ if (length(.x) == 0) "nac" else paste(.x, collapse = "__"))

  return(tablas_loc)
}
