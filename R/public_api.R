# ---------------------------------------------------------------------------- #
# Archivo: public_api.R (VERSIÓN FINAL Y VERIFICADA)
# ---------------------------------------------------------------------------- #

#' @title Internal helper function for a single worker process
#' @noRd
calculate_single_design <- function(dsgn, meta, var, des, filt, rm_na_var, type) {
  calculate_estimates(
    dsgn = dsgn,
    var = var, des = des, filt = filt, rm_na_var = rm_na_var, type = type,
    psu_var = meta$psu, strata_var = meta$strata, weight_var = meta$weight
  )
}

# --- FUNCIÓN INTERNA PARA CREAR DISEÑOS LIGEROS ---
create_lightweight_designs <- function(design_list, main_var, des_vars, filter_vars, meta_list) {
  purrr::map2(design_list, meta_list, function(dsgn, meta) {
    design_specific_vars <- c(meta$psu, meta$strata, meta$weight)
    vars_to_keep <- unique(c(main_var, des_vars, filter_vars, design_specific_vars))
    vars_to_keep <- intersect(vars_to_keep, names(dsgn$variables))

    light_dsgn <- dsgn %>%
      srvyr::select(all_of(vars_to_keep))

    return(light_dsgn)
  })
}


#' @title Calcula estimaciones de proporciones para diseños complejos
#' @description Procesa uno o más `tbl_svy` para calcular proporciones.
#' @inheritParams obs_media
#' @param porcentaje Booleano. Muestra las proporciones como porcentajes en Excel.
#' @return Un data.frame con los resultados consolidados (invisiblemente).
#' @export
obs_prop <- function(designs,
                     sufijo      = NULL,
                     var,
                     des         = NULL,
                     filt        = NULL,
                     rm_na_var   = TRUE,
                     parallel    = FALSE,
                     n_cores     = NULL,
                     save_xlsx   = TRUE,
                     formato     = TRUE,
                     porcentaje  = TRUE,
                     decimales   = 2) {

  with_progress({
    p <- progressor(steps = 4)
    p(message = "Fase 1/4: Preparando datos y disenos...")

    if (!inherits(designs, "list")) designs <- list(designs)
    n_designs <- length(designs)
    if (is.null(sufijo)) {
      sufijo <- if (!is.null(names(designs)) && all(nzchar(names(designs)))) names(designs) else as.character(seq_len(n_designs))
    }
    stopifnot("La longitud de 'sufijo' debe coincidir con el numero de disenos." = length(sufijo) == n_designs)
    names(designs) <- sufijo

    design_metadata <- purrr::map(designs, ~list(
      psu    = colnames(.x$cluster)[1],
      strata = colnames(.x$strata)[1],
      weight = names(.x$allprob)[1]
    ))
    vars_in_filter <- if (!is.null(filt)) all.vars(rlang::parse_expr(filt)) else character(0)

    designs_light <- create_lightweight_designs(designs, var, des, vars_in_filter, design_metadata)

    p(message = "Fase 2/4: Calculando estimaciones...")
    pmap_args <- list(dsgn = designs_light, meta = design_metadata)

    if (parallel && n_designs > 1) {
      max_safe_cores <- 4
      cores_detected <- tryCatch(parallel::detectCores(), error = function(e) 2)
      if (is.null(n_cores)) {
        cores <- min(cores_detected - 1, max_safe_cores, n_designs)
        cores <- max(1, cores)
      } else { cores <- n_cores }
      message(paste("Usando modo paralelo con", cores, "workers."))
      old_plan <- future::plan(multisession, workers = cores)
      on.exit(future::plan(old_plan), add = TRUE)

      lista_tablas <- furrr::future_pmap(
        pmap_args,
        calculate_single_design,
        var = var, des = des, filt = filt, rm_na_var = rm_na_var, type = "prop",
        .progress = TRUE
      )

    } else {
      prog <- progressor(along = designs_light)
      lista_tablas <- purrr::pmap(pmap_args, function(dsgn, meta) {
        res <- calculate_single_design(dsgn, meta, var, des, filt, rm_na_var, "prop")
        prog()
        return(res)
      })
    }

    p(message = "Fase 3/4: Agregando y consolidando resultados...")
    keys_prop <- c(var, "nivel", des)
    hojas_list <- aggregate_results(lista_tablas, sufijo, keys = keys_prop, all_designs = designs, type = "prop")

    p(message = "Fase 4/4: Generando archivo Excel...")
    if (save_xlsx) {
      if (!dir.exists("output")) dir.create("output")
      des_tag <- if (!is.null(des)) paste0("_", paste(des, collapse = "-")) else "_nac"
      filename <- file.path("output", paste0(var, des_tag, "_", paste(sufijo, collapse = "-"), "_PROP.xlsx"))

      if (formato) {
        generate_prop_report(hojas_list, filename, var, des, sufijo, porcentaje, decimales, designs = designs)
      } else {
        resultado_final <- bind_rows(hojas_list) %>% unique_cols() %>% select(any_of(keys_prop), everything())
        wb <- createWorkbook()
        addWorksheet(wb, "Consolidado")
        write_clean_table(wb, "Consolidado", resultado_final)
        saveWorkbook(wb, filename, overwrite = TRUE)
        message("Reporte Excel simple creado en: ", filename)
      }
    }
    invisible(bind_rows(hojas_list))
  })
}

#' @title Calcula estimaciones de medias para diseños complejos
#' @description Procesa uno o más `tbl_svy` para calcular medias.
#'
#' @param designs Un objeto `tbl_svy` o una lista de ellos.
#' @param sufijo Vector de strings para sufijos (p.ej. c("2020","2022")).
#' @param var Un string con el nombre de la variable de interés (numérica).
#' @param des Un vector de strings con los nombres de las variables de desagregación.
#' @param filt Un string con una expresión de filtro para `dplyr::filter()`.
#' @param rm_na_var Booleano. Si `TRUE`, elimina NAs en `var` antes de calcular.
#' @param parallel Booleano. Activa el cálculo en paralelo.
#' @param n_cores Entero. Número de núcleos a usar. Si es NULL, se usa un valor seguro.
#' @param save_xlsx Booleano. Si `TRUE`, guarda un reporte en Excel.
#' @param formato Booleano. Si `TRUE`, genera un reporte de Excel con formato avanzado.
#' @param decimales Entero. Número de decimales para las estimaciones en Excel.
#'
#' @return Un data.frame con los resultados consolidados (invisiblemente).
#' @export
obs_media <- function(designs,
                      sufijo      = NULL,
                      var,
                      des         = NULL,
                      filt        = NULL,
                      rm_na_var   = TRUE,
                      parallel    = FALSE,
                      n_cores     = NULL,
                      save_xlsx   = TRUE,
                      formato     = TRUE,
                      decimales   = 2) {

  with_progress({
    p <- progressor(steps = 4)
    p(message = "Fase 1/4: Preparando datos y disenos...")
    if (!inherits(designs, "list")) designs <- list(designs)
    n_designs <- length(designs)
    if (is.null(sufijo)) {
      sufijo <- if (!is.null(names(designs)) && all(nzchar(names(designs)))) names(designs) else as.character(seq_len(n_designs))
    }
    stopifnot("La longitud de 'sufijo' debe coincidir con el numero de disenos." = length(sufijo) == n_designs)
    names(designs) <- sufijo

    design_metadata <- purrr::map(designs, ~list(
      psu    = colnames(.x$cluster)[1],
      strata = colnames(.x$strata)[1],
      weight = names(.x$allprob)[1]
    ))
    vars_in_filter <- if (!is.null(filt)) all.vars(rlang::parse_expr(filt)) else character(0)

    designs_light <- create_lightweight_designs(designs, var, des, vars_in_filter, design_metadata)

    p(message = "Fase 2/4: Calculando estimaciones...")
    pmap_args <- list(dsgn = designs_light, meta = design_metadata)

    if (parallel && n_designs > 1) {
      max_safe_cores <- 4
      cores_detected <- tryCatch(parallel::detectCores(), error = function(e) 2)
      if (is.null(n_cores)) {
        cores <- min(cores_detected - 1, max_safe_cores, n_designs)
        cores <- max(1, cores)
      } else { cores <- n_cores }
      message(paste("Usando modo paralelo con", cores, "workers."))
      old_plan <- future::plan(multisession, workers = cores)
      on.exit(future::plan(old_plan), add = TRUE)

      lista_tablas <- furrr::future_pmap(
        pmap_args,
        calculate_single_design,
        var = var, des = des, filt = filt, rm_na_var = rm_na_var, type = "mean",
        .progress = TRUE
      )
    } else {
      prog <- progressor(along = designs_light)
      lista_tablas <- purrr::pmap(pmap_args, function(dsgn, meta) {
        res <- calculate_single_design(dsgn, meta, var, des, filt, rm_na_var, "mean")
        prog()
        return(res)
      })
    }

    p(message = "Fase 3/4: Agregando y consolidando resultados...")
    keys_media <- c("variable", "nivel", des)
    hojas_list <- aggregate_results(lista_tablas, sufijo, keys = keys_media, all_designs = designs, type = "mean")

    p(message = "Fase 4/4: Generando archivo Excel...")
    if (save_xlsx) {
      if (!dir.exists("output")) dir.create("output")
      des_tag <- if (!is.null(des)) paste0("_", paste(des, collapse = "-")) else "_nac"
      filename <- file.path("output", paste0(var, des_tag, "_", paste(sufijo, collapse = "-"), "_MEDIA.xlsx"))

      if (formato) {
        generate_mean_report(hojas_list, filename, var, des, sufijo, decimales, designs = designs)
      } else {
        resultado_final <- bind_rows(hojas_list) %>% unique_cols() %>% select(any_of(keys_media), everything())
        wb <- createWorkbook()
        addWorksheet(wb, "Consolidado")
        write_clean_table(wb, "Consolidado", resultado_final)
        saveWorkbook(wb, filename, overwrite = TRUE)
        message("Reporte Excel simple creado en: ", filename)
      }
    }
    invisible(bind_rows(hojas_list))
  })
}
