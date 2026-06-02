# test-engine.R — Tests de calculate_estimates() con fixtures CASEN reales.

# ── Medias ────────────────────────────────────────────────────────────────────

test_that("calculate_estimates devuelve lista con clave 'nac' para medias", {
  result <- dosr:::calculate_estimates(
    dsgn_2022, var = "edad", des = NULL, filt = NULL,
    rm_na_var = TRUE, rm_na_des = FALSE, type = "mean",
    psu_var = PSU_VAR, strata_var = STRATA_VAR, weight_var = WEIGHT_VAR,
    multi_des = FALSE, es_var_estudio = FALSE, porcentaje = FALSE
  )
  expect_type(result, "list")
  expect_true("nac" %in% names(result))
})

test_that("resultado de media contiene columnas esperadas", {
  result <- dosr:::calculate_estimates(
    dsgn_2022, var = "edad", des = NULL, filt = NULL,
    rm_na_var = TRUE, rm_na_des = FALSE, type = "mean",
    psu_var = PSU_VAR, strata_var = STRATA_VAR, weight_var = WEIGHT_VAR,
    multi_des = FALSE, es_var_estudio = FALSE, porcentaje = FALSE
  )
  nac <- result[["nac"]]
  expect_true(all(c("media", "se", "cv", "n_mues", "N_pob", "gl", "fiabilidad") %in% names(nac)))
})

test_that("media nacional de edad es un valor plausible", {
  result <- dosr:::calculate_estimates(
    dsgn_2022, var = "edad", des = NULL, filt = NULL,
    rm_na_var = TRUE, rm_na_des = FALSE, type = "mean",
    psu_var = PSU_VAR, strata_var = STRATA_VAR, weight_var = WEIGHT_VAR,
    multi_des = FALSE, es_var_estudio = FALSE, porcentaje = FALSE
  )
  media <- result[["nac"]]$media
  expect_true(is.numeric(media) && media > 10 && media < 100)
})

test_that("fiabilidad solo toma valores del conjunto esperado", {
  result <- dosr:::calculate_estimates(
    dsgn_2022, var = "edad", des = "region", filt = NULL,
    rm_na_var = TRUE, rm_na_des = FALSE, type = "mean",
    psu_var = PSU_VAR, strata_var = STRATA_VAR, weight_var = WEIGHT_VAR,
    multi_des = FALSE, es_var_estudio = FALSE, porcentaje = FALSE
  )
  valores_validos <- c("Fiable", "Poco Fiable", "No Fiable", "Sin casos", NA_character_)
  for (tbl in result) {
    expect_true(all(tbl$fiabilidad %in% valores_validos))
  }
})

# ── Proporciones ──────────────────────────────────────────────────────────────

test_that("proporciones con porcentaje=TRUE produce valores entre 0 y 100", {
  result <- dosr:::calculate_estimates(
    dsgn_2022, var = "pobreza", des = NULL, filt = NULL,
    rm_na_var = TRUE, rm_na_des = FALSE, type = "prop",
    psu_var = PSU_VAR, strata_var = STRATA_VAR, weight_var = WEIGHT_VAR,
    multi_des = FALSE, es_var_estudio = FALSE, porcentaje = TRUE
  )
  nac <- result[["nac"]]
  expect_true(all(nac$prop >= 0 & nac$prop <= 100, na.rm = TRUE))
})

test_that("proporciones con porcentaje=FALSE produce valores entre 0 y 1", {
  result <- dosr:::calculate_estimates(
    dsgn_2022, var = "pobreza", des = NULL, filt = NULL,
    rm_na_var = TRUE, rm_na_des = FALSE, type = "prop",
    psu_var = PSU_VAR, strata_var = STRATA_VAR, weight_var = WEIGHT_VAR,
    multi_des = FALSE, es_var_estudio = FALSE, porcentaje = FALSE
  )
  nac <- result[["nac"]]
  expect_true(all(nac$prop >= 0 & nac$prop <= 1, na.rm = TRUE))
})

test_that("proporción de pobreza produce 3 categorías a nivel nacional", {
  result <- dosr:::calculate_estimates(
    dsgn_2022, var = "pobreza", des = NULL, filt = NULL,
    rm_na_var = TRUE, rm_na_des = FALSE, type = "prop",
    psu_var = PSU_VAR, strata_var = STRATA_VAR, weight_var = WEIGHT_VAR,
    multi_des = FALSE, es_var_estudio = FALSE, porcentaje = FALSE
  )
  expect_equal(nrow(result[["nac"]]), 3)
})

# ── Umbrales configurables ────────────────────────────────────────────────────

test_that("umbrales de CV personalizados afectan la clasificación de fiabilidad", {
  result_estricto <- dosr:::calculate_estimates(
    dsgn_2022, var = "edad", des = "region", filt = NULL,
    rm_na_var = TRUE, rm_na_des = FALSE, type = "mean",
    psu_var = PSU_VAR, strata_var = STRATA_VAR, weight_var = WEIGHT_VAR,
    multi_des = FALSE, es_var_estudio = FALSE, porcentaje = FALSE,
    cv_umbral_alto = 0.01, cv_umbral_medio = 0.005
  )
  result_laxo <- dosr:::calculate_estimates(
    dsgn_2022, var = "edad", des = "region", filt = NULL,
    rm_na_var = TRUE, rm_na_des = FALSE, type = "mean",
    psu_var = PSU_VAR, strata_var = STRATA_VAR, weight_var = WEIGHT_VAR,
    multi_des = FALSE, es_var_estudio = FALSE, porcentaje = FALSE,
    cv_umbral_alto = 0.99, cv_umbral_medio = 0.98
  )
  n_fiable_laxo     <- sum(result_laxo[["region"]]$fiabilidad == "Fiable", na.rm = TRUE)
  n_fiable_estricto <- sum(result_estricto[["region"]]$fiabilidad == "Fiable", na.rm = TRUE)
  expect_gte(n_fiable_laxo, n_fiable_estricto)
})

# ── CV finito ─────────────────────────────────────────────────────────────────

test_that("fiabilidad no es NA cuando CV es finito y media está definida", {
  result <- dosr:::calculate_estimates(
    dsgn_2022, var = "edad", des = NULL, filt = NULL,
    rm_na_var = TRUE, rm_na_des = FALSE, type = "mean",
    psu_var = PSU_VAR, strata_var = STRATA_VAR, weight_var = WEIGHT_VAR,
    multi_des = FALSE, es_var_estudio = FALSE, porcentaje = FALSE
  )
  nac <- result[["nac"]]
  rows_ok <- !is.na(nac$media) & is.finite(nac$media) & is.finite(nac$cv)
  if (any(rows_ok)) {
    expect_false(any(is.na(nac$fiabilidad[rows_ok])))
  }
})

# ── Totales ───────────────────────────────────────────────────────────────────

test_that("calculate_estimates devuelve total positivo para edad", {
  result <- dosr:::calculate_estimates(
    dsgn_2022, var = "edad", des = NULL, filt = NULL,
    rm_na_var = TRUE, rm_na_des = FALSE, type = "total",
    psu_var = PSU_VAR, strata_var = STRATA_VAR, weight_var = WEIGHT_VAR,
    multi_des = FALSE, es_var_estudio = FALSE, porcentaje = FALSE
  )
  expect_gt(result[["nac"]]$total, 0)
})

# ── Cuantiles ─────────────────────────────────────────────────────────────────

test_that("cuantil produce columna 'cuantil' correctamente renombrada", {
  result <- dosr:::calculate_estimates(
    dsgn_2022, var = "edad", des = NULL, filt = NULL,
    rm_na_var = TRUE, rm_na_des = FALSE, type = "quantile",
    psu_var = PSU_VAR, strata_var = STRATA_VAR, weight_var = WEIGHT_VAR,
    multi_des = FALSE, es_var_estudio = FALSE, porcentaje = FALSE,
    quantile_prob = 0.5
  )
  nac <- result[["nac"]]
  expect_true("cuantil" %in% names(nac))
  expect_true(is.numeric(nac$cuantil))
})

test_that("mediana de edad nacional es un valor plausible (20-60 años)", {
  result <- dosr:::calculate_estimates(
    dsgn_2022, var = "edad", des = NULL, filt = NULL,
    rm_na_var = TRUE, rm_na_des = FALSE, type = "quantile",
    psu_var = PSU_VAR, strata_var = STRATA_VAR, weight_var = WEIGHT_VAR,
    multi_des = FALSE, es_var_estudio = FALSE, porcentaje = FALSE,
    quantile_prob = 0.5
  )
  mediana <- result[["nac"]]$cuantil
  expect_true(!is.na(mediana) && mediana > 20 && mediana < 60)
})

test_that("cuantil con desagregación produce 'cuantil' en todas las filas no vacías", {
  result <- dosr:::calculate_estimates(
    dsgn_2022, var = "edad", des = "region", filt = NULL,
    rm_na_var = TRUE, rm_na_des = FALSE, type = "quantile",
    psu_var = PSU_VAR, strata_var = STRATA_VAR, weight_var = WEIGHT_VAR,
    multi_des = FALSE, es_var_estudio = FALSE, porcentaje = FALSE,
    quantile_prob = 0.5
  )
  region_tbl <- result[["region"]]
  expect_true("cuantil" %in% names(region_tbl))
  expect_gt(sum(!is.na(region_tbl$cuantil)), 0)
})

# ── Desagregación con celdas vacías (Sin casos) ───────────────────────────────

test_that("desagregación region x area con multi_des produce 'No Fiable' en celdas pequeñas", {
  result <- dosr:::calculate_estimates(
    dsgn_2022, var = "edad", des = c("region", "area"), filt = NULL,
    rm_na_var = TRUE, type = "mean",
    psu_var = PSU_VAR, strata_var = STRATA_VAR, weight_var = WEIGHT_VAR,
    multi_des = TRUE, es_var_estudio = FALSE, porcentaje = FALSE
  )
  # El fixture tiene celdas pequeñas (pocos PSU por celda) que deben clasificarse
  # como "No Fiable". Las celdas completamente vacías se omiten del resultado.
  tbl <- result[["region__area"]]
  expect_true(any(startsWith(tbl$fiabilidad, "No Fiable"), na.rm = TRUE))
  # Las celdas con 0 obs del fixture no deben aparecer como filas del resultado
  expect_lt(nrow(tbl), 32)
})
