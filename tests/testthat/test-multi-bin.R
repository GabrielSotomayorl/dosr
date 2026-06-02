# test-multi-bin.R — Tests de multi_bin() con variables r8a-r8h de CASEN 2024.
# multi_bin trabaja con un único diseño y variables binarias (0/1).
# Siempre genera Excel; se usa withr::local_tempdir() para contener los archivos.

# ── Output básico ─────────────────────────────────────────────────────────────

test_that("multi_bin devuelve data.frame con columnas esperadas", {
  tmp <- withr::local_tempdir()
  result <- multi_bin(dsgn_2024, vars_binarias = R8_VARS,
                      dir = tmp, verbose = FALSE)
  expect_s3_class(result, "data.frame")
  expect_true(all(c("variable", "etiqueta", "estimacion", "estimacion_se",
                    "n_mues", "fiabilidad") %in% names(result)))
})

test_that("multi_bin produce una fila por variable a nivel nacional", {
  tmp <- withr::local_tempdir()
  result <- multi_bin(dsgn_2024, vars_binarias = R8_VARS,
                      dir = tmp, verbose = FALSE)
  nac_rows <- result[result$desagregacion_tipo == "Nacional", ]
  expect_equal(nrow(nac_rows), length(R8_VARS))
})

test_that("multi_bin produce estimaciones entre 0 y 100 (porcentaje)", {
  tmp <- withr::local_tempdir()
  result <- multi_bin(dsgn_2024, vars_binarias = R8_VARS,
                      dir = tmp, verbose = FALSE)
  vals <- result$estimacion[!is.na(result$estimacion)]
  expect_true(all(vals >= 0 & vals <= 100))
})

test_that("multi_bin fiabilidad solo toma valores esperados", {
  tmp <- withr::local_tempdir()
  result <- multi_bin(dsgn_2024, vars_binarias = R8_VARS,
                      dir = tmp, verbose = FALSE)
  valores_validos <- c(
    "Fiable", "Poco Fiable", "No Fiable", "Sin casos", NA_character_
  )
  expect_true(all(result$fiabilidad %in% valores_validos))
})

test_that("r8a (más común) tiene prevalencia mayor que r8h (más severa)", {
  tmp <- withr::local_tempdir()
  result <- multi_bin(dsgn_2024, vars_binarias = R8_VARS,
                      dir = tmp, verbose = FALSE)
  nac <- result[result$desagregacion_tipo == "Nacional", ]
  est_r8a <- nac$estimacion[nac$variable == "r8a"]
  est_r8h <- nac$estimacion[nac$variable == "r8h"]
  expect_gt(est_r8a, est_r8h)
})

# ── Desagregación ─────────────────────────────────────────────────────────────

test_that("multi_bin con des='sexo' produce filas por cada categoría de sexo", {
  tmp <- withr::local_tempdir()
  result <- multi_bin(dsgn_2024, vars_binarias = R8_VARS, des = "sexo",
                      dir = tmp, verbose = FALSE)
  des_rows <- result[result$desagregacion_tipo == "sexo", ]
  expect_gt(nrow(des_rows), 0)
  cats <- unique(as.character(des_rows$desagregacion_categoria))
  expect_gte(length(cats), 2)
})

test_that("multi_bin con des='region' produce filas para múltiples regiones", {
  tmp <- withr::local_tempdir()
  result <- multi_bin(dsgn_2024, vars_binarias = R8_VARS[1:3], des = "region",
                      dir = tmp, verbose = FALSE)
  des_rows <- result[result$desagregacion_tipo == "region", ]
  n_regiones <- length(unique(des_rows$desagregacion_categoria))
  expect_gte(n_regiones, 10)
})

# ── n_minimo configurable ─────────────────────────────────────────────────────

test_that("n_minimo=0 produce más 'Fiable' que n_minimo=1000", {
  tmp1 <- withr::local_tempdir()
  tmp2 <- withr::local_tempdir()
  res_laxo <- multi_bin(
    dsgn_2024, vars_binarias = R8_VARS, des = "region",
    n_minimo = 0, dir = tmp1, verbose = FALSE
  )
  res_estricto <- multi_bin(
    dsgn_2024, vars_binarias = R8_VARS, des = "region",
    n_minimo = 1000, dir = tmp2, verbose = FALSE
  )
  n_laxo    <- sum(res_laxo$fiabilidad    == "Fiable", na.rm = TRUE)
  n_estricto <- sum(res_estricto$fiabilidad == "Fiable", na.rm = TRUE)
  expect_gte(n_laxo, n_estricto)
})

# ── filt funcional ────────────────────────────────────────────────────────────

test_that("multi_bin con filt reduce el n_mues respecto a sin filt", {
  tmp1 <- withr::local_tempdir()
  tmp2 <- withr::local_tempdir()
  res_sin <- multi_bin(dsgn_2024, vars_binarias = "r8a",
                       dir = tmp1, verbose = FALSE)
  res_con <- multi_bin(dsgn_2024, vars_binarias = "r8a",
                       filt = "as.numeric(region) == 13",
                       dir = tmp2, verbose = FALSE)
  n_sin <- res_sin$n_mues[res_sin$desagregacion_tipo == "Nacional"]
  n_con <- res_con$n_mues[res_con$desagregacion_tipo == "Nacional"]
  expect_lt(n_con, n_sin)
})

# ── Taxonomía de fiabilidad homologada ────────────────────────────────────────

test_that("multi_bin usa etiquetas de fiabilidad específicas (no genéricas)", {
  tmp <- withr::local_tempdir()
  result <- multi_bin(dsgn_2024, vars_binarias = R8_VARS,
                      dir = tmp, verbose = FALSE)
  valid <- c("Fiable", "Sin casos",
             "No Fiable (gl)", "No Fiable (muestra)",
             "Poco Fiable (EE)")
  flab <- unique(result$fiabilidad[!is.na(result$fiabilidad)])
  expect_true(all(flab %in% valid),
              info = paste("Etiquetas inesperadas:", paste(setdiff(flab, valid), collapse = ", ")))
})
