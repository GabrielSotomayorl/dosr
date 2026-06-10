# test-utils.R — Tests de funciones utilitarias internas.

# ── .resolve_filt ────────────────────────────────────────────────────────────

test_that(".resolve_filt: NULL y string vacío devuelven NULL", {
  expect_null(dosr:::.resolve_filt(rlang::quo(NULL)))
  expect_null(dosr:::.resolve_filt(rlang::quo("")))
})

test_that(".resolve_filt: string entre comillas se devuelve tal cual", {
  expect_equal(dosr:::.resolve_filt(rlang::quo("edad > 18")), "edad > 18")
})

test_that(".resolve_filt: expresión sin comillas se convierte a string", {
  expect_equal(dosr:::.resolve_filt(rlang::quo(edad > 18)), "edad > 18")
  expect_equal(dosr:::.resolve_filt(rlang::quo(region %in% c(1, 13))),
               "region %in% c(1, 13)")
})

test_that("obs_media acepta filt como expresión sin comillas", {
  res_str  <- obs_media(dsgn_2022, sufijo = "2022", var = "edad",
                        filt = "sexo == 1", save_xlsx = FALSE, verbose = FALSE)
  res_expr <- obs_media(dsgn_2022, sufijo = "2022", var = "edad",
                        filt = sexo == 1,  save_xlsx = FALSE, verbose = FALSE)
  expect_equal(res_str, res_expr)
})

# ── validate_filt ─────────────────────────────────────────────────────────────

test_that("validate_filt acepta NULL y string vacío sin error", {
  expect_silent(dosr:::validate_filt(NULL))
  expect_silent(dosr:::validate_filt(""))
})

test_that("validate_filt acepta expresiones R válidas", {
  expect_silent(dosr:::validate_filt("edad > 18"))
  expect_silent(dosr:::validate_filt("region == 13 & area == 1"))
})

test_that("validate_filt lanza error con expresión inválida", {
  expect_error(dosr:::validate_filt("edad >>>"),     "válida")
  expect_error(dosr:::validate_filt("( sin cerrar"), "válida")
})

# ── validate_designs ──────────────────────────────────────────────────────────

test_that("validate_designs acepta un tbl_svy sin error", {
  expect_silent(dosr:::validate_designs(dsgn_2022))
})

test_that("validate_designs acepta lista de tbl_svy sin error", {
  expect_silent(dosr:::validate_designs(list(dsgn_2022, dsgn_2024)))
})

test_that("validate_designs falla con data.frame crudo", {
  expect_error(dosr:::validate_designs(casen_2022_mini), "tbl_svy")
})

test_that("validate_designs falla con lista que mezcla tbl_svy y data.frame", {
  expect_error(
    dosr:::validate_designs(list(dsgn_2022, casen_2022_mini)),
    "tbl_svy"
  )
})

# ── truncate_sheet_name ───────────────────────────────────────────────────────

test_that("truncate_sheet_name recorta a 31 caracteres", {
  long_name <- paste(rep("a", 40), collapse = "")
  expect_lte(nchar(dosr:::truncate_sheet_name(long_name)), 31)
})

test_that("truncate_sheet_name no modifica nombres cortos", {
  expect_equal(dosr:::truncate_sheet_name("Mi hoja"), "Mi hoja")
})

test_that("truncate_sheet_name evita colisiones tras truncar", {
  long_a <- paste0(paste(rep("a", 35), collapse = ""), "_x")
  long_b <- paste0(paste(rep("a", 35), collapse = ""), "_y")
  nm1 <- dosr:::truncate_sheet_name(long_a)
  nm2 <- dosr:::truncate_sheet_name(long_b, existing = nm1)
  expect_false(nm1 == nm2)
  expect_lte(nchar(nm2), 31)

  nm3 <- dosr:::truncate_sheet_name(long_b, existing = c(nm1, nm2))
  expect_false(nm3 %in% c(nm1, nm2))
  expect_lte(nchar(nm3), 31)
})

# ── validate_inputs ───────────────────────────────────────────────────────────

test_that("validate_inputs revisa todos los diseños de la lista", {
  dsgn_sin_var <- srvyr::as_survey_design(
    casen_2022_mini[, setdiff(names(casen_2022_mini), "edad")],
    ids = varunit, strata = varstrat, weights = expr, nest = TRUE
  )
  expect_silent(dosr:::validate_inputs(list(a = dsgn_2022, b = dsgn_2024), "edad", NULL))
  expect_error(
    dosr:::validate_inputs(list(a = dsgn_2022, b = dsgn_sin_var), "edad", NULL),
    "edad"
  )
  expect_error(
    dosr:::validate_inputs(list(a = dsgn_2022, b = dsgn_sin_var), "edad", NULL),
    "'b'"
  )
})

# ── n_cores inválido ──────────────────────────────────────────────────────────

test_that("obs_media con n_cores inválido falla con mensaje claro", {
  expect_error(
    obs_media(dsgn_2022, sufijo = "2022", var = "edad",
              parallel = TRUE, n_cores = 0,
              save_xlsx = FALSE, verbose = FALSE),
    "n_cores"
  )
})

# ── validate_dir ──────────────────────────────────────────────────────────────

test_that("obs_* exige 'dir' cuando save_xlsx = TRUE", {
  expect_error(
    obs_media(dsgn_2022, sufijo = "2022", var = "edad", verbose = FALSE),
    "dir"
  )
  expect_silent(dosr:::validate_dir(NULL, save_xlsx = FALSE))
  expect_silent(dosr:::validate_dir(tempdir(), save_xlsx = TRUE))
})

test_that("multi_bin exige 'dir' siempre", {
  expect_error(
    multi_bin(dsgn_2024, vars_binarias = R8_VARS, verbose = FALSE),
    "dir"
  )
})
