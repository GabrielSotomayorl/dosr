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
