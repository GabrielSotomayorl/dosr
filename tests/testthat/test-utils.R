# test-utils.R — Tests de funciones utilitarias internas.

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
