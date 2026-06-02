# test-excel.R — Verifica que los reportes Excel se generen correctamente.
# Cubre la dimensión "el paquete no cumple su propósito si explota el Excel".

# ── Helpers ───────────────────────────────────────────────────────────────────

# Devuelve el primer .xlsx encontrado en el directorio (falla si no hay ninguno)
find_xlsx <- function(dir) {
  files <- list.files(dir, pattern = "\\.xlsx$", full.names = TRUE)
  if (length(files) == 0) stop("No se generó ningún archivo .xlsx en: ", dir)
  files[[1]]
}

# ── obs_media ─────────────────────────────────────────────────────────────────

test_that("obs_media crea archivo Excel no vacío", {
  tmp <- withr::local_tempdir()
  obs_media(dsgn_2022, var = "edad", dir = tmp, save_xlsx = TRUE, verbose = FALSE)
  xlsx <- find_xlsx(tmp)
  expect_gt(file.size(xlsx), 0)
})

test_that("obs_media Excel contiene hoja Consolidado y hoja nacional", {
  tmp <- withr::local_tempdir()
  obs_media(dsgn_2022, var = "edad", dir = tmp, save_xlsx = TRUE, verbose = FALSE)
  wb <- openxlsx::loadWorkbook(find_xlsx(tmp))
  expect_true("1_Consolidado" %in% names(wb))
  expect_true(any(grepl("nac", names(wb), ignore.case = TRUE)))
})

test_that("obs_media Excel con desagregación crea hoja para la variable de des", {
  tmp <- withr::local_tempdir()
  obs_media(dsgn_2022, var = "edad", des = "region", dir = tmp,
            save_xlsx = TRUE, verbose = FALSE)
  wb <- openxlsx::loadWorkbook(find_xlsx(tmp))
  expect_true(any(grepl("region", names(wb), ignore.case = TRUE)))
})

test_that("obs_media Excel: Consolidado tiene filas con datos", {
  tmp <- withr::local_tempdir()
  obs_media(dsgn_2022, var = "edad", dir = tmp, save_xlsx = TRUE, verbose = FALSE)
  df <- openxlsx::read.xlsx(find_xlsx(tmp), sheet = "1_Consolidado")
  expect_gt(nrow(df), 0)
})

# ── obs_prop ──────────────────────────────────────────────────────────────────

test_that("obs_prop crea archivo Excel no vacío", {
  tmp <- withr::local_tempdir()
  obs_prop(dsgn_2022, var = "pobreza", dir = tmp, save_xlsx = TRUE, verbose = FALSE)
  expect_gt(file.size(find_xlsx(tmp)), 0)
})

test_that("obs_prop Excel contiene hoja Consolidado con datos", {
  tmp <- withr::local_tempdir()
  obs_prop(dsgn_2022, var = "pobreza", dir = tmp, save_xlsx = TRUE, verbose = FALSE)
  wb <- openxlsx::loadWorkbook(find_xlsx(tmp))
  expect_true("1_Consolidado" %in% names(wb))
  df <- openxlsx::read.xlsx(find_xlsx(tmp), sheet = "1_Consolidado")
  expect_gt(nrow(df), 0)
})

test_that("obs_prop Excel con desagregación crea hoja para la variable de des", {
  tmp <- withr::local_tempdir()
  obs_prop(dsgn_2022, var = "pobreza", des = "sexo", dir = tmp,
           save_xlsx = TRUE, verbose = FALSE)
  wb <- openxlsx::loadWorkbook(find_xlsx(tmp))
  expect_true(any(grepl("sexo", names(wb), ignore.case = TRUE)))
})

# ── obs_total ─────────────────────────────────────────────────────────────────

test_that("obs_total crea archivo Excel no vacío", {
  tmp <- withr::local_tempdir()
  obs_total(dsgn_2022, var = "edad", dir = tmp, save_xlsx = TRUE, verbose = FALSE)
  expect_gt(file.size(find_xlsx(tmp)), 0)
})

test_that("obs_total Excel contiene hoja Consolidado con datos", {
  tmp <- withr::local_tempdir()
  obs_total(dsgn_2022, var = "edad", dir = tmp, save_xlsx = TRUE, verbose = FALSE)
  df <- openxlsx::read.xlsx(find_xlsx(tmp), sheet = "1_Consolidado")
  expect_gt(nrow(df), 0)
})

# ── obs_cuantil ───────────────────────────────────────────────────────────────

test_that("obs_cuantil crea archivo Excel no vacío", {
  tmp <- withr::local_tempdir()
  obs_cuantil(dsgn_2022, var = "edad", cuant = 0.5, dir = tmp,
              save_xlsx = TRUE, verbose = FALSE)
  expect_gt(file.size(find_xlsx(tmp)), 0)
})

test_that("obs_cuantil Excel contiene hoja Consolidado con datos", {
  tmp <- withr::local_tempdir()
  obs_cuantil(dsgn_2022, var = "edad", cuant = 0.5, dir = tmp,
              save_xlsx = TRUE, verbose = FALSE)
  df <- openxlsx::read.xlsx(find_xlsx(tmp), sheet = "1_Consolidado")
  expect_gt(nrow(df), 0)
})

# ── Serie de tiempo (múltiples diseños) ───────────────────────────────────────

test_that("obs_media con dos diseños crea Excel con hoja Consolidado multi-año", {
  tmp <- withr::local_tempdir()
  obs_media(
    list(dsgn_2022, dsgn_2024),
    sufijo    = c("2022", "2024"),
    var       = "edad",
    dir       = tmp,
    save_xlsx = TRUE, verbose = FALSE
  )
  wb <- openxlsx::loadWorkbook(find_xlsx(tmp))
  expect_true("1_Consolidado" %in% names(wb))
  df <- openxlsx::read.xlsx(find_xlsx(tmp), sheet = "1_Consolidado")
  # Debe tener columnas para ambos años
  expect_true(any(grepl("2022", names(df))))
  expect_true(any(grepl("2024", names(df))))
})

test_that("obs_prop con dos diseños crea Excel con hoja Consolidado multi-año", {
  tmp <- withr::local_tempdir()
  obs_prop(
    list(dsgn_2022, dsgn_2024),
    sufijo    = c("2022", "2024"),
    var       = "pobreza",
    dir       = tmp,
    save_xlsx = TRUE, verbose = FALSE
  )
  df <- openxlsx::read.xlsx(find_xlsx(tmp), sheet = "1_Consolidado")
  expect_true(any(grepl("2022", names(df))))
  expect_true(any(grepl("2024", names(df))))
})

# ── obs_ratio ─────────────────────────────────────────────────────────────────

test_that("obs_ratio crea archivo Excel no vacío", {
  tmp <- withr::local_tempdir()
  obs_ratio(dsgn_2022, num = "mujer", den = "hombre",
            dir = tmp, save_xlsx = TRUE, verbose = FALSE)
  expect_gt(file.size(find_xlsx(tmp)), 0)
})

test_that("obs_ratio Excel contiene hoja Consolidado con datos", {
  tmp <- withr::local_tempdir()
  obs_ratio(dsgn_2022, num = "mujer", den = "hombre",
            dir = tmp, save_xlsx = TRUE, verbose = FALSE)
  wb <- openxlsx::loadWorkbook(find_xlsx(tmp))
  expect_true("1_Consolidado" %in% names(wb))
  df <- openxlsx::read.xlsx(find_xlsx(tmp), sheet = "1_Consolidado")
  expect_gt(nrow(df), 0)
})

test_that("obs_ratio con desagregación crea hoja de des en Excel", {
  tmp <- withr::local_tempdir()
  obs_ratio(dsgn_2022, num = "mujer", den = "hombre", des = "region",
            dir = tmp, save_xlsx = TRUE, verbose = FALSE)
  wb <- openxlsx::loadWorkbook(find_xlsx(tmp))
  expect_true(any(grepl("region", names(wb), ignore.case = TRUE)))
})

# ── sig = TRUE (tablas de significancia dentro del Excel) ─────────────────────

test_that("obs_media con sig=TRUE y dos diseños crea Excel sin error", {
  tmp <- withr::local_tempdir()
  expect_no_error(
    obs_media(
      list(dsgn_2022, dsgn_2024),
      sufijo    = c("2022", "2024"),
      var       = "edad",
      des       = "region",
      sig       = TRUE,
      dir       = tmp,
      save_xlsx = TRUE, verbose = FALSE
    )
  )
  expect_gt(file.size(find_xlsx(tmp)), 0)
})

test_that("obs_prop con sig=TRUE y dos diseños crea Excel sin error", {
  tmp <- withr::local_tempdir()
  expect_no_error(
    obs_prop(
      list(dsgn_2022, dsgn_2024),
      sufijo    = c("2022", "2024"),
      var       = "pobreza",
      des       = "region",
      sig       = TRUE,
      dir       = tmp,
      save_xlsx = TRUE, verbose = FALSE
    )
  )
  expect_gt(file.size(find_xlsx(tmp)), 0)
})

# ── multi_bin Excel ────────────────────────────────────────────────────────────

test_that("multi_bin crea archivo Excel no vacío", {
  tmp <- withr::local_tempdir()
  multi_bin(dsgn_2024, vars_binarias = R8_VARS,
            dir = tmp, verbose = FALSE)
  expect_gt(file.size(find_xlsx(tmp)), 0)
})

test_that("multi_bin Excel contiene hojas Consolidado y Nacional", {
  tmp <- withr::local_tempdir()
  multi_bin(dsgn_2024, vars_binarias = R8_VARS,
            dir = tmp, verbose = FALSE)
  wb <- openxlsx::loadWorkbook(find_xlsx(tmp))
  expect_true("1_Consolidado" %in% names(wb))
  expect_true("2_Nacional"    %in% names(wb))
})

test_that("multi_bin Excel con desagregación crea hoja de des", {
  tmp <- withr::local_tempdir()
  multi_bin(dsgn_2024, vars_binarias = R8_VARS, des = "sexo",
            dir = tmp, verbose = FALSE)
  wb <- openxlsx::loadWorkbook(find_xlsx(tmp))
  expect_true(any(grepl("sexo", names(wb), ignore.case = TRUE)))
})

test_that("multi_bin hoja Nacional tiene una fila por variable", {
  tmp <- withr::local_tempdir()
  multi_bin(dsgn_2024, vars_binarias = R8_VARS,
            dir = tmp, verbose = FALSE)
  # startRow=5 en multi_bin; read.xlsx con startRow no disponible en todas las
  # versiones, así que validamos a través del Consolidado
  df <- openxlsx::read.xlsx(find_xlsx(tmp), sheet = "1_Consolidado")
  nac_rows <- df[!is.na(df$desagregacion_tipo) &
                   df$desagregacion_tipo == "Nacional", ]
  expect_equal(nrow(nac_rows), length(R8_VARS))
})
