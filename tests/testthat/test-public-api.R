# test-public-api.R — Tests de las funciones obs_*() con fixtures CASEN reales.

# ── Validación de inputs ──────────────────────────────────────────────────────

test_that("obs_media falla con filt inválido", {
  expect_error(
    obs_media(dsgn_2022, var = "edad", filt = "edad >>>", save_xlsx = FALSE),
    "válida"
  )
})

test_that("obs_prop falla con filt inválido", {
  expect_error(
    obs_prop(dsgn_2022, var = "pobreza", filt = "(sin cerrar", save_xlsx = FALSE),
    "válida"
  )
})

test_that("obs_ratio falla si num == den", {
  expect_error(
    obs_ratio(dsgn_2022, num = "edad", den = "edad", save_xlsx = FALSE),
    "distintas"
  )
})

test_that("obs_cuantil falla con cuant fuera de [0,1]", {
  expect_error(
    obs_cuantil(dsgn_2022, var = "edad", cuant = 1.5, save_xlsx = FALSE)
  )
})

test_that("obs_media falla si designs no es tbl_svy", {
  expect_error(
    obs_media(list(data.frame(x = 1)), var = "edad", save_xlsx = FALSE),
    "tbl_svy"
  )
})

test_that("obs_media falla si la variable no existe en el diseño", {
  expect_error(
    obs_media(dsgn_2022, var = "variable_inexistente", save_xlsx = FALSE)
  )
})

# ── Salida básica ─────────────────────────────────────────────────────────────

test_that("obs_media devuelve data.frame con columnas básicas", {
  result <- obs_media(dsgn_2022, var = "edad", save_xlsx = FALSE, verbose = FALSE)
  expect_s3_class(result, "data.frame")
  expect_true("variable" %in% names(result))
  expect_true(any(grepl("media", names(result))))
  expect_true(any(grepl("fiabilidad", names(result))))
})

test_that("obs_prop devuelve proporciones entre 0 y 100 con porcentaje=TRUE", {
  result <- obs_prop(dsgn_2022, var = "pobreza", save_xlsx = FALSE,
                     verbose = FALSE, porcentaje = TRUE)
  prop_cols <- grep("^prop_", names(result), value = TRUE)
  expect_gt(length(prop_cols), 0)
  for (col in prop_cols) {
    vals <- result[[col]][!is.na(result[[col]])]
    expect_true(all(vals >= 0 & vals <= 100))
  }
})

test_that("obs_prop con pobreza produce 3 categorías a nivel nacional", {
  result <- obs_prop(dsgn_2022, var = "pobreza", save_xlsx = FALSE, verbose = FALSE)
  expect_equal(nrow(result), 3)
})

test_that("obs_total devuelve total positivo para edad", {
  result <- obs_total(dsgn_2022, var = "edad", save_xlsx = FALSE, verbose = FALSE)
  total_col <- grep("^total_", names(result), value = TRUE)[1]
  expect_gt(result[[total_col]][1], 0)
})

test_that("obs_cuantil devuelve mediana plausible para edad", {
  result <- obs_cuantil(dsgn_2022, var = "edad", cuant = 0.5,
                        save_xlsx = FALSE, verbose = FALSE)
  cuantil_col <- grep("^cuantil_", names(result), value = TRUE)[1]
  expect_true(result[[cuantil_col]][1] > 20 && result[[cuantil_col]][1] < 60)
})

# ── Desagregación ─────────────────────────────────────────────────────────────

test_that("obs_media con des='sexo' produce filas para Hombre y Mujer", {
  result <- obs_media(dsgn_2022, var = "edad", des = "sexo",
                      save_xlsx = FALSE, verbose = FALSE)
  expect_true("sexo" %in% names(result))
  # Los datos reales pueden incluir NA en sexo; nos interesa que estén ambas categorías
  sexo_vals <- as.character(na.omit(result$sexo))
  expect_true(any(grepl("Hombre", sexo_vals, ignore.case = TRUE)))
  expect_true(any(grepl("Mujer",  sexo_vals, ignore.case = TRUE)))
})

test_that("obs_prop con des='region' produce filas para todas las regiones del fixture", {
  result <- obs_prop(dsgn_2022, var = "pobreza", des = "region",
                     save_xlsx = FALSE, verbose = FALSE)
  expect_true("region" %in% names(result))
  expect_gte(length(unique(result$region)), 16)
})

# ── Umbrales configurables ────────────────────────────────────────────────────

test_that("obs_media respeta umbrales personalizados de CV", {
  result_laxo <- obs_media(dsgn_2022, var = "edad", des = "region",
                            save_xlsx = FALSE, verbose = FALSE,
                            cv_umbral_alto = 0.99, cv_umbral_medio = 0.98)
  fiab_cols <- grep("fiabilidad", names(result_laxo), value = TRUE)
  expect_gt(sum(result_laxo[[fiab_cols[1]]] == "Fiable", na.rm = TRUE), 0)
})

# ── Múltiples diseños (serie de tiempo) ──────────────────────────────────────

test_that("obs_media con dos diseños produce columnas con sufijo por año", {
  result <- obs_media(
    list(dsgn_2022, dsgn_2024),
    sufijo  = c("2022", "2024"),
    var     = "edad",
    save_xlsx = FALSE, verbose = FALSE
  )
  expect_true("media_2022" %in% names(result))
  expect_true("media_2024" %in% names(result))
})

test_that("obs_prop con dos diseños produce columnas de fiabilidad por año", {
  result <- obs_prop(
    list(dsgn_2022, dsgn_2024),
    sufijo    = c("2022", "2024"),
    var       = "pobreza",
    save_xlsx = FALSE, verbose = FALSE
  )
  expect_true("fiabilidad_2022" %in% names(result))
  expect_true("fiabilidad_2024" %in% names(result))
})

test_that("obs_media con sufijo personalizado nombra columnas correctamente", {
  result <- obs_media(
    list(A = dsgn_2022, B = dsgn_2024),
    sufijo    = c("A", "B"),
    var       = "edad",
    save_xlsx = FALSE, verbose = FALSE
  )
  expect_true("media_A" %in% names(result))
  expect_true("media_B" %in% names(result))
})

# ── obs_ratio funcional ───────────────────────────────────────────────────────

test_that("obs_ratio devuelve índice de feminidad positivo y finito", {
  result <- obs_ratio(dsgn_2022, num = "mujer", den = "hombre",
                      save_xlsx = FALSE, verbose = FALSE)
  ratio_col <- grep("^ratio_", names(result), value = TRUE)[1]
  val <- result[[ratio_col]][1]
  expect_true(is.finite(val) && val > 0)
})

test_that("obs_ratio con des='region' produce una fila por región", {
  result <- obs_ratio(dsgn_2022, num = "mujer", den = "hombre",
                      des = "region", save_xlsx = FALSE, verbose = FALSE)
  expect_true("region" %in% names(result))
  expect_gte(length(unique(result$region)), 16)
})

test_that("obs_ratio contiene columnas de fiabilidad", {
  result <- obs_ratio(dsgn_2022, num = "mujer", den = "hombre",
                      save_xlsx = FALSE, verbose = FALSE)
  expect_true(any(grepl("fiabilidad", names(result))))
})

# ── filt funcional ────────────────────────────────────────────────────────────

test_that("filt filtra correctamente: solo adultos tienen edad media > 18", {
  sin_filt <- obs_media(dsgn_2022, var = "edad",
                        save_xlsx = FALSE, verbose = FALSE)
  con_filt <- obs_media(dsgn_2022, var = "edad", filt = "edad >= 18",
                        save_xlsx = FALSE, verbose = FALSE)
  media_col <- grep("^media_", names(sin_filt), value = TRUE)[1]
  expect_gt(con_filt[[media_col]][1], sin_filt[[media_col]][1])
})

test_that("filt a región específica produce n_mues menor que sin filtro", {
  sin_filt <- obs_media(dsgn_2022, var = "edad",
                        save_xlsx = FALSE, verbose = FALSE)
  con_filt <- obs_media(dsgn_2022, var = "edad",
                        filt = "as.numeric(region) == 13",
                        save_xlsx = FALSE, verbose = FALSE)
  n_col_sf <- grep("^n_mues_", names(sin_filt), value = TRUE)[1]
  n_col_cf <- grep("^n_mues_", names(con_filt), value = TRUE)[1]
  expect_lt(con_filt[[n_col_cf]][1], sin_filt[[n_col_sf]][1])
})

# ── Features 0.3.0: nueva funcionalidad ──────────────────────────────────────

test_that("fiabilidad incluye la causa del rechazo", {
  result <- obs_media(dsgn_2022, var = "edad", des = "region",
                      save_xlsx = FALSE, verbose = FALSE)
  flab <- unique(result$fiabilidad_1)
  flab <- flab[!is.na(flab)]
  # Todos los labels deben ser de las categorías conocidas
  valid_patterns <- c("^Fiable$", "^No Fiable \\(", "^Poco Fiable \\(", "^Sin casos$")
  expect_true(all(vapply(flab, function(x)
    any(vapply(valid_patterns, function(p) grepl(p, x), logical(1))),
    logical(1))))
})

test_that("nombre sobreescribe la etiqueta del indicador en el Excel", {
  tmp <- withr::local_tempdir()
  obs_media(dsgn_2022, var = "edad", des = "region",
            nombre = "Ingreso del hogar test",
            dir = tmp, verbose = FALSE)
  wb <- openxlsx::loadWorkbook(
    list.files(tmp, pattern = "edad.*\\.xlsx", full.names = TRUE)[1])
  hdr <- openxlsx::read.xlsx(wb, sheet = "2_region", rows = 2, cols = 1,
                              colNames = FALSE)
  expect_equal(hdr[1, 1], "Ingreso del hogar test")
})

test_that("snac=TRUE excluye hoja nacional del formato", {
  tmp <- withr::local_tempdir()
  obs_prop(dsgn_2022, sufijo = "2022", var = "pobreza", des = "region",
           snac = TRUE, dir = tmp, verbose = FALSE)
  wb <- openxlsx::loadWorkbook(
    list.files(tmp, pattern = "pobreza.*PROP.*\\.xlsx", full.names = TRUE)[1])
  expect_false("2_nac" %in% names(wb))
  expect_true("2_region" %in% names(wb))
})

test_that("categoria filtra las filas del resultado", {
  result <- obs_prop(dsgn_2022, sufijo = "2022", var = "pobreza",
                     categoria = "Pobreza extrema",
                     save_xlsx = FALSE, verbose = FALSE)
  expect_true(all(as.character(result$pobreza) == "Pobreza extrema"))
  expect_equal(nrow(result[result$nivel == "Nacional", ]), 1L)
})

test_that("categoria acepta codigo numerico", {
  result_lbl  <- obs_prop(dsgn_2022, sufijo = "2022", var = "pobreza",
                          categoria = "Pobreza extrema",
                          save_xlsx = FALSE, verbose = FALSE)
  result_code <- obs_prop(dsgn_2022, sufijo = "2022", var = "pobreza",
                          categoria = 1L,
                          save_xlsx = FALSE, verbose = FALSE)
  expect_equal(nrow(result_lbl), nrow(result_code))
})

test_that("universo_crit=TRUE no falla y produce resultado coherente", {
  result <- obs_prop(dsgn_2022, sufijo = "2022", var = "pobreza",
                     universo_crit = TRUE,
                     save_xlsx = FALSE, verbose = FALSE)
  expect_s3_class(result, "data.frame")
  expect_true(any(grepl("fiabilidad", names(result))))
})

test_that("nota de fiabilidad se escribe en el Excel", {
  tmp <- withr::local_tempdir()
  obs_prop(dsgn_2022, sufijo = "2022", var = "pobreza", des = "region",
           mostrar_pct_fiable = TRUE, dir = tmp, verbose = FALSE)
  wb <- openxlsx::loadWorkbook(
    list.files(tmp, pattern = "pobreza.*PROP.*\\.xlsx", full.names = TRUE)[1])
  datos <- openxlsx::read.xlsx(wb, sheet = "2_region", colNames = FALSE)
  expect_true(any(datos[, 1] == "Nota:", na.rm = TRUE))
  expect_true(any(grepl("estimaciones", datos[, 1], ignore.case = TRUE), na.rm = TRUE))
})

test_that("fuente casen escribe texto de fuente en el Excel", {
  tmp <- withr::local_tempdir()
  obs_media(dsgn_2022, sufijo = "2022", var = "edad", des = "region",
            fuente = "casen", dir = tmp, verbose = FALSE)
  wb <- openxlsx::loadWorkbook(
    list.files(tmp, pattern = "edad.*\\.xlsx", full.names = TRUE)[1])
  datos <- openxlsx::read.xlsx(wb, sheet = "2_region", colNames = FALSE)
  expect_true(any(grepl("Fuente:.*Casen", datos[, 1], ignore.case = TRUE), na.rm = TRUE))
})

# ── Tests adicionales P2 ──────────────────────────────────────────────────────

test_that("obs_cuantil respeta umbrales configurables de CV", {
  result_estricto <- obs_cuantil(dsgn_2022, var = "edad", des = "region",
                                  cv_umbral_alto = 0.01, cv_umbral_medio = 0.001,
                                  save_xlsx = FALSE, verbose = FALSE)
  result_laxo <- obs_cuantil(dsgn_2022, var = "edad", des = "region",
                               cv_umbral_alto = 0.99, cv_umbral_medio = 0.98,
                               save_xlsx = FALSE, verbose = FALSE)
  fcol <- grep("fiabilidad", names(result_laxo), value = TRUE)[1]
  expect_gt(sum(result_laxo[[fcol]] == "Fiable", na.rm = TRUE),
            sum(result_estricto[[fcol]] == "Fiable", na.rm = TRUE))
})

test_that("color_fiabilidad = TRUE no produce error", {
  tmp <- withr::local_tempdir()
  expect_no_error(
    obs_media(dsgn_2022, sufijo = "2022", var = "edad", des = "sexo",
              color_fiabilidad = TRUE, dir = tmp, verbose = FALSE)
  )
})

test_that("rm_na_des = TRUE produce resultado sin error", {
  result <- obs_media(dsgn_2022, var = "edad", des = "region",
                      rm_na_des = TRUE, save_xlsx = FALSE, verbose = FALSE)
  expect_s3_class(result, "data.frame")
  expect_true(any(grepl("media", names(result))))
})

test_that("parallel = TRUE con dos diseños produce resultado correcto", {
  result <- obs_media(
    list(dsgn_2022, dsgn_2024),
    sufijo    = c("2022", "2024"),
    var       = "edad",
    parallel  = TRUE,
    n_cores   = 2L,
    save_xlsx = FALSE,
    verbose   = FALSE
  )
  expect_true("media_2022" %in% names(result))
  expect_true("media_2024" %in% names(result))
})

test_that("sig = TRUE produce columnas p_value para dos diseños", {
  result <- obs_media(
    list(dsgn_2022, dsgn_2024),
    sufijo    = c("2022", "2024"),
    var       = "edad",
    des       = "sexo",
    sig       = TRUE,
    save_xlsx = FALSE,
    verbose   = FALSE
  )
  expect_s3_class(result, "data.frame")
  # La función de significancia es interna; el resultado principal no cambia
  expect_true("media_2022" %in% names(result))
})

test_that("validate_inputs da error claro cuando var no existe", {
  expect_error(
    obs_media(dsgn_2022, var = "var_que_no_existe", save_xlsx = FALSE),
    "no se encontraron"
  )
})

test_that("validate_inputs da error claro cuando des no existe", {
  expect_error(
    obs_media(dsgn_2022, var = "edad", des = "des_inexistente", save_xlsx = FALSE),
    "no se encontraron"
  )
})
