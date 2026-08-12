test_that(".perform_t_test devuelve NA si hay NAs en inputs", {
  expect_true(is.na(dosr:::.perform_t_test(NA, 1, 10, 2, 1, 10)))
  expect_true(is.na(dosr:::.perform_t_test(1, NA, 10, 2, 1, 10)))
  expect_true(is.na(dosr:::.perform_t_test(1, 1, NA, 2, 1, 10)))
})

test_that(".perform_t_test devuelve p-valor entre 0 y 1 con inputs válidos", {
  p <- dosr:::.perform_t_test(est1 = 100, se1 = 5, gl1 = 30,
                               est2 = 110, se2 = 5, gl2 = 30)
  expect_true(is.numeric(p))
  expect_gte(p, 0)
  expect_lte(p, 1)
})

test_that(".perform_t_test devuelve 1 cuando el SE de la diferencia es ~0", {
  p <- dosr:::.perform_t_test(est1 = 100, se1 = 1e-11, gl1 = 30,
                               est2 = 100, se2 = 1e-11, gl2 = 30)
  expect_equal(p, 1)
})

test_that(".perform_t_test devuelve NA cuando SE es Inf", {
  p <- dosr:::.perform_t_test(est1 = 100, se1 = Inf, gl1 = 30,
                               est2 = 110, se2 = 5,   gl2 = 30)
  expect_true(is.na(p))
})

test_that("calculate_significance con un solo sufijo retorna solo intra_year (sin tests inter-año)", {
  hojas <- list(nac = data.frame(prop_2022 = 0.3, se_2022 = 0.02, gl_2022 = 50))
  result <- dosr:::calculate_significance(hojas, sufijo = "2022", type = "prop",
                                           main_var_prop = "cat", des_vars = NULL)
  expect_type(result, "list")
  expect_true("intra_year" %in% names(result))
  expect_null(result$against_last_year)
  expect_null(result$against_national)
})

test_that("significancias de proporciones conservan orden de desagregación y categoría", {
  df <- tibble::tibble(
    categoria = factor(c("Primera", "Segunda", "Tercera"),
                       levels = c("Primera", "Segunda", "Tercera")),
    grupo = factor(c("B", "B", "B"), levels = c("B", "A")),
    nivel = "grupo",
    prop_2022 = c(10, 20, 70), se_2022 = c(1, 1, 1), gl_2022 = 50,
    prop_2024 = c(12, 18, 70), se_2024 = c(1, 1, 1), gl_2024 = 50
  )
  hojas <- list(
    nac = dplyr::select(df, -grupo),
    grupo = df
  )

  result <- dosr:::calculate_significance(
    hojas, c("2022", "2024"), "prop", "categoria", "grupo"
  )
  last <- result$against_last_year$grupo
  intra <- result$intra_year$grupo$`2022`

  expect_equal(names(last)[1:2], c("grupo", "categoria"))
  expect_equal(as.character(last$categoria), c("Primera", "Segunda", "Tercera"))
  expect_equal(as.character(intra$categoria), c("Primera", "Segunda", "Tercera"))
  expect_match(names(intra)[3], "B\\nPrimera", fixed = FALSE)
})
