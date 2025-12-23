skip_if_not_installed("srvyr")
skip_if_not_installed("survey")

test_that("obs_media devuelve estructura esperada y calcula medias correctamente", {
  old_opts <- options(survey.lonely.psu = "adjust")
  on.exit(options(old_opts), add = TRUE)

  synth <- make_svy_casen_synth(n = 800, seed = 123, scenario = "A", year = "2022")
  design <- synth$design_person

  res <- obs_media(
    designs = list("2022" = design),
    var = "ytrabajocorh",
    des = "sexo",
    sig = FALSE,
    multi_des = TRUE,
    parallel = FALSE,
    save_xlsx = FALSE,
    verbose = FALSE
  )

  expect_s3_class(res, "data.frame")
  expected_cols <- c(
    "variable", "nivel", "sexo",
    "media_2022", "se_2022", "cv_2022", "fiabilidad_2022"
  )
  expect_true(all(expected_cols %in% names(res)))

  df_manual <- data.frame(
    varunit = rep(1:10, each = 2),
    varstrat = rep(1, 20),
    expr = rep(1, 20),
    sexo = rep(c(1, 2), times = 10),
    ytrabajocorh = ifelse(rep(c(1, 2), times = 10) == 1, 100, 200)
  )
  design_manual <- srvyr::as_survey_design(
    df_manual,
    ids = varunit,
    strata = varstrat,
    weights = expr
  )

  res_manual <- obs_media(
    designs = list(manual = design_manual),
    var = "ytrabajocorh",
    des = "sexo",
    sig = FALSE,
    multi_des = FALSE,
    save_xlsx = FALSE,
    verbose = FALSE
  )

  sexo_manual <- as.character(res_manual$sexo)
  media_manual <- res_manual$media_manual[sexo_manual == "1" & res_manual$nivel == "sexo"]
  expect_equal(media_manual, 100, tolerance = 1e-8)
})

test_that("obs_media etiqueta como Fiable cuando el CV es bajo", {
  old_opts <- options(survey.lonely.psu = "adjust")
  on.exit(options(old_opts), add = TRUE)

  synth <- make_svy_casen_synth(n = 1200, seed = 42, scenario = "A", year = "2022")
  design <- synth$design_person

  res <- obs_media(
    designs = list("2022" = design),
    var = "ytrabajocorh",
    des = "sexo",
    sig = FALSE,
    multi_des = TRUE,
    parallel = FALSE,
    save_xlsx = FALSE,
    verbose = FALSE
  )

  expect_true(any(res$fiabilidad_2022 == "Fiable")) # TODO: endurecer cuando los umbrales sean configurables.
})

test_that("obs_media detecta fiabilidad baja por CV alto o n reducido", {
  old_opts <- options(survey.lonely.psu = "adjust")
  on.exit(options(old_opts), add = TRUE)

  synth_b <- make_svy_casen_synth(n = 1000, seed = 321, scenario = "B", year = "2022")
  design_b <- synth_b$design_person

  res_b <- obs_media(
    designs = list("2022" = design_b),
    var = "ytrabajocorh",
    des = "sexo",
    sig = FALSE,
    multi_des = TRUE,
    parallel = FALSE,
    save_xlsx = FALSE,
    verbose = FALSE
  )

  expect_true(any(res_b$fiabilidad_2022 %in% c("No Fiable", "Poco Fiable")))

  synth_c <- make_svy_casen_synth(n = 160, seed = 456, scenario = "C", year = "2022")
  design_c <- synth_c$design_person

  res_c <- obs_media(
    designs = list("2022" = design_c),
    var = "ytrabajocorh",
    des = "sexo",
    sig = FALSE,
    multi_des = TRUE,
    parallel = FALSE,
    save_xlsx = FALSE,
    verbose = FALSE
  )

  sexo_c <- as.character(res_c$sexo)
  fiab_baja <- res_c$fiabilidad_2022[sexo_c == "2" & res_c$nivel == "sexo"]
  expect_true(any(fiab_baja %in% c("No Fiable", "Sin casos", "Poco Fiable")))
})

test_that("obs_media maneja diferencias hogar vs persona con diseños filtrados", {
  old_opts <- options(survey.lonely.psu = "adjust")
  on.exit(options(old_opts), add = TRUE)

  synth <- make_svy_casen_synth(n = 900, seed = 10, scenario = "A", year = "2022")
  design_person <- synth$design_person
  design_hogar <- synth$design_hogar

  res_persona <- obs_media(
    designs = list("2022" = design_person),
    var = "ytrabajocorh",
    des = "sexo",
    sig = FALSE,
    multi_des = FALSE,
    parallel = FALSE,
    save_xlsx = FALSE,
    verbose = FALSE
  )

  res_hogar <- obs_media(
    designs = list("2022" = design_hogar),
    var = "tot_per_h",
    des = "area",
    sig = FALSE,
    multi_des = FALSE,
    parallel = FALSE,
    save_xlsx = FALSE,
    verbose = FALSE
  )

  expect_s3_class(res_hogar, "data.frame")
  expect_lt(nrow(design_hogar$variables), nrow(design_person$variables))
  expect_true(all(c("media_2022", "fiabilidad_2022") %in% names(res_hogar)))
})

test_that("obs_media combina múltiples años y genera columnas por sufijo", {
  old_opts <- options(survey.lonely.psu = "adjust")
  on.exit(options(old_opts), add = TRUE)

  s2020 <- make_svy_casen_synth(n = 1000, seed = 11, scenario = "A", year = "2020")
  s2022 <- make_svy_casen_synth(n = 1000, seed = 12, scenario = "A", year = "2022")

  designs <- list("2020" = s2020$design_person, "2022" = s2022$design_person)

  res_multi <- obs_media(
    designs = designs,
    var = "ytrabajocorh",
    des = "sexo",
    sig = TRUE,
    multi_des = TRUE,
    parallel = FALSE,
    save_xlsx = FALSE,
    verbose = FALSE
  )

  expect_true(all(c("media_2020", "media_2022", "fiabilidad_2020", "fiabilidad_2022") %in% names(res_multi)))
  expect_true(nrow(res_multi) > 0)
})
