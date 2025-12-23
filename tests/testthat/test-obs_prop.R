skip_if_not_installed("srvyr")
skip_if_not_installed("survey")

test_that("obs_prop estructura básica y columnas por año", {
  old_opts <- options(survey.lonely.psu = "adjust")
  on.exit(options(old_opts), add = TRUE)

  s2020 <- make_svy_casen_synth(n = 1000, seed = 201, scenario = "A", year = "2020")
  s2022 <- make_svy_casen_synth(n = 1000, seed = 202, scenario = "A", year = "2022")

  designs <- list("2020" = s2020$design_person, "2022" = s2022$design_person)

  res <- obs_prop(
    designs = designs,
    var = "pobreza_bin",
    des = "area",
    sig = TRUE,
    multi_des = FALSE,
    parallel = FALSE,
    save_xlsx = FALSE,
    verbose = FALSE
  )

  expect_s3_class(res, "data.frame")
  expected_cols <- c(
    "pobreza_bin", "nivel", "area",
    "prop_2020", "prop_2022",
    "fiabilidad_2020", "fiabilidad_2022"
  )
  expect_true(all(expected_cols %in% names(res)))
  diffs <- res$prop_2020 - res$prop_2022
  expect_true(any(!is.na(diffs) & diffs != 0))
})
