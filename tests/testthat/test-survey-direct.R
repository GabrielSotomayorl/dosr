make_direct_backend_design <- function() {
  set.seed(42)
  data <- data.frame(
    stratum = rep(seq_len(12L), each = 12L),
    psu = rep(rep(seq_len(3L), each = 4L), times = 12L),
    weight = runif(144L, 0.5, 4),
    group_a = factor(rep(c("A", "B"), times = 72L)),
    group_b = factor(rep(c("X", "Y", "Z"), each = 48L)),
    category = factor(rep(c("uno", "dos", "tres"), times = 48L)),
    value = rnorm(144L, 100, 20),
    numerator = runif(144L, 1, 10),
    denominator = runif(144L, 2, 12)
  )
  srvyr::as_survey_design(
    data,
    ids = psu,
    strata = stratum,
    weights = weight,
    nest = TRUE
  )
}

srvyr_scalar_reference <- function(design, groups, type) {
  grouped <- if (length(groups)) {
    srvyr::group_by(design, dplyr::across(dplyr::all_of(groups)))
  } else {
    design
  }
  if (type == "mean") {
    return(grouped %>%
      srvyr::summarise(
        estimate = srvyr::survey_mean(value, vartype = "se", na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::rename(se = estimate_se))
  }
  if (type == "total") {
    return(grouped %>%
      srvyr::summarise(
        estimate = srvyr::survey_total(value, vartype = "se", na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::rename(se = estimate_se))
  }
  grouped %>%
    srvyr::summarise(
      estimate = srvyr::survey_ratio(
        numerator, denominator, vartype = "se", na.rm = TRUE
      ),
      .groups = "drop"
    ) %>%
    dplyr::rename(se = estimate_se)
}

test_that("backend directo reproduce medias, totales y razones de srvyr", {
  design <- make_direct_backend_design()
  group_sets <- list(character(), "group_a", c("group_a", "group_b"))

  for (groups in group_sets) {
    for (type in c("mean", "total", "ratio")) {
      direct <- if (type == "ratio") {
        dosr:::.survey_direct_scalar(
          design, groups, type,
          numerator = "numerator", denominator = "denominator"
        )
      } else {
        dosr:::.survey_direct_scalar(
          design, groups, type, variable = "value"
        )
      }
      reference <- srvyr_scalar_reference(design, groups, type)
      keys <- groups
      compared <- if (length(keys)) {
        dplyr::left_join(reference, direct, by = keys, suffix = c("_ref", "_new"))
      } else {
        dplyr::bind_cols(
          dplyr::rename_with(reference, ~ paste0(.x, "_ref")),
          dplyr::rename_with(direct, ~ paste0(.x, "_new"))
        )
      }
      expect_equal(compared$estimate_new, compared$estimate_ref, tolerance = 1e-12)
      expect_equal(compared$se_new, compared$se_ref, tolerance = 1e-12)
      expect_false(".dosr_domain" %in% names(direct))
    }
  }
})

test_that("backend directo conserva la convención de CV para estimaciones negativas", {
  design <- make_direct_backend_design()
  design$variables$value <- -abs(design$variables$value)
  direct <- dosr:::.survey_direct_scalar(
    design, character(), "mean", variable = "value"
  )
  reference <- suppressWarnings(
    srvyr::summarise(
      design,
      estimate = srvyr::survey_mean(
        value, vartype = c("se", "cv"), na.rm = TRUE
      )
    )
  )

  expect_equal(direct$cv, reference$estimate_cv, tolerance = 1e-12)
})

test_that("backend directo conserva NaN o Inf en CV con estimación cero", {
  design <- make_direct_backend_design()
  design$variables$value <- 0
  direct <- dosr:::.survey_direct_scalar(
    design, character(), "total", variable = "value"
  )
  reference <- suppressWarnings(
    srvyr::summarise(
      design,
      estimate = srvyr::survey_total(
        value, vartype = c("se", "cv"), na.rm = TRUE
      )
    )
  )

  expect_equal(direct$cv, reference$estimate_cv)
})

test_that("backend directo rechaza escalares no numéricos", {
  design <- make_direct_backend_design()

  expect_error(
    dosr:::.survey_direct_scalar(
      design, character(), "mean", variable = "category"
    ),
    "debe ser numérica"
  )
  expect_error(
    dosr:::.survey_direct_scalar(
      design, character(), "ratio",
      numerator = "category", denominator = "denominator"
    ),
    "deben ser numéricas"
  )
})

test_that("backend directo reproduce proporciones y errores estándar", {
  design <- make_direct_backend_design()
  group_sets <- list(character(), "group_a", c("group_a", "group_b"))

  for (groups in group_sets) {
    grouped <- if (length(groups)) {
      srvyr::group_by(
        design,
        dplyr::across(dplyr::all_of(c(groups, "category")))
      )
    } else {
      srvyr::group_by(design, category)
    }
    reference <- grouped %>%
      srvyr::summarise(
        prop = srvyr::survey_prop(vartype = "se"),
        .groups = "drop"
      )
    direct <- dosr:::.survey_direct_prop(design, groups, "category")
    compared <- dplyr::left_join(
      reference, direct,
      by = c(groups, "category"),
      suffix = c("_ref", "_new")
    )

    # survey_prop obtains the point through an iterative logit fit, whereas the
    # direct path uses the closed-form survey mean of category indicators.
    expect_equal(compared$prop_new, compared$prop_ref, tolerance = 1e-8)
    expect_equal(compared$se, compared$prop_se, tolerance = 1e-12)
    expect_false(".dosr_domain" %in% names(direct))
  }
})

test_that("backend directo conserva NA como categoría cuando se solicita", {
  design <- make_direct_backend_design()
  design$variables$category[seq(1L, nrow(design$variables), by = 4L)] <- NA

  reference <- design %>%
    srvyr::group_by(group_a, category) %>%
    srvyr::summarise(
      prop = srvyr::survey_prop(vartype = "se"),
      .groups = "drop"
    )
  direct <- dosr:::.survey_direct_prop(design, "group_a", "category")
  compared <- dplyr::full_join(
    reference, direct,
    by = c("group_a", "category"), suffix = c("_ref", "_new")
  )

  expect_true(any(is.na(as.character(direct$category))))
  expect_equal(compared$prop_new, compared$prop_ref, tolerance = 1e-8)
  expect_equal(compared$se, compared$prop_se, tolerance = 1e-12)
})

test_that("backend directo acepta PSU repetidas entre estratos", {
  design <- make_nested_psu_design()
  direct <- dosr:::.survey_direct_scalar(
    design, character(), "mean", variable = "value"
  )
  reference <- survey::svymean(~value, design)

  expect_equal(direct$estimate, as.numeric(stats::coef(reference)), tolerance = 1e-12)
  expect_equal(direct$se, as.numeric(survey::SE(reference)), tolerance = 1e-12)
})

test_that("backend survey conserva grupos con NA", {
  design <- make_direct_backend_design()
  design$variables$group_a[1L] <- NA

  direct <- dosr:::.survey_direct_scalar(
    design, "group_a", "mean", variable = "value"
  )
  reference <- srvyr_scalar_reference(design, "group_a", "mean")
  compared <- dplyr::full_join(
    reference, direct, by = "group_a", suffix = c("_ref", "_new")
  )

  expect_true(any(is.na(direct$group_a)))
  expect_equal(compared$estimate_new, compared$estimate_ref, tolerance = 1e-12)
  expect_equal(compared$se_new, compared$se_ref, tolerance = 1e-12)
})

test_that("backend survey omite categorías ausentes dentro de un dominio", {
  design <- make_direct_backend_design()
  keep <- !(design$variables$group_a == "A" & design$variables$category == "tres")
  design <- srvyr::filter(design, keep)

  direct <- dosr:::.survey_direct_prop(design, "group_a", "category")
  reference <- design %>%
    srvyr::group_by(group_a, category) %>%
    srvyr::summarise(
      prop = srvyr::survey_prop(vartype = "se"),
      .groups = "drop"
    )
  compared <- dplyr::full_join(
    reference, direct,
    by = c("group_a", "category"), suffix = c("_ref", "_new")
  )

  expect_false(any(direct$group_a == "A" & direct$category == "tres"))
  expect_equal(compared$prop_new, compared$prop_ref, tolerance = 1e-8)
  expect_equal(compared$se, compared$prop_se, tolerance = 1e-12)
})

test_that("backend survey admite diseños de pesos replicados", {
  skip_if_not_installed("survey")
  data(api, package = "survey")
  base <- survey::svydesign(
    ids = ~1, strata = ~stype, weights = ~pw, data = apistrat
  )
  replicated <- srvyr::as_survey(
    survey::as.svrepdesign(base, type = "bootstrap", replicates = 20)
  )

  direct_mean <- dosr:::.survey_direct_scalar(
    replicated, "stype", "mean", variable = "api00"
  )
  reference_mean <- replicated %>%
    srvyr::group_by(stype) %>%
    srvyr::summarise(
      estimate = srvyr::survey_mean(api00, vartype = "se", na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::rename(se = estimate_se)
  compared_mean <- dplyr::left_join(
    reference_mean, direct_mean, by = "stype", suffix = c("_ref", "_new")
  )

  direct_prop <- dosr:::.survey_direct_prop(
    replicated, "stype", "sch.wide"
  )
  reference_prop <- replicated %>%
    srvyr::group_by(stype, sch.wide) %>%
    srvyr::summarise(
      prop = srvyr::survey_prop(vartype = "se"),
      .groups = "drop"
    )
  compared_prop <- dplyr::left_join(
    reference_prop, direct_prop,
    by = c("stype", "sch.wide"), suffix = c("_ref", "_new")
  )

  expect_equal(
    compared_mean$estimate_new, compared_mean$estimate_ref, tolerance = 1e-12
  )
  expect_equal(compared_mean$se_new, compared_mean$se_ref, tolerance = 1e-12)
  expect_equal(compared_prop$prop_new, compared_prop$prop_ref, tolerance = 1e-8)
  expect_equal(compared_prop$se, compared_prop$prop_se, tolerance = 1e-12)
})
