# Direct survey back-end -------------------------------------------------------

# srvyr >= 1.0 evaluates grouped survey summaries one group at a time.  dosr
# requests many related domains, so delegating the grouped operation to
# survey::svyby avoids repeatedly subsetting and rebuilding the design.

.survey_domain_groups <- function(design, groups) {
  if (length(groups) == 0L) {
    return(list(design = design, mapping = NULL))
  }

  grouped <- design$variables %>%
    dplyr::select(dplyr::all_of(groups)) %>%
    dplyr::group_by(dplyr::across(dplyr::everything()), .drop = TRUE)
  mapping <- dplyr::group_keys(grouped) %>%
    dplyr::mutate(.dosr_domain = dplyr::row_number())
  design$variables$.dosr_domain <- dplyr::group_indices(grouped)

  list(design = design, mapping = mapping)
}

.survey_restore_groups <- function(result, mapping, groups) {
  result %>%
    dplyr::left_join(mapping, by = ".dosr_domain") %>%
    dplyr::select(dplyr::all_of(groups), dplyr::everything(),
                  -dplyr::all_of(".dosr_domain"))
}

.survey_direct_scalar <- function(design, groups, type, variable = NULL,
                                  numerator = NULL, denominator = NULL) {
  stopifnot(type %in% c("mean", "total", "ratio"))

  grouped_design <- .survey_domain_groups(design, groups)
  design_local <- grouped_design$design
  if (identical(type, "ratio")) {
    invalid <- c(numerator, denominator)[!vapply(
      design_local$variables[c(numerator, denominator)],
      function(x) is.numeric(x) || is.logical(x), logical(1)
    )]
    if (length(invalid) > 0L) {
      stop(
        "Las variables de una raz\u00f3n deben ser num\u00e9ricas: ",
        paste(invalid, collapse = ", "), ".",
        call. = FALSE
      )
    }
    design_local$variables$.dosr_numerator <- design_local$variables[[numerator]]
    design_local$variables$.dosr_denominator <- design_local$variables[[denominator]]
    formula <- ~.dosr_numerator
    denominator_formula <- ~.dosr_denominator
    statistic <- survey::svyratio
  } else {
    value <- design_local$variables[[variable]]
    if (!is.numeric(value) && !is.logical(value)) {
      stop(
        "La variable '", variable, "' debe ser num\u00e9rica para calcular una ",
        if (identical(type, "mean")) "media." else "total.",
        call. = FALSE
      )
    }
    design_local$variables$.dosr_value <- value
    formula <- ~.dosr_value
    statistic <- if (identical(type, "mean")) survey::svymean else survey::svytotal
  }

  if (length(groups) == 0L) {
    fit <- if (identical(type, "ratio")) {
      statistic(
        formula, denominator_formula, design_local,
        na.rm = TRUE
      )
    } else {
      statistic(formula, design_local, na.rm = TRUE)
    }
    estimate <- as.numeric(stats::coef(fit))
    se <- as.numeric(survey::SE(fit))
    out <- tibble::tibble(estimate = estimate, se = se)
  } else {
    by_formula <- ~.dosr_domain
    fit <- if (identical(type, "ratio")) {
      survey::svyby(
        formula, by_formula, design_local, statistic,
        denominator = denominator_formula,
        na.rm = TRUE, keep.var = TRUE, drop.empty.groups = TRUE
      )
    } else {
      survey::svyby(
        formula, by_formula, design_local, statistic,
        na.rm = TRUE, keep.var = TRUE, drop.empty.groups = TRUE
      )
    }
    fit_df <- tibble::as_tibble(fit)
    estimate_col <- names(fit_df)[2L]
    se_col <- names(fit_df)[3L]
    out <- fit_df %>%
      dplyr::transmute(
        .dosr_domain,
        estimate = as.numeric(.data[[estimate_col]]),
        se = as.numeric(.data[[se_col]])
      ) %>%
      .survey_restore_groups(grouped_design$mapping, groups)
  }

  out %>% dplyr::mutate(cv = se / estimate)
}

.survey_direct_prop <- function(design, groups, variable) {
  grouped_design <- .survey_domain_groups(design, groups)
  design_local <- grouped_design$design
  outcome <- design_local$variables[[variable]]
  if (!is.factor(outcome)) outcome <- factor(outcome)
  outcome_raw <- outcome
  if (anyNA(outcome)) outcome <- addNA(outcome, ifany = TRUE)
  design_local$variables$.dosr_category <- outcome
  category_levels <- levels(outcome)

  indicator_names <- paste0(".dosr_category_", seq_along(category_levels))
  for (i in seq_along(category_levels)) {
    design_local$variables[[indicator_names[[i]]]] <- as.numeric(
      as.numeric(outcome) == i
    )
  }
  indicator_formula <- stats::reformulate(indicator_names)

  fit <- if (length(groups) == 0L) {
    survey::svymean(indicator_formula, design_local, na.rm = TRUE)
  } else {
    survey::svyby(
      indicator_formula,
      ~.dosr_domain,
      design_local,
      survey::svymean,
      na.rm = TRUE,
      keep.var = TRUE,
      drop.empty.groups = TRUE
    )
  }

  if (length(groups) == 0L) {
    out <- tibble::tibble(
      !!variable := factor(
        category_levels, levels = category_levels[!is.na(category_levels)]
      ),
      prop = as.numeric(stats::coef(fit)),
      se = as.numeric(survey::SE(fit))
    )
    observed <- tibble::tibble(!!variable := outcome_raw) %>%
      dplyr::distinct()
    return(dplyr::inner_join(out, observed, by = variable))
  }

  fit_df <- tibble::as_tibble(fit)
  n_categories <- length(category_levels)
  estimate_cols <- names(fit_df)[
    seq.int(2L, 1L + n_categories)
  ]
  se_cols <- names(fit_df)[
    seq.int(2L + n_categories, 1L + 2L * n_categories)
  ]

  out <- purrr::map_dfr(seq_along(category_levels), function(i) {
    fit_df %>%
      dplyr::transmute(
        .dosr_domain,
        !!variable := factor(
          category_levels[[i]],
          levels = category_levels[!is.na(category_levels)]
        ),
        prop = as.numeric(.data[[estimate_cols[[i]]]]),
        se = as.numeric(.data[[se_cols[[i]]]])
      )
  }) %>%
    .survey_restore_groups(grouped_design$mapping, groups)

  observed <- design_local$variables %>%
    dplyr::transmute(
      dplyr::across(dplyr::all_of(groups)),
      !!variable := outcome_raw
    ) %>%
    dplyr::distinct()

  dplyr::inner_join(out, observed, by = c(groups, variable))
}
