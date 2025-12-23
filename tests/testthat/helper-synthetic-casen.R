make_expr_weights <- function(n, seed, p01 = 13, p50 = 75, p99 = 391, sdlog = 0.9) {
  set.seed(seed)
  w <- stats::rlnorm(n, meanlog = 0, sdlog = sdlog)
  w <- w / stats::median(w) * p50
  base::pmin(base::pmax(w, p01), p99)
}

split_into_households <- function(members) {
  sizes <- integer()
  while (sum(sizes) < members) {
    sizes <- c(sizes, sample(1:6, size = 1, prob = c(0.25, 0.25, 0.2, 0.15, 0.1, 0.05)))
  }
  overflow <- sum(sizes) - members
  if (overflow > 0) {
    sizes[length(sizes)] <- max(1, sizes[length(sizes)] - overflow)
  }
  sizes
}

make_svy_casen_synth <- function(n = 400, seed = 123, scenario = c("A", "B", "C"), year = NULL) {
  scenario <- match.arg(scenario)
  set.seed(seed)

  n_strata <- 5
  n_psu <- max(ceiling(n / 16), 10)

  psu_tbl <- data.frame(
    varunit = seq_len(n_psu),
    varstrat = rep(seq_len(n_strata), length.out = n_psu)
  )

  psu_size <- pmin(pmax(stats::rpois(n_psu, lambda = 16), 2), 63)

  household_blocks_list <- vector("list", length(psu_size))
  for (idx in seq_along(psu_size)) {
    members <- psu_size[idx]
    sizes <- split_into_households(members)
    hh_ids <- paste0("H", psu_tbl$varunit[idx], "_", seq_along(sizes))
    household_blocks_list[[idx]] <- data.frame(
      varunit = psu_tbl$varunit[idx],
      varstrat = psu_tbl$varstrat[idx],
      idhogar = rep(hh_ids, sizes),
      tot_per_h = rep(sizes, sizes)
    )
  }

  household_blocks <- dplyr::bind_rows(household_blocks_list)
  if (nrow(household_blocks) > n) {
    household_blocks <- household_blocks[seq_len(n), , drop = FALSE]
  }

  base <- household_blocks

  base <- base %>%
    dplyr::group_by(.data$idhogar) %>%
    dplyr::mutate(
      id_persona = paste0(.data$idhogar, "_", dplyr::row_number()),
      pco1 = as.integer(dplyr::row_number() == 1)
    ) %>%
    dplyr::ungroup()

  set.seed(seed + 2)
  if (scenario == "C") {
    sexo <- rep(1L, nrow(base))
    idx2 <- sample(seq_len(nrow(base)), size = min(8L, nrow(base)))
    sexo[idx2] <- 2L
  } else {
    sexo <- sample(c(1L, 2L), size = nrow(base), replace = TRUE)
  }

  set.seed(seed + 3)
  area <- sample(c(1L, 2L), size = nrow(base), replace = TRUE)

  p_na <- 0.000593
  p1 <- 0.0230
  p2 <- 0.0525
  p3 <- 1 - (p1 + p2 + p_na)
  pobreza_probs <- c(p1, p2, p3, p_na)
  set.seed(seed + 4)
  pobreza_raw <- sample(c(1, 2, 3, NA), size = nrow(base), replace = TRUE, prob = pobreza_probs)

  p_base <- ifelse(area == 2, 0.12, 0.06)
  p_year <- if (!is.null(year) && year == "2022") 0.03 else 0
  p <- pmin(pmax(p_base + p_year, 0.001), 0.9)
  set.seed(seed + 5)
  pobreza_bin <- stats::rbinom(nrow(base), 1, p)

  base_mean <- 500
  mean_shift <- ifelse(sexo == 2, 5, 0)
  year_shift <- if (is.null(year)) 0 else if (year == "2022") 20 else 0
  sd_val <- switch(
    scenario,
    A = 8,
    B = 1500,
    C = 8
  )
  ytrabajocorh <- stats::rnorm(nrow(base), mean = base_mean + mean_shift + year_shift, sd = sd_val)

  expr <- make_expr_weights(nrow(base), seed = seed + 1)

  base <- base %>%
    dplyr::mutate(
      sexo = sexo,
      area = area,
      pco1 = pco1,
      expr = expr,
      ytrabajocorh = ytrabajocorh,
      tot_per_h = tot_per_h,
      pobreza = pobreza_raw,
      pobreza_bin = pobreza_bin
    )

  design_person <- srvyr::as_survey_design(
    base,
    ids = varunit,
    strata = varstrat,
    weights = expr
  )

  design_hogar <- srvyr::as_survey_design(
    dplyr::filter(base, .data$pco1 == 1),
    ids = varunit,
    strata = varstrat,
    weights = expr
  )

  list(
    data = base,
    design_person = design_person,
    design_hogar = design_hogar
  )
}
