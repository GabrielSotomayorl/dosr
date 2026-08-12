# Synthetic fixtures for survey-design edge cases. These are generated in
# memory so they do not add exported datasets or binary files to the package.

make_nested_psu_data <- function() {
  data.frame(
    strata = rep(seq_len(12L), each = 2L),
    psu = rep(1:2, times = 12L),
    weight = rep(1, 24L),
    value = seq_len(24L),
    binary = rep(0:1, times = 12L),
    ratio_num = rep(c(1, 2), times = 12L),
    ratio_den = rep(c(2, 1), times = 12L)
  )
}

make_nested_psu_design <- function() {
  srvyr::as_survey_design(
    make_nested_psu_data(),
    ids = psu,
    strata = strata,
    weights = weight,
    nest = TRUE
  )
}

make_srs_design <- function() {
  srvyr::as_survey_design(
    data.frame(weight = rep(1, 40L), value = seq_len(40L)),
    weights = weight
  )
}

make_labelled_design <- function(offset = 0) {
  codes <- rep(c(10, 20, 99, 10), times = 10L)
  data <- data.frame(
    strata = rep(seq_len(20L), each = 2L),
    psu = seq_len(40L),
    weight = rep(1, 40L),
    category = haven::labelled(
      codes,
      labels = c(
        "Categoría diez" = 10,
        "Categoría veinte" = 20,
        "Categoría noventa y nueve" = 99
      )
    ),
    value = seq_len(40L) + offset
  )
  srvyr::as_survey_design(
    data,
    ids = psu,
    strata = strata,
    weights = weight,
    nest = TRUE
  )
}

make_multi_binary_data <- function() {
  x1 <- rep(c(1L, 0L, 0L, NA_integer_), times = 10L)
  x2 <- rep(c(1L, 1L, 0L, 0L), times = 10L)
  data.frame(
    strata = rep(seq_len(20L), each = 2L),
    psu = rep(1:2, times = 20L),
    weight = rep(1, 40L),
    domain = rep(c("A", "B"), times = 20L),
    x1 = x1,
    x2 = x2
  )
}

make_multi_binary_design <- function() {
  srvyr::as_survey_design(
    make_multi_binary_data(),
    ids = psu,
    strata = strata,
    weights = weight,
    nest = TRUE
  )
}
