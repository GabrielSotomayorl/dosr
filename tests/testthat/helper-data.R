# helper-data.R — Fixtures CASEN reales para todos los tests.
# Cargado automáticamente por testthat antes de cada archivo de test.
#
# Variables comunes (2022 y 2024):
#   expr, varstrat, varunit, sexo, region, area, edad, pobreza (3 cats),
#   mujer (0/1), hombre (0/1)          ← para obs_ratio (índice de feminidad)
#
# Variables solo en 2024:
#   r8a-r8h (0/1, inseguridad alimentaria) ← para multi_bin
#
# Diseño: 48 estratos × 5 PSU/estrato × 16 regiones; sin single-PSU.

casen_2022_mini <- readRDS(
  system.file("testdata/casen_2022_mini.rds", package = "dosr")
)
casen_2024_mini <- readRDS(
  system.file("testdata/casen_2024_mini.rds", package = "dosr")
)

dsgn_2022 <- srvyr::as_survey_design(
  casen_2022_mini,
  ids     = varunit,
  strata  = varstrat,
  weights = expr,
  nest    = TRUE
)

dsgn_2024 <- srvyr::as_survey_design(
  casen_2024_mini,
  ids     = varunit,
  strata  = varstrat,
  weights = expr,
  nest    = TRUE
)

R8_VARS <- paste0("r8", letters[1:8])

# Nombres de columnas de diseño (usados en llamadas a calculate_estimates)
PSU_VAR    <- "varunit"
STRATA_VAR <- "varstrat"
WEIGHT_VAR <- "expr"
