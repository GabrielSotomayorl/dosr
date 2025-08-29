# ---------------------------------------------------------------------------- #
# Archivo: globals.R
# Objetivo: Declarar variables globales para satisfacer a R CMD check y evitar
#           los NOTEs de "no visible binding for global variable".
# ---------------------------------------------------------------------------- #

utils::globalVariables(c(
  ".", ".data", ".psu", ".str", ".w", "prop_se", "media_se", "media_cv",
  "prop", "nivel", "multisession", "n_mues", "cv", "se", "se_umbral_prop",
  "prop_val",

  # Añadido para satisfacer el último check
  "data"
))
