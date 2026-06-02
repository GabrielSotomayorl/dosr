# ---------------------------------------------------------------------------- #
# Archivo: utils.R (VERSIÓN CON HELPERS ROBUSTECIDOS)
# ---------------------------------------------------------------------------- #

#' @importFrom openxlsx createStyle writeData addStyle mergeCells
#' @importFrom stringr str_trunc
NULL

#' Truncate a sheet name to be compatible with Excel's 31-character limit.
#' @noRd
truncate_sheet_name <- function(name) {
  if (nchar(name) > 31) {
    return(stringr::str_trunc(name, 31, side = "right", ellipsis = ""))
  } else {
    return(name)
  }
}
unique_cols <- function(df) {
  names(df) <- make.unique(names(df))
  df
}
hdrStyle <- createStyle(
  fgFill = "#D9D9D9",
  textDecoration = "bold",
  halign = "center",
  valign = "center",
  border = "TopBottomLeftRight",
  borderColour = "black"
)
bodyStyle <- createStyle(
  border = "TopBottomLeftRight",
  borderColour = "black",
  valign = "center"
)


#' Write a data frame to an Excel sheet with clean and safe styling.
#' This is the centralized function to write all tables.
#' @noRd
write_clean_table <- function(
  wb,
  sheet,
  x,
  startRow = 1,
  startCol = 1,
  na_string = ""
) {
  # ## CORRECCIÓN ##: Guardia defensiva para prevenir el error 'replacement has length zero'
  if (is.null(x) || nrow(x) == 0) {
    return() # No hacer nada si el data.frame está vacío o es NULL
  }

  # Convertir a data.frame para manejar cualquier estructura de entrada
  x <- as.data.frame(x, check.names = FALSE)

  openxlsx::writeData(
    wb,
    sheet,
    x,
    startRow = startRow,
    startCol = startCol,
    headerStyle = hdrStyle,
    na.string = na_string
  )

  rows_body <- (startRow + 1):(startRow + nrow(x))
  cols_all <- startCol:(startCol + ncol(x) - 1)

  openxlsx::addStyle(
    wb,
    sheet,
    bodyStyle,
    rows = rows_body,
    cols = cols_all,
    gridExpand = TRUE,
    stack = TRUE
  )
}


#' Validate that a filter string is parseable R code
#' @noRd
validate_filt <- function(filt) {
  if (is.null(filt) || !nzchar(filt)) return(invisible(NULL))
  tryCatch(
    rlang::parse_expr(filt),
    error = function(e) stop(
      "El argumento 'filt' no es una expresi\u00f3n R v\u00e1lida: ", conditionMessage(e),
      call. = FALSE
    )
  )
  invisible(NULL)
}

#' Validate that designs argument is a tbl_svy or a list of tbl_svy objects
#' @noRd
validate_designs <- function(designs) {
  if (inherits(designs, "tbl_svy")) return(invisible(NULL))
  if (is.list(designs)) {
    bad <- which(!vapply(designs, inherits, logical(1), what = "tbl_svy"))
    if (length(bad) > 0) {
      stop(
        "Todos los elementos de 'designs' deben ser objetos 'tbl_svy' (dise\u00f1os de encuesta de srvyr). ",
        "Los elementos en la(s) posici\u00f3n/es ", paste(bad, collapse = ", "), " no son de ese tipo. ",
        "Use `srvyr::as_survey_design()` para crear el dise\u00f1o.",
        call. = FALSE
      )
    }
    return(invisible(NULL))
  }
  stop(
    "El argumento 'designs' debe ser un objeto 'tbl_svy' o una lista de ellos. ",
    "Objeto recibido: ", paste(class(designs), collapse = "/"), ". ",
    "Use `srvyr::as_survey_design()` para crear el dise\u00f1o.",
    call. = FALSE
  )
}

#' Validate that necessary variables exist in the design
#' @noRd
validate_inputs <- function(design, var, des) {
  all_vars <- c(var, des)
  design_vars <- names(design$variables)
  missing_vars <- setdiff(all_vars, design_vars)

  if (length(missing_vars) > 0) {
    stop(
      paste(
        "Las siguientes variables no se encontraron en el diseno:",
        paste(missing_vars, collapse = ", ")
      ),
      call. = FALSE
    )
  }
}

#' NULL Coalescing Operator
#'
#' Devuelve `b` si `a` es NULL, de lo contrario devuelve `a`.
#' Es una versión simple del operador `%||%` de rlang para evitar dependencias.
#'
#' @param a El valor a probar.
#' @param b El valor por defecto.
#' @noRd
`%||%` <- function(a, b) {
  if (!is.null(a)) a else b
}
