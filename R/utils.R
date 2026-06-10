# ---------------------------------------------------------------------------- #
# Archivo: utils.R (VERSIÓN CON HELPERS ROBUSTECIDOS)
# ---------------------------------------------------------------------------- #

#' @importFrom openxlsx createStyle writeData addStyle mergeCells
#' @importFrom stringr str_trunc
NULL

#' Truncate a sheet name to be compatible with Excel's 31-character limit.
#' If the truncated name collides with one in `existing`, a numeric suffix
#' (~2, ~3, ...) is appended to keep sheet names unique within the workbook.
#' @noRd
truncate_sheet_name <- function(name, existing = character(0)) {
  if (nchar(name) > 31) {
    name <- stringr::str_trunc(name, 31, side = "right", ellipsis = "")
  }
  if (!name %in% existing) {
    return(name)
  }
  i <- 2L
  repeat {
    suffix <- paste0("~", i)
    base <- stringr::str_trunc(name, 31L - nchar(suffix), side = "right", ellipsis = "")
    candidate <- paste0(base, suffix)
    if (!candidate %in% existing) {
      return(candidate)
    }
    i <- i + 1L
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


#' Normalize filt to a string, accepting both quoted strings and bare expressions.
#' @noRd
.resolve_filt <- function(filt_quo) {
  if (rlang::quo_is_null(filt_quo)) return(NULL)
  result <- tryCatch(rlang::eval_tidy(filt_quo), error = function(e) NULL)
  if (is.character(result) && length(result) == 1L) {
    return(if (nzchar(result)) result else NULL)
  }
  expr_text <- paste(deparse(rlang::quo_get_expr(filt_quo)), collapse = " ")
  if (identical(expr_text, "NULL")) return(NULL)
  expr_text
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

#' Validate the output directory for Excel reports
#' @noRd
validate_dir <- function(dir, save_xlsx = TRUE) {
  if (!save_xlsx) return(invisible(NULL))
  if (is.null(dir) || !is.character(dir) || length(dir) != 1L || !nzchar(dir)) {
    stop(
      "Debe especificar 'dir' (directorio donde guardar el reporte Excel). ",
      "Ejemplo: dir = tempdir() o una ruta de su proyecto. ",
      "Para omitir el Excel use save_xlsx = FALSE.",
      call. = FALSE
    )
  }
  invisible(NULL)
}

#' Validate that necessary variables exist in the design(s)
#' Accepts a single tbl_svy or a (possibly named) list of them.
#' @noRd
validate_inputs <- function(design, var, des) {
  designs <- if (inherits(design, "tbl_svy")) list(design) else design
  all_vars <- c(var, des)
  for (i in seq_along(designs)) {
    missing_vars <- setdiff(all_vars, names(designs[[i]]$variables))
    if (length(missing_vars) > 0) {
      etiqueta <- if (length(designs) > 1) {
        nm <- names(designs)[i]
        id <- if (!is.null(nm) && nzchar(nm)) nm else as.character(i)
        paste0(" (dise\u00f1o '", id, "')")
      } else {
        ""
      }
      stop(
        paste0(
          "Las siguientes variables no se encontraron en el dise\u00f1o", etiqueta, ": ",
          paste(missing_vars, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }
  invisible(NULL)
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
