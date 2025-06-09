# en R/utils.R

#' Create unique column names for a data frame
#' @param df A data frame.
#' @return A data frame with unique column names.
#' @noRd
unique_cols <- function(df) {
  names(df) <- make.unique(names(df))
  df
}

#' Header style for openxlsx tables
#' @noRd
hdrStyle  <- createStyle(
  fgFill = "#D9D9D9", textDecoration = "bold",
  halign = "center", valign = "center",
  border = "TopBottomLeftRight", borderColour = "black"
)

#' Body style for openxlsx tables
#' @noRd
bodyStyle <- createStyle(
  border = "TopBottomLeftRight", borderColour = "black",
  valign = "center"
)

#' Write a data frame to an Excel sheet with clean styling
#' @param wb A workbook object.
#' @param sheet The name of the sheet.
#' @param df The data frame to write.
#' @param startRow Starting row.
#' @param startCol Starting column.
#' @noRd
write_clean_table <- function(wb, sheet, df, startRow = 1, startCol = 1) {
  writeData(wb, sheet, df,
            startRow = startRow, startCol = startCol,
            headerStyle = hdrStyle)
  rows_body <- (startRow + 1):(startRow + nrow(df))
  cols_all  <- startCol:(startCol + ncol(df) - 1)
  addStyle(wb, sheet, hdrStyle,
           rows = startRow, cols = cols_all,
           gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet, bodyStyle,
           rows = rows_body, cols = cols_all,
           gridExpand = TRUE, stack = TRUE)
}

#' Validate that necessary variables exist in the design
#' @param design A survey design object.
#' @param var The main variable.
#' @param des The disaggregation variables.
#' @noRd
validate_inputs <- function(design, var, des) {
  all_vars <- c(var, des)
  design_vars <- names(design$variables)
  missing_vars <- setdiff(all_vars, design_vars)

  if (length(missing_vars) > 0) {
    stop(paste("Las siguientes variables no se encontraron en el diseno:",
               paste(missing_vars, collapse = ", ")), call. = FALSE)
  }
}
