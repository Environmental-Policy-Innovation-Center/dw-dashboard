
# return column of numeric strings with non-numeric characters as numeric column
convert_to_numeric <- function(column) {
  column_numeric <- as.numeric(str_replace_all(column, "[^0-9.]", ""))
  if (any(is.na(column_numeric))) {
    warning("Some values could not be converted to numeric.")
  }
  return(column_numeric)
}
