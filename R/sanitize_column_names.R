#' Clean column names
#' Removes unwanted spaces and invisible special and control characters from the
#' column names of a DataFrame
#' @param x characterer vector of column names
#'
#' @returns cleaned vector of column names
#' @export
#'

sanitize_column_names <- function(x) {
  x <- enc2utf8(x)

  # Unicode-Whitespace durch normales Leerzeichen ersetzen
  x <- gsub("[\\p{Z}\\s]+", " ", x, perl = TRUE)

  # Unsichtbare Formatierungs- und Steuerzeichen entfernen
  x <- gsub("[\\p{Cc}\\p{Cf}]", "", x, perl = TRUE)

  # Leerzeichen am Anfang und Ende entfernen
  x <- trimws(x)

  x
}
