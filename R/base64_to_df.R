#' Convert base64 encoded data to a data frame
#'
#' This function converts raw base64 results into a data frame.
#' @param x \dots
#' @param check.names logical; if \code{TRUE} (default), column names are
#' made syntactically valid via \code{make.names}. Set to \code{FALSE} to
#' preserve original LimeSurvey headings (spaces, parentheses, etc.).
#' @importFrom utils read.csv
#' @export
#' @examples \dontrun{
#' base64_to_df()
#' }

base64_to_df <- function(x, check.names = TRUE) {
  raw_csv <- rawToChar(base64enc::base64decode(x))

  if (raw_csv == "\r\n") {
    warning("empty data table", call. = F)
    return(data.frame())
  }
  return(read.csv(
    textConnection(raw_csv),
    stringsAsFactors = FALSE,
    sep = ";",
    encoding = "UTF-8",
    check.names = check.names
  ))
}
