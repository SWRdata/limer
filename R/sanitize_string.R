#' sanitize_string
#'
#' A function to sanitize a string and make it a valid name
#'
#' @param input_vector string
#'
#' @return sanitized string
#'
sanitize_string <- function(input_vector) {
  sanitized_vector <- sapply(input_vector, function(input_string) {
    # Replace spaces with underscores
    cleaned_string <- gsub(" ", "_", input_string)

    # Remove invalid characters
    cleaned_string <- gsub("[^A-Za-z0-9_]", "", cleaned_string)

    # Check for a valid starting character
    if (grepl("^[0-9_]", cleaned_string)) {
      cleaned_string <- paste("x", cleaned_string, sep = "")
    }

    # Check for reserved words (optional)
    reserved_words <- c("mysql", "php", "etc") # Add more reserved words as needed
    if (cleaned_string %in% reserved_words) {
      cleaned_string <- paste("prefix_", cleaned_string, sep = "")
    }

    return(cleaned_string)
  })

  return(unname(sanitized_vector))
}
