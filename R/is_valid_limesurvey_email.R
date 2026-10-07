#' is_valid_limesurvey_email
#'
#' Checks whether an email address is accepted by LimeSurvey's token
#' validator (LSYii_EmailIDNAValidator -> LimeMailer php-idna ->
#' PHPMailer / PHP FILTER_VALIDATE_EMAIL).
#'
#' Empty / NA values are treated as valid (LimeSurvey allowEmpty = TRUE).
#' Multiple addresses separated by comma or semicolon are all checked
#' (allowMultiple = TRUE). Addresses containing CR/LF are rejected.
#'
#' IDN (non-ASCII) domains are not punycode-converted here; validation
#' uses an ASCII-oriented pattern close to PHP's FILTER_VALIDATE_EMAIL.
#'
#' @param email character vector of email addresses (or NA)
#'
#' @return logical vector, TRUE if valid (or empty)
#' @examples
#' is_valid_limesurvey_email(c("a@b.de", "foo@", "", NA))
#' @export
is_valid_limesurvey_email <- function(email) {
  if (length(email) == 0L) {
    return(logical(0))
  }
  vapply(email, is_valid_limesurvey_email_one, logical(1), USE.NAMES = FALSE)
}

#' @noRd
is_valid_limesurvey_email_one <- function(email) {
  if (is.null(email) || length(email) != 1L || is.na(email)) {
    return(TRUE)
  }
  email <- as.character(email)
  if (!nzchar(email)) {
    return(TRUE)
  }
  if (grepl("[\r\n]", email, perl = TRUE)) {
    return(FALSE)
  }
  parts <- strsplit(email, "[,;]", perl = TRUE)[[1]]
  parts <- trimws(parts)
  parts <- parts[nzchar(parts)]
  if (length(parts) == 0L) {
    return(TRUE)
  }
  all(vapply(parts, is_valid_limesurvey_email_atom, logical(1)))
}

#' Single address check approximating PHP FILTER_VALIDATE_EMAIL
#' after LimeSurvey's IDN punyencode step (ASCII domain expected).
#' @noRd
is_valid_limesurvey_email_atom <- function(address) {
  address <- trimws(as.character(address))
  if (!nzchar(address)) {
    return(FALSE)
  }
  if (grepl("[\r\n]", address, perl = TRUE)) {
    return(FALSE)
  }
  pattern <- paste0(
    "^[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@",
    "[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?",
    "(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)+$"
  )
  grepl(pattern, address, perl = TRUE)
}

#' Format rows with invalid emails for stop()/warning() messages.
#' @noRd
format_invalid_email_rows <- function(data, invalid_idx) {
  id_cols <- intersect(c("ags", "gemeinde", "firstname", "lastname"),
                       colnames(data))
  lines <- vapply(invalid_idx, function(i) {
    email_val <- if ("email" %in% colnames(data)) {
      as.character(data$email[[i]])
    } else {
      NA_character_
    }
    extras <- character(0)
    for (col in id_cols) {
      extras <- c(
        extras,
        glue::glue("{col}='{data[[col]][[i]]}'")
      )
    }
    extra_txt <- if (length(extras)) {
      paste0(", ", paste(extras, collapse = ", "))
    } else {
      ""
    }
    glue::glue("Zeile {i}: email='{email_val}'{extra_txt}")
  }, character(1))
  paste(lines, collapse = "\n")
}
