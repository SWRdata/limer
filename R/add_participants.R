#' add_participants
#'
#' Adds participants to a survey. Extra columns beyond the known token fields
#' (e.g. \code{ags}, \code{gemeinde}) are created as named LimeSurvey
#' attributes (\code{attribute_1} ...) with matching display descriptions.
#'
#' Email addresses are validated locally against LimeSurvey's rules before
#' any participant table is created. Invalid addresses cause an error unless
#' \code{force = TRUE}.
#'
#' @param iSurveyID integer, ID of the Survey to insert responses
#' @param data dataframe with the columns firstname, lastname and email
#' @param bCreateToken boolean Should tokens be created
#' @param chunksize integer, size of chunks for handling php memory problems
#' @param ask boolean, if TRUE (default) and none of the usual attributes
#' are present, asks for interactive confirmation before continuing. Set
#' to FALSE for non-interactive/scripted use.
#' @param force boolean, if FALSE (default) abort when any email fails
#' LimeSurvey validation before creating/uploading participants. If TRUE,
#' continue and report invalid rows as warnings (API may still reject them).
#'
#' @return data.frame combining the API's per-participant results across
#' all chunks (includes an `errors` column when applicable)
#' @examples
#' \dontrun{
#' add_participants(475835, data = data.frame(
#'   firstname = c("Max", "Moritz"),
#'   lastname = c("Mustermann", "Mueller"),
#'   email = c("m@aol.de", "m@gmx.de")
#' ), bCreateToken = TRUE)
#' }
#' @export
#'
#' @references https://api.limesurvey.org/classes/remotecontrol-handle.html#method_add_participants
add_participants <- function(iSurveyID, data, bCreateToken = FALSE,
                             chunksize = 200, ask = TRUE, force = FALSE) {
  # if data is a character vector of tokens
  if (inherits(data, "character")) {
    data <- data.frame(
      firstname = "", lastname = "", email = "", token = data,
      stringsAsFactors = FALSE
    )
  }
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame or character vector of tokens",
         call. = FALSE)
  }

  default_fields <- c("email", "firstname", "lastname")
  fields <- colnames(data)
  attr_names <- extra_attribute_columns(fields)
  n_fields <- length(attr_names)

  if (!any(default_fields %in% fields)) {
    if (ask) {
      answer <- readline(
        prompt = paste0(
          "None of the usual attributes `firstname`, `lastname` or ",
          "`email` are present in data. Continue anyway (y)?"
        )
      )
      if (tolower(answer) != "y") {
        return("No participants were added")
      }
    } else {
      warning(
        "None of the usual attributes `firstname`, `lastname` or `email` ",
        "are present in data - continuing (ask = FALSE)",
        call. = FALSE
      )
    }
  }

  # Trim character columns before email validation
  data <- data %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.character), stringr::str_trim))

  # Without tokens, invite_participants / mail_survey_invitation always
  # return "No candidate tokens" (LimeSurvey filters on token <> '').
  if (!isTRUE(bCreateToken) && "email" %in% colnames(data) &&
      !"token" %in% colnames(data)) {
    warning(
      "bCreateToken = FALSE: participants are created without access ",
      "tokens. mail_survey_invitation() will then fail. Use ",
      "bCreateToken = TRUE or call add_participant_codes() afterwards.",
      call. = FALSE
    )
  }

  # --- email precheck (before creating participant table) ----------------
  if ("email" %in% colnames(data)) {
    valid <- is_valid_limesurvey_email(data$email)
    invalid_idx <- which(!valid)
    if (length(invalid_idx) > 0L) {
      msg <- format_invalid_email_rows(data, invalid_idx)
      if (!isTRUE(force)) {
        stop(
          "Ung\u00fcltige E-Mail-Adresse(n) \u2013 keine Teilnehmer angelegt ",
          "(force = FALSE):\n", msg,
          call. = FALSE
        )
      }
      warning(
        "Ung\u00fcltige E-Mail-Adresse(n) (force = TRUE) \u2013 Upload wird ",
        "trotzdem versucht:\n", msg,
        call. = FALSE
      )
    }
  }

  table_existed <- exists_participants_table(iSurveyID)

  if (!table_existed) {
    attr_arg <- if (n_fields > 0L) n_fields else NULL
    create_participants_table(iSurveyID, aAttributeFields = attr_arg)
    warning("No participant table found and a new one created", call. = FALSE)
    if (n_fields > 0L) {
      descriptions <- build_attribute_descriptions(attr_names)
      tryCatch(
        set_attribute_descriptions(iSurveyID, descriptions),
        error = function(e) {
          warning(
            "Could not set attributedescriptions: ",
            conditionMessage(e),
            call. = FALSE
          )
        }
      )
      data <- map_columns_to_attributes(data, attr_names)
    }
  } else if (n_fields > 0L) {
    # Map extra columns onto existing attribute_* by description name
    existing <- tryCatch(
      get_attribute_descriptions(iSurveyID),
      error = function(e) list()
    )
    mapping <- match_attributes_by_description(attr_names, existing)
    missing <- names(mapping)[is.na(mapping)]
    if (length(missing) > 0L) {
      stop(
        "Teilnehmer-Tabelle existiert bereits, aber folgende Attribute ",
        "fehlen (keine stillen Drops): ",
        paste(missing, collapse = ", "),
        call. = FALSE
      )
    }
    # Rename by matched attribute field names (may not be 1..n order)
    new_names <- colnames(data)
    for (nm in names(mapping)) {
      idx <- which(new_names == nm)
      if (length(idx) == 1L) {
        new_names[[idx]] <- mapping[[nm]]
      }
    }
    colnames(data) <- new_names
  }

  # Convert rows to named lists for the API
  row_list <- stats::setNames(split(data, seq_len(nrow(data))), rownames(data))
  row_list <- lapply(row_list, function(x) as.list(unlist(x)))

  limit <- 1L
  n <- if (length(row_list) > chunksize) {
    as.integer(ceiling(length(row_list) / chunksize))
  } else {
    1L
  }

  all_resp <- list()
  for (i in seq_len(n)) {
    list_data <- row_list[limit:(limit + chunksize - 1L)]
    list_data <- Filter(function(x) length(x) > 0, list_data)
    # Keep original absolute row indices for error messages
    chunk_row_idx <- as.integer(names(list_data))
    if (length(chunk_row_idx) == 0L || all(is.na(chunk_row_idx))) {
      chunk_row_idx <- seq.int(limit, length.out = length(list_data))
    }

    params <- list(
      "iSurveyID" = iSurveyID,
      "aParticipantData" = list_data,
      "bCreateToken" = bCreateToken
    )
    resp <- call_limer(method = "add_participants", params = params)
    resp <- data.table::rbindlist(resp, fill = TRUE) %>% suppressWarnings()

    if ("errors" %in% colnames(resp)) {
      err_elements <- which(vapply(
        resp$errors,
        function(x) !is.null(x) && !(length(x) == 1L && is.na(x)),
        logical(1)
      ))
      if (length(err_elements) > 0L) {
        lines <- vapply(err_elements, function(j) {
          abs_row <- chunk_row_idx[[j]]
          email_val <- if ("email" %in% colnames(resp)) {
            as.character(resp$email[[j]])
          } else if ("email" %in% names(list_data[[j]])) {
            as.character(list_data[[j]]$email)
          } else {
            NA_character_
          }
          err_txt <- paste(unlist(resp$errors[[j]]), collapse = "; ")
          glue::glue(
            "Zeile {abs_row}: email='{email_val}' \u2013 {err_txt}"
          )
        }, character(1))
        warning(
          "Fehler beim Anlegen von Teilnehmern:\n",
          paste(lines, collapse = "\n"),
          call. = FALSE
        )
      }
    }
    all_resp[[i]] <- resp
    limit <- limit + chunksize
    cat("\r", round((i / n) * 100, digits = 2), "%")
    utils::flush.console()
  }

  cat("\r")
  utils::flush.console()

  data.table::rbindlist(all_resp, fill = TRUE) %>% as.data.frame()
}
