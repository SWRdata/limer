#' Export data from a LimeSurvey survey
#'
#' This function exports and downloads data from a LimeSurvey survey.
#' @param iSurveyID string ID of the Survey
#' @param sDocumentType string any format available by plugins (for example :
#' pdf, csv, xls, doc, json).
#' @param sLanguageCode string (optional) The language to be used.
#' @param sCompletionStatus string (optional) 'complete','incomplete' or 'all'
#' - defaults to 'all'.
#' @param sHeadingType string (optional) 'code','full' or 'abbreviated'.
#' defaults to 'code'.
#' @param sResponseType (optional)'short' or 'long'. defaults to 'short'
#' @param remove_tech_cols logical, if \code{TRUE} drop LimeSurvey meta columns
#'   (id, dates, ip, …) after export. The access code column
#'   (\code{token} / \code{Zugangscode}) is kept. Default = \code{FALSE}.
#' @param add_participant_data logical, if \code{TRUE} left-join participant
#'   data from \code{\link{get_participants}} on \code{token}. Only participant
#'   columns that contain at least one non-empty value among matched tokens
#'   are kept (e.g. blank \code{firstname}/\code{lastname} are dropped).
#'   Default = \code{FALSE}.
#' @param \dots Further arguments passed to internal function \code{call_limer()}.
#' @export
#' @examples \dontrun{
#' get_responses(475835)
#' get_responses(475835, remove_tech_cols = TRUE, add_participant_data = TRUE)
#' }
#' @references \url{https://api.limesurvey.org/classes/remotecontrol-handle.html#method_export_responses}

get_responses <- function(iSurveyID, sDocumentType = "csv", sLanguageCode = NULL,
                          sCompletionStatus = "complete", sHeadingType = "code",
                          sResponseType = "long", remove_tech_cols = FALSE,
                          add_participant_data = FALSE, ...) {
  # Put all the function's arguments in a list to then be passed to call_limer()
  params <- as.list(environment())
  dots <- list(...)
  if (length(dots) > 0) params <- append(params, dots)
  # Local convenience flags must not reach export_responses.
  params$remove_tech_cols <- NULL
  params$add_participant_data <- NULL
  # print(params) # uncomment to debug the params

  results <- call_limer(method = "export_responses", params = params)

  # export_responses returns list(status = "...") on API errors (e.g. unknown
  # language). Do not base64-decode that string — it becomes binary garbage
  # and crashes later in clean_full_heading_names()/gsub().
  if (is.list(results) && !is.null(results$status)) {
    stop(
      glue::glue(
        "Could not export responses for survey \u00b4{iSurveyID}\u00b4: ",
        "{results$status}"
      ),
      call. = FALSE
    )
  }

  if (grepl("No Data, ", unlist(results))) {
    warning(unlist(results), call. = F)
    return(data.frame())
  }

  df <- base64_to_df(
    unlist(results),
    check.names = !identical(sHeadingType, "full")
  )
  if (identical(sHeadingType, "full")) {
    colnames(df) <- clean_full_heading_names(colnames(df))
  }

  if (isTRUE(remove_tech_cols)) {
    df <- drop_tech_response_cols(df)
  }

  if (isTRUE(add_participant_data)) {
    df <- merge_participant_data(df, iSurveyID)
  }

  return(df)
}

#' Clean LimeSurvey full-heading column names for readability.
#'
#' Replaces the LimeSurvey question/answer separator (\code{...}) with
#' \code{" - "} and collapses whitespace. Intended for headings read with
#' \code{check.names = FALSE} so parentheses like \code{(en)} stay intact.
#'
#' @param col_names character vector of column names
#' @return character vector of cleaned names
#' @noRd
clean_full_heading_names <- function(col_names) {
  col_names <- gsub("\\.{3,}", " - ", col_names)
  col_names <- gsub("\\s+", " ", col_names)
  trimws(col_names)
}

#' LimeSurvey response meta column names (codes + DE/EN full headings).
#'
#' Does not include token / Zugangscode / Access code — those are kept.
#' @noRd
limesurvey_tech_response_cols <- function() {
  c(
    # fieldmap codes
    "id", "submitdate", "lastpage", "startlanguage", "seed",
    "startdate", "datestamp", "ipaddr", "refurl",
    # German full headings
    "Antwort ID", "Antwort-ID", "Datum abgeschickt", "Datum gestartet",
    "Letzte Seite", "Startsprache", "IP-Adresse", "Referrer-URL", "Start-Sprache",
    "Referrer URL", "Zufallsstartwert", "Datum letzte Aktivit\u00e4t",
    # English full headings
    "Response ID", "Date submitted", "Date started", "Last page",
    "Start language", "IP address", "Referrer url"
  )
}

#' Token / access-code column names kept when dropping tech cols.
#' @noRd
limesurvey_token_response_cols <- function() {
  c("token", "Zugangscode", "Access code")
}

#' Drop LimeSurvey technical meta columns; keep token/access code.
#' @param df data.frame of exported responses
#' @return data.frame without tech columns
#' @noRd
drop_tech_response_cols <- function(df) {
  if (!is.data.frame(df) || ncol(df) == 0L) {
    return(df)
  }
  tech <- tolower(limesurvey_tech_response_cols())
  keep_token <- tolower(limesurvey_token_response_cols())
  cols_lower <- tolower(colnames(df))
  drop_idx <- cols_lower %in% tech & !cols_lower %in% keep_token
  df[, !drop_idx, drop = FALSE]
}

#' Whether a column is completely empty.
#'
#' LimeSurvey participant fields are often \code{""} rather than \code{NA}.
#' Cells that are NA, blank/whitespace, or placeholder strings are treated as
#' empty; a column is empty if every cell is empty.
#' @noRd
is_completely_empty_col <- function(x) {
  if (length(x) == 0L) {
    return(TRUE)
  }
  if (is.list(x) && !is.data.frame(x)) {
    x <- vapply(
      x,
      function(el) {
        if (is.null(el) || length(el) == 0L) {
          return("")
        }
        paste(as.character(unlist(el, use.names = FALSE)), collapse = " ")
      },
      character(1)
    )
  }
  # Canonicalise to "" so LimeSurvey's empty strings are the normal case.
  x_chr <- trimws(as.character(x))
  x_chr[is.na(x) | is.na(x_chr)] <- ""
  x_chr[tolower(x_chr) %in% c("na", "null", "n/a", "nan")] <- ""
  all(!nzchar(x_chr))
}

#' Drop columns that are completely empty.
#' @noRd
drop_empty_cols <- function(df) {
  if (!is.data.frame(df) || ncol(df) == 0L) {
    return(df)
  }
  keep <- !vapply(df, is_completely_empty_col, logical(1))
  df[, keep, drop = FALSE]
}

#' Resolve response column used as join key to participants$token.
#' @return character scalar column name, or NA_character_ if none
#' @noRd
response_token_col <- function(df) {
  cols_lower <- tolower(colnames(df))
  token_aliases <- tolower(limesurvey_token_response_cols())
  hit <- which(cols_lower %in% token_aliases)
  if (length(hit) == 0L) {
    return(NA_character_)
  }
  colnames(df)[[hit[[1]]]]
}

#' Left-join participant data onto responses by token.
#' @noRd
merge_participant_data <- function(df, iSurveyID) {
  if (!is.data.frame(df) || nrow(df) == 0L) {
    return(df)
  }

  token_col <- response_token_col(df)
  if (is.na(token_col)) {
    warning(
      "add_participant_data=TRUE, but no token/Zugangscode column in ",
      "responses; skipping participant merge.",
      call. = FALSE
    )
    return(df)
  }

  participants <- get_participants(
    iSurveyID = iSurveyID,
    iLimit = 100000L,
    aAttributes = TRUE,
    bUnused = FALSE
  )

  if (!is.data.frame(participants) || nrow(participants) == 0L) {
    warning(
      "add_participant_data=TRUE, but get_participants() returned no rows; ",
      "skipping participant merge.",
      call. = FALSE
    )
    return(df)
  }

  if (!"token" %in% colnames(participants)) {
    warning(
      "add_participant_data=TRUE, but participants have no token column; ",
      "skipping participant merge.",
      call. = FALSE
    )
    return(df)
  }

  # Only judge emptiness among participants that match response tokens
  # (e.g. unused tokens with leftover names must not keep firstname).
  response_tokens <- unique(as.character(df[[token_col]]))
  response_tokens <- response_tokens[
    !is.na(response_tokens) & nzchar(trimws(response_tokens))
  ]
  participants <- participants[
    as.character(participants$token) %in% response_tokens, ,
    drop = FALSE
  ]
  if (nrow(participants) == 0L) {
    warning(
      "add_participant_data=TRUE, but no participants matched response ",
      "tokens; skipping participant merge.",
      call. = FALSE
    )
    return(df)
  }

  # Drop columns with no real values (e.g. empty firstname/lastname).
  participants <- drop_empty_cols(participants)

  # Ensure token survived empty-col filtering (should — usually non-empty).
  if (!"token" %in% colnames(participants)) {
    warning(
      "add_participant_data=TRUE, but token column was empty after filtering; ",
      "skipping participant merge.",
      call. = FALSE
    )
    return(df)
  }

  response_cols <- colnames(df)
  by_spec <- stats::setNames("token", token_col)
  out <- dplyr::left_join(df, participants, by = by_spec)

  # Drop participant columns that are still empty after the join.
  added_cols <- setdiff(colnames(out), response_cols)
  if (length(added_cols) > 0L) {
    empty_added <- added_cols[
      vapply(out[added_cols], is_completely_empty_col, logical(1))
    ]
    if (length(empty_added) > 0L) {
      out <- out[, !colnames(out) %in% empty_added, drop = FALSE]
    }
  }
  out
}
