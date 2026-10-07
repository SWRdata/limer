#' Known LimeSurvey token table columns (non-attribute_*).
#' @noRd
known_token_fields <- function() {
  c(
    "tid", "participant_id", "firstname", "lastname", "email",
    "emailstatus", "token", "language", "blacklisted", "sent",
    "remindersent", "remindercount", "completed", "usesleft",
    "validfrom", "validuntil", "mpid"
  )
}

#' Extra participant columns that map to attribute_N fields.
#' @noRd
extra_attribute_columns <- function(fields) {
  setdiff(fields, known_token_fields())
}

#' Build attributedescriptions list for set_survey_properties.
#' @noRd
build_attribute_descriptions <- function(attr_names) {
  descriptions <- lapply(attr_names, function(name) {
    list(
      description = as.character(name),
      mandatory = "N",
      encrypted = "N",
      show_register = "N"
    )
  })
  names(descriptions) <- paste0("attribute_", seq_along(attr_names))
  descriptions
}

#' Rename extra columns to attribute_1..N for the LimeSurvey API.
#' @noRd
map_columns_to_attributes <- function(data, attr_names) {
  if (length(attr_names) == 0L) {
    return(data)
  }
  new_names <- colnames(data)
  for (i in seq_along(attr_names)) {
    idx <- which(new_names == attr_names[[i]])
    if (length(idx) == 1L) {
      new_names[[idx]] <- paste0("attribute_", i)
    }
  }
  colnames(data) <- new_names
  data
}

#' Rename attribute_* columns back to attributedescriptions labels.
#'
#' Uses each attribute's \code{description} (set by add_participants) as
#' the column name. Duplicate descriptions are made unique with
#' \code{make.unique()}. Columns without a description stay as
#' \code{attribute_N}.
#' @noRd
rename_attributes_to_descriptions <- function(data, descriptions) {
  if (is.null(descriptions) || length(descriptions) == 0L) {
    return(data)
  }
  new_names <- colnames(data)
  for (i in seq_along(new_names)) {
    nm <- new_names[[i]]
    if (!grepl("^attribute_[0-9]+$", nm)) {
      next
    }
    item <- descriptions[[nm]]
    d <- if (is.list(item)) item$description else item
    if (!is.null(d) && !is.na(d) && nzchar(as.character(d))) {
      new_names[[i]] <- as.character(d)
    }
  }
  colnames(data) <- make.unique(new_names, sep = "_")
  data
}

#' Parse attributedescriptions from get_survey_properties.
#' @noRd
parse_attribute_descriptions <- function(raw) {
  if (is.null(raw) || (length(raw) == 1L && is.na(raw))) {
    return(list())
  }
  if (is.list(raw) && !is.character(raw)) {
    return(raw)
  }
  if (is.character(raw) && length(raw) == 1L && nzchar(raw)) {
    parsed <- tryCatch(
      jsonlite::fromJSON(raw, simplifyVector = FALSE),
      error = function(e) NULL
    )
    if (!is.null(parsed)) {
      return(parsed)
    }
    return(list())
  }
  list()
}

#' Map extra column names to existing attribute_* fields by description.
#' @noRd
match_attributes_by_description <- function(attr_names, descriptions) {
  if (length(attr_names) == 0L) {
    return(character(0))
  }
  desc_map <- character(0)
  for (nm in names(descriptions)) {
    item <- descriptions[[nm]]
    d <- if (is.list(item)) item$description else item
    if (!is.null(d) && !is.na(d) && nzchar(as.character(d))) {
      desc_map[[as.character(d)]] <- nm
    }
  }
  mapping <- character(length(attr_names))
  names(mapping) <- attr_names
  for (nm in attr_names) {
    if (nm %in% names(desc_map)) {
      mapping[[nm]] <- desc_map[[nm]]
    } else if (grepl("^attribute_[0-9]+$", nm)) {
      mapping[[nm]] <- nm
    } else {
      mapping[[nm]] <- NA_character_
    }
  }
  mapping
}

#' Set survey attributedescriptions via RemoteControl.
#' @noRd
set_attribute_descriptions <- function(iSurveyID, descriptions) {
  aSurveyData <- list(
    attributedescriptions = jsonlite::toJSON(
      descriptions,
      auto_unbox = TRUE,
      null = "null"
    )
  )
  call_limer(
    method = "set_survey_properties",
    params = list(
      "iSurveyID" = iSurveyID,
      "aSurveyData" = aSurveyData
    )
  )
}

#' Resolve aAttributes for list_participants API.
#'
#' LimeSurvey PHP uses array_intersect on the 5th argument; a boolean TRUE
#' causes a fatal error (HTTP 500). TRUE is expanded to attribute_* names
#' from the survey's attributedescriptions.
#' @noRd
resolve_list_participants_a_attributes <- function(iSurveyID, aAttributes) {
  if (identical(aAttributes, FALSE)) {
    return(FALSE)
  }
  if (isTRUE(aAttributes)) {
    descriptions <- tryCatch(
      get_attribute_descriptions(iSurveyID),
      error = function(e) list()
    )
    attr_names <- names(descriptions)
    attr_names <- attr_names[grepl("^attribute_[0-9]+$", attr_names)]
    if (length(attr_names) == 0L) {
      warning(
        "aAttributes=TRUE, aber keine Token-Attribute in ",
        "attributedescriptions gefunden; nur Basisfelder.",
        call. = FALSE
      )
      return(FALSE)
    }
    return(as.list(attr_names))
  }
  if (is.character(aAttributes)) {
    return(as.list(aAttributes))
  }
  aAttributes
}

#' Read attributedescriptions for a survey.
#' @noRd
get_attribute_descriptions <- function(iSurveyID) {
  props <- call_limer(
    method = "get_survey_properties",
    params = list(
      "iSurveyID" = iSurveyID,
      "aSurveySettings" = list("attributedescriptions")
    )
  )
  raw <- if (is.list(props)) props$attributedescriptions else props
  parse_attribute_descriptions(raw)
}
