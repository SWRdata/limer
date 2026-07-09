
#' add_participants
#'
#' Adds participants to a survey
#'
#' @param iSurveyID integer, ID of the Survey to insert responses
#' @param data dataframe with the columns firstname, lastname and email
#' @param bCreateToken boolean Should tokens be created
#' @param chunksize integer, size of chunks for handling php memory problems
#'
#' @return API Response
#' @example add_participants(475835, data = data.frame(firstname = c("Max",
#'                                                                   "Moritz"),
#'                           lastname = c("Mustermann", "Müller"),
#'                           email = c("m@aol.de", "m@gmx.de")),
#'                           bCreateToken = TRUE)
#' @export
#'
#' @references https://api.limesurvey.org/classes/remotecontrol_handle.html#method_add_participants

add_participants <- function(iSurveyID, data, bCreateToken = F, chunksize = 200){

  default_fields <- c("email", "firstname", "lastname")
  fields <- colnames(data)

  # how many custom attributes are there?
  n_fields <- sum(!fields %in% default_fields)

  # Test if the usual default attributes are set
  if (!any(default_fields %in% fields)) {

    answer <- readline(prompt = "None of the usual attributes `firstname`, `lastname` or `email` are present in data. Continue anyway (y)?")
    if (tolower(answer) != "y")
      return("No participants were added")
  }

  if (!exists_participants_table(iSurveyID)) {

      create_participants_table(iSurveyID, aAttributeFields = ifelse(n_fields > 0, n_fields, NULL) )

      warning("No participant table found and a new one created", call. = F)
  }

  # if data is a character vector
  if (inherits(data, "character")) {
    data <- data.frame(firstname = "", lastname = "", email = "" , token = data)
  }

  # For some column types, such as mail addresses, no leading or trailing spaces
  # may be included.
  data <- data %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.character), stringr::str_trim))


  data <- stats::setNames(split(data, seq(nrow(data))), rownames(data))
  data <- lapply(data, FUN = function(x) unlist(x) %>% as.list() )

    # for php memory problems split iLimit in chunks
  limit <- 1
  if (length(data) > chunksize) {
    n <- ceiling(length(data)/chunksize)
  } else{
    n <-  1
  }

  for (i in 1:n) {

    list_data <- data[limit:(limit + chunksize)]
    # if the number of list items is smaller than chunksize and this would
    # result in empty entries, delete empty list items
    list_data <- Filter(function(x) length(x) > 0, list_data)

    params <- list("iSurveyID" = iSurveyID, "aParticipantData" = list_data,
                   "bCreateToken" = bCreateToken)

    resp <- call_limer(method = "add_participants", params = params)
    resp <- data.table::rbindlist(resp, fill = T) %>% suppressWarnings()
    if ("errors" %in% colnames(resp)) {
      # Which elements contain an error
      err_elements <- which(sapply(resp$errors, function(x) !is.null(x)))
      e <- paste("Element",err_elements, "contains error:",resp$errors %>% unlist(), collapse = "\n")
      warning("there were errors when adding participants among others.\n ", e, call. = F)
    }

    # set count
    limit <- limit + chunksize + 1
    cat("\r",round((i/n)*100, digits = 2), "%")
    utils::flush.console()
  }

  if (n_fields > 0) {
    # names of additional attributes
    descriptions <- fields[!fields %in% default_fields]
    descriptions <- sanitize_string(descriptions)

    params <-
      list("iSurveyID" = iSurveyID, "aAttributeFields" = descriptions)

    resp <- call_limer("update_token_description", params = params)

  }

  cat("\r")
  utils::flush.console()

}


# TODO
# Gleiche die Spalten der bisherigen Tabelle mit den neu hinzuzufügenden ab
# und gebe eine Warnung aus, wenn sie nicht übereinstimmen.
# Fehlende Spalten werden wie importiert?
# Die Felder müssen in der UI vorher angelegt sein, um korrekt importiert
# werden zu können.
