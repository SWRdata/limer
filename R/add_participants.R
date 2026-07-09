
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

add_participants <- function(iSurveyID, data, bCreateToken = F, chunksize = 200){

    if (!exists_participants_table(iSurveyID)) {
      warning("No participant table found and a new one created", call. = F)

      n <- length(colnames(data))
      field_names <- glue::glue("description_attribute_{1:n}")
      fields <- colnames(data)
      names(fields) = field_names
      create_participants_table(iSurveyID, aAttributeFields = fields)
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
    if ("errors" %in% colnames(resp))
      warning("there were errors when adding participants among others. ", resp$errors[[1]], call. = F)
    # set count
    limit <- limit + chunksize + 1
    cat("\r",round((i/n)*100, digits = 2), "%")
    utils::flush.console()
  }

  cat("\r")
  utils::flush.console()

}

