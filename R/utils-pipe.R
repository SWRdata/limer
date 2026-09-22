#' Pipe operator
#'
#' `limer` uses the magrittr pipe throughout its functions (e.g. chaining
#' `dplyr::filter()`/`dplyr::pull()` calls on survey data). This operator
#' is re-exported so it's available to users of the package without
#' requiring a separate `library(magrittr)` call.
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL