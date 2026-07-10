#' @title limer: A LimeSurvey R Client
#'
#' @description Provides access to LimeSurvey's RemoteControl 2 API,
#' allowing you to collect and manage survey data in a simple, reproducible
#' workflow. In addition to wrapping the documented RemoteControl RPC
#' methods (surveys, questions, responses, participants), the package also
#' provides admin-web-UI-based helpers for functionality not exposed via
#' the API, such as survey copying, PDF/LSA/LSS export, and email template
#' attachments.
#'
#' @details Typical usage starts with setting connection options and
#' obtaining a session key:
#' \preformatted{
#' options(lime_api = "https://example.com/index.php/admin/remotecontrol")
#' options(lime_username = "user")
#' options(lime_password = "password")
#' get_session_key()
#' }
#' All functions in this package call \code{\link{call_limer}} internally,
#' which automatically manages the session key for every request - callers
#' never need to pass one directly.
#'
#' @author Andrew Heiss \email{andrew@andrewheiss.com}
#' @author Ulrich Lang \email{ulrich.lang@swr.de}
#' @author Dorina Kohler \email{dorina.kohler@swr.de}
#'
#' @seealso \code{\link{call_limer}} for the low-level API wrapper used by
#' all other functions in this package.
#' @references \url{https://api.limesurvey.org/classes/remotecontrol_handle.html}
#'
#' @keywords internal
"_PACKAGE"
