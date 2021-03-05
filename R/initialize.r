#' Turn on the log
#'
#' \code{initialize} turns on logging for this package. This function
#' is designed for internal use and should not be called externally.
#'
#' @return Void

initialize <- function() {
  # Initialize a log file when a path is provided
  futile.logger::flog.appender(futile.logger::appender.tee("verto2covax.log"))
  futile.logger::flog.info("Starting Verto2Covax as %s", Sys.getenv("USERNAME"))
}
