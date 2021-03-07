#' Convert a list of files to match the CoVax schema
#'
#' \code{convertMany} iterates over a list of files and calls
#' \code{\link{convertOne}} on each file. See \code{\link{getFiles}} for
#' information on how the file list is created.
#'
#' @param path Character scalar. Names the directory containing
#'   the Excel files to convert.
#' @return Void
#' @export

convertMany <- function(path, regexp, logFile = "verto2covax.log") {
  initialize(logFile)
  fileList <- getFiles(path, regexp)
  purrr::walk(fileList, function(xlFile) {
    convertOne(xlFile)
  })
}
