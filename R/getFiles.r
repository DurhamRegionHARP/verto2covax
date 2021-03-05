#' List the files to convert
#'
#' \code{getFiles} looks for Excel files within a
#' directory and returns file names as a list.
#' For this to work, Excel files must be saved with the
#' \code{.xlsx} extension.
#'
#' @param excelDir Character scalar. Names the directory used
#'   to search for .xlsx files. Defaults to the current working directory.
#' @return A \code{list} containing file names. Otherwise, an
#'   error if the directory does not have read access, or does not
#'   contain any \code{.xlsx} files.

getFiles <- function(excelDir = ".") {
  # Test the path
  xlFiles <- tryCatch({
    fs::dir_ls(excelDir, glob = "*.xlsx")
  },
  error = function(err) {
    futile.logger::flog.error(err)
    stop("Process failed!\n Stopping execution.", call. = FALSE)
  })
  # Make sure we have files
  if (length(xlFiles) == 0) {
    futile.logger::flog.error("No files in process in %s", excelDir)
    stop("Process failed!\n Stopping execution.", call. = FALSE)
  }
  return(xlFiles)
}
