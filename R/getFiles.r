#' Get the files the user would like to convert
#'

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
