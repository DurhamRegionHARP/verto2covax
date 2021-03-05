#' Iterate over a list of files and convert each
#' file to match the CoVax schema
#'

convertMany <- function(path) {
  initialize()
  fileList <- getFiles(path)
  purrr::walk(fileList, function(xlFile) {
    convertOne(xlFile)
  })
}
