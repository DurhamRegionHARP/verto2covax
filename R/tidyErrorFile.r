#' Tidy the CoVax error file after a Mass Dataload
#'
#' \code{tidyErrorFile} iterates through the failed record returned after
#' a CoVax Mass Dataload and cleans the "Error" column. A CSV is written to
#' the path specified with \code{outputFile}. \code{tidyErrorFile} takes a
#' conservative approach to cleaning, given that the "Error" column contains free-text.
#'
#' @param file Character scalar. Path to the CSV error file returned
#'   by CoVax.
#' @param outputFile Character scalar. Path the function should use to write the tidy CSV file.
#' @return \code{tidyErrorFile} returns the input \code{file} invisibly.
#' @importFrom rlang .data
#' @export

tidyErrorFile <- function(file, outputFile) {
  futile.logger::flog.info("Processing file: %s", file)
  # Bring in the data
  errorDF <- readr::read_csv(file, col_types = readr::cols(.default = readr::col_character()))
  # Filter out duplicate clients
  # Remove duplicated health card numbers
  # Set "Error" to NA where applicable
  tidyErrorDF <- errorDF %>%
    dplyr::filter(.data$Error != "Duplicate Clients found.") %>%
    dplyr::mutate(
      CCM_PatientId__c = dplyr::if_else(
        stringr::str_starts(.data$Error, "duplicate value found: CCM_PatientId__c duplicates value on record with id:"),
        NA_character_,
        .data$CCM_PatientId__c
      ),
      Error = dplyr::if_else(
        stringr::str_starts(.data$Error, "duplicate value found: CCM_PatientId__c duplicates value on record with id:"),
        NA_character_,
        .data$Error
      )
    )
  # Write the data to outputFile
  tidyErrorDF %>% readr::write_csv(
    outputFile,
    na = "",
    eol = "\r\n"
  )
  futile.logger::flog.info("Created %s", outputFile)
  return(invisible(file))
}
