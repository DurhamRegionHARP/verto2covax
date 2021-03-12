#' Convert an Excel spreadsheet to match the CoVaxON schema
#'
#' \code{convertOne} reads an Excel file and converts the data
#' to match the schema required for upload into CoVaxON. The
#' converted file is saved as CSV and given the same name as
#' the input Excel file.
#'
#'
#' @param file Character scalar. Names the path to an Excel file to convert.
#' @return Void
#' @export
#' @importFrom rlang .data
#' @importFrom rlang :=

convertOne <- function(file) {
  futile.logger::flog.info("Processing file: %s", file)
  # Covax required columns names
  covaxColumnNames <- c(
    "LastName",
    "FirstName",
    "MiddleName",
    "PersonBirthdate",
    "Gender__c",
    "Consent_for_Data_Capture__c",
    "Email_Communication__c",
    "Phone_SMS_Communication__c",
    "Email_Notification_Covid_Info__c",
    "Sms_Notification_Covid_Info__c",
    "Reason_for_Immunization__c",
    "CCM_PatientId__c",
    "Alternative_ID__c",
    "Alternative_ID_Type__c",
    "CCM_Proxy_Name__c",
    "Proxy_Phone__c",
    "CCM_Proxy_Relationship__c",
    "PersonHomePhone",
    "PersonMobilePhone",
    "PersonEmail",
    "PersonMailingStreet",
    "PersonMailingCity",
    "PersonMailingState",
    "PersonMailingPostalCode",
    "Vaccination_Event__c"
  )
  columnNames <- c(
    "PatientID",
    "AppointmentID",
    "AppointmentDate",
    "AppointmentTime",
    "LastName",
    "FirstName",
    "MiddleName",
    covaxColumnNames[4],
    covaxColumnNames[5],
    covaxColumnNames[12],
    covaxColumnNames[18],
    covaxColumnNames[19],
    covaxColumnNames[20],
    covaxColumnNames[21],
    covaxColumnNames[22],
    covaxColumnNames[23],
    covaxColumnNames[24],
    "TargetPopulation",
    "Reason",
    "VaccineDose",
    "Status"
  )
  # Read the spreadsheet
  xlList <- tryCatch({
    readxl::read_excel(
      file,
      sheet = 'Vaccine Report',
      range = cellranger::cell_limits(c(2, 1), c(NA, 21)),
      col_names = columnNames,
      col_types = 'text',
      na = NA_character_
    )
  },
  warning = function(warning) {
    futile.logger::flog.warn(warning)
    warning("There was a problem reading the excel file!\n Please see the log.", call. = FALSE)
  },
  error = function(err) {
    futile.logger::flog.error(err)
    stop("Process failed!\n Stopping execution.", call. = FALSE)
  })
  # Create an empty data.frame using the CoVaxON structure
  for (index in 1:length(covaxColumnNames)) {
    if (!(covaxColumnNames[index] %in% colnames(xlList))) {
      newColumn <- covaxColumnNames[index]
      xlList <- tibble::add_column(xlList, !!newColumn := NA)
    }
  }
  # Apply the Covax schema
  cleanList <- xlList %>% dplyr::mutate(
    PersonHomePhone = dplyr::if_else(
      is.na(.data$PersonHomePhone),
      .data$PersonHomePhone,
      stringr::str_replace(.data$PersonHomePhone, '(\\d\\d\\d)(\\d\\d\\d)(\\d\\d\\d\\d)', '\\1-\\2-\\3')
    ),
    PersonMobilePhone = dplyr::if_else(
      is.na(.data$PersonMobilePhone),
      .data$PersonMobilePhone,
      stringr::str_replace(.data$PersonMobilePhone, '(\\d\\d\\d)(\\d\\d\\d)(\\d\\d\\d\\d)', '\\1-\\2-\\3')
    ),
    PersonMailingPostalCode = dplyr::if_else(
      is.na(.data$PersonMailingPostalCode),
      .data$PersonMailingPostalCode,
      stringr::str_replace(.data$PersonMailingPostalCode, '([a-zA-Z]\\d[a-zA-Z])\\W?(\\d[a-zA-Z]\\d)', '\\1 \\2'),
    ),
    PersonMailingStreet = dplyr::if_else(
      is.na(.data$PersonMailingStreet),
      .data$PersonMailingStreet,
      stringr::str_replace_all(.data$PersonMailingStreet, ',', ''),
    ),
    Vaccination_Event__c = paste0(
      "VE-",
      stringr::str_split(fs::path_ext_remove(fs::path_file(file)), "-", Inf, TRUE)[,3]
    ),
    Email_Communication__c = dplyr::if_else(
      is.na(.data$PersonEmail),
      NA_character_,
      "TRUE"
    )
  ) %>% dplyr::select(
    dplyr::all_of(covaxColumnNames)
  ) %>% dplyr::relocate(
    dplyr::all_of(covaxColumnNames)
  )
  # Remove spaces and the period from file names
  safeFileName <- stringr::str_replace_all(
    fs::path_ext_remove(fs::path_file(file)),
    '[.\\s]',
    ''
  )
  # Generate a filename with .csv extension
  output <- paste0(
    fs::path_dir(file),
    '/',
    safeFileName,
    '.csv'
  )
  # Save the result
  readr::write_csv(
    cleanList,
    output,
    na = "",
    eol = "\r\n"
  )
  futile.logger::flog.info("Created %s", output)
}
