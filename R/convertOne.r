#' @title Convert an Excel spreadsheet to match the CoVaxON schema
#'
#' @description \code{convertOne} reads an Excel file and converts the data
#'   to match the schema required for upload into CoVaxON. The
#'   converted file is saved as CSV and given the same name as
#'   the input Excel file.
#'
#' @name convertOne
#' @param file Character scalar. Names the path to an Excel file to convert.
#' @return Void
#' @export
#' @importFrom rlang .data
#' @importFrom rlang :=

utils::globalVariables("where")

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
    "Vaccination_Event__c",
    "Organization__c"
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
    "PriorityArea",
    "TargetPopulation",
    "Reason",
    "VaccineDose",
    "VaccineType",
    "Status",
    "AppointmentDateBooked",
    "AppointmentTimeBooked",
    "Site"
  )
  # Read the spreadsheet
  xlList <- tryCatch({
    readxl::read_excel(
      file,
      sheet = 'Vaccine Report',
      range = cellranger::cell_limits(c(2, 1), c(NA, 26)),
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
      stringr::str_detect(.data$PersonMailingPostalCode, '([a-zA-Z]\\d[a-zA-Z])\\W?(\\d[a-zA-Z]\\d)'),
      stringr::str_to_upper(
        stringr::str_replace(.data$PersonMailingPostalCode, '([a-zA-Z]\\d[a-zA-Z])\\W?(\\d[a-zA-Z]\\d)', '\\1 \\2')
      ),
      NA_character_
    ),
    Vaccination_Event__c = getVaccinationEvent(.data$Site),
    Email_Communication__c = dplyr::if_else(
      is.na(.data$PersonEmail),
      NA_character_,
      "TRUE"
    ),
    PersonBirthdate = dplyr::if_else(
      stringr::str_detect(.data$PersonBirthdate, '10[0-9][0-9]-[01][0-9]-[0123][0-9]'),
      stringr::str_replace(.data$PersonBirthdate, '10([0-9][0-9]-[01][0-9]-[0123][0-9])', '19\\1'),
      .data$PersonBirthdate,
      NA_character_
    ),
    CCM_PatientId__c = dplyr::if_else(
      nchar(.data$CCM_PatientId__c) == 10,
      .data$CCM_PatientId__c,
      NA_character_,
      NA_character_
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
  # BUG: The Verto report may contain unexpected characters. These will cause
  # CoVax to choke during Mass Dataload. This step removes them.
  cleanList %>% dplyr::mutate(
    dplyr::across(where(is.character), ~ stringr::str_replace_all(.x, '[",\n]', ''))
  ) %>% readr::write_csv(
    output,
    na = "",
    eol = "\r\n"
  )
  futile.logger::flog.info("Created %s", output)
}

# Helper function
# Replace the name of the clinic with the CoVax Vaccination Event string
getVaccinationEvent <- function(clinic) {
  vaccinationEvent <- dplyr::case_when(
    clinic == "Pickering Chestnut Hill Recreation Complex" ~ "VE-001710",
    clinic == "Clarington Garnet B Rickard Recreation Complex" ~ "VE-001841",
    clinic == "Ajax Audley Recreation Centre" ~ "VE-001835",
    clinic == "Scugog Arena" ~ "VE-001843",
    clinic == "Uxbridge Arena" ~ "VE-001842",
    clinic == "Cannington Rick MacLeish Memorial Community Centre Arena" ~ "VE-001845",
    clinic == "Whitby McKinney Centre" ~ "VE-001838",
    clinic == "L1X \u2013 Pickering \u2013 Pineridge High School" ~ "VE-007370",
    clinic == "L1Z \u2013 Ajax \u2013 St Teresa of Calcutta Catholic School" ~ "VE-007371",
    clinic == "L1V \u2013 Pickering \u2013 Dunbarton High School" ~ "VE-007372",
    clinic == "L1T \u2013 Ajax \u2013 McLean Community Centre" ~ "VE-007373",
    clinic == "Whitby \u2013 Sinclair Secondary School" ~ "VE-010822",
    TRUE ~ NA_character_
  )
  return(vaccinationEvent)
}
