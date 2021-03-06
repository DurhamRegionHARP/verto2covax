#' Look up the Vaccination Event Id using the Vaccination Event number
#'
#' \code{getVaccinationEventId} looks for a Vaccination Event Object
#' Id given a Vaccination Event Number. This look up is performed
#' using the digit portion of the Vaccination Event Number without
#' the "VE" prefix.
#'
#' @param VENumber Character scalar. Digit portion of the
#'   Vaccination Event Number used to look up the corresponding
#'   Vaccination Event object Id.
#' @return If the look up is successful, the Vaccination Event object
#'   Id as a character scalar. NA otherwise.

getVaccinationEventId <- function(VENumber) {
  dplyr::case_when(
    VENumber == "000004" ~ "1314t000000004qAAA",
    VENumber == "000027" ~ "1314t000000005DAAQ",
    VENumber == "000048" ~ "1314t000000005YAAQ",
    VENumber == "000055" ~ "1314t000000005fAAA",
    VENumber == "000057" ~ "1314t000000005hAAA",
    VENumber == "000059" ~ "1314t000000005jAAA",
    VENumber == "000089" ~ "1314t000000006DAAQ",
    VENumber == "000171" ~ "1314t000000007XAAQ",
    VENumber == "000191" ~ "1314t000000007rAAA",
    VENumber == "000321" ~ "1314t000000009xAAA",
    VENumber == "000346" ~ "1314t00000000AMAAY",
    VENumber == "000481" ~ "1314t00000000CXAAY",
    VENumber == "000512" ~ "1314t00000000D2AAI",
    VENumber == "000592" ~ "1314t00000000EKAAY",
    VENumber == "000617" ~ "1314t00000000EjAAI",
    VENumber == "000707" ~ "1314t00000000GBAAY",
    VENumber == "000728" ~ "1314t00000000GWAAY",
    VENumber == "000748" ~ "1314t00000000GqAAI",
    VENumber == "000779" ~ "1314t00000000HLAAY",
    VENumber == "000876" ~ "1314t00000000IuAAI",
    VENumber == "000969" ~ "1314t00000000KPAAY",
    VENumber == "001029" ~ "1314t00000000LNAAY",
    VENumber == "001059" ~ "1314t00000000LrAAI",
    VENumber == "001113" ~ "1314t00000000MjAAI",
    VENumber == "001245" ~ "1314t00000000OrAAI",
    VENumber == "001246" ~ "1314t00000000OsAAI",
    VENumber == "001259" ~ "1314t00000000P5AAI",
    VENumber == "001704" ~ "1314t00000000adAAA",
    VENumber == "001706" ~ "1314t00000000anAAA",
    VENumber == "001710" ~ "1314t00000000b7AAA",
    VENumber == "001712" ~ "1314t00000000bHAAQ",
    VENumber == "001733" ~ "1314t00000000cyAAA",
    VENumber == "001740" ~ "1314t00000000dXAAQ",
    VENumber == "001834" ~ "1314t00000000knAAA",
    VENumber == "001838" ~ "1314t00000000koAAA",
    VENumber == "001835" ~ "1314t00000000ksAAA",
    VENumber == "001836" ~ "1314t00000000kxAAA",
    VENumber == "001839" ~ "1314t00000000l7AAA",
    VENumber == "001840" ~ "1314t00000000lCAAQ",
    VENumber == "001841" ~ "1314t00000000lHAAQ",
    VENumber == "001842" ~ "1314t00000000lMAAQ",
    VENumber == "001843" ~ "1314t00000000lRAAQ",
    VENumber == "001845" ~ "1314t00000000lbAAA",
    VENumber == "001847" ~ "1314t00000000llAAA",
    VENumber == "001848" ~ "1314t00000000lqAAA",
    VENumber == "001849" ~ "1314t00000000lvAAA",
    VENumber == "001872" ~ "1314t00000000nhAAA",
    VENumber == "001873" ~ "1314t00000000nmAAA",
    VENumber == "001918" ~ "1314t00000000rPAAQ",
    VENumber == "001987" ~ "1314t00000000wyAAA",
    VENumber == "002079" ~ "1314t000000015CAAQ",
    VENumber == "002149" ~ "1314t00000001AlAAI"
  )
}
