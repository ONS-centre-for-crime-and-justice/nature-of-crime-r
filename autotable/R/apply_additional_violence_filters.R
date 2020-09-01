#'@title apply_additional_violence_filters
#'
#'@description Applies additional filters to a VF-based dataframe, as appopriate, in the production of one subtable of a
#'"Nature of Crime: Violence" Table.
#'
#'@details This function is motivated by the following: As at May 2020, 7 of the 8 headline crime types require that the
#'same number and types of filters be applied to the VF datasets, in the production of the corresponding Nature of Crime
#'tables. Violence, however, requires additional filters, since its data or subtables are further split by (A) whether
#'the offence caused injury or not; (B) type of injury; and (C) the relationship of the offender to the victim. This
#'function applies the appropriate filter for the subtable, based off \code{subtable_name}.
#'
#'@param dataframe A dataframe based on the data (or a subset of the data) of a VF dataset.
#'
#'@param subtable_name A string, identifying the subtable of interest by name. Please inspect the function body to see
#'which sub-strings trigger which filter.
#'
#'@return The original \code{dataframe}, except where the additional filters have been applied.
#'
#'@note At the time of writing, the 09/10 and 10/11 VF datasets are missing some requisite derived variables, which is
#'an issue that's currently being addressed outside of R - therefore, there is no use writing code that derives these
#'variables INSIDE R in the interim, as this will only prove to be wasted time long-term. Instead, as a short-term fix,
#'the script that calls this function should check which year's VF \code{dataframe} is based on, and skip the function
#'if that year is 09/10 or 10/11.
#'
#'@examples
#'
#'@export
#'

apply_additional_violence_filters = function(dataframe, subtable_name) {

  if (grepl("all violence", subtable_name, ignore.case = TRUE)) {    # If the subtable is for "All violence", no additional
                                                                     # filters need be applied.

  } else if (grepl("domestic", subtable_name, ignore.case = TRUE)) {     # Else if the subtable is for "Domestic violence"...

    dataframe = dplyr::filter(dataframe, violgrpnr == 1)

  } else if (grepl("stranger", subtable_name, ignore.case = TRUE)) {     # Else if the subtable is for "Violence by a stranger"...

    dataframe = dplyr::filter(dataframe, violgrpnr == 2)

  } else if (grepl("acquaintance", subtable_name, ignore.case = TRUE)) { # Else if the subtable is for "Violence by an acquaintance"...

    dataframe = dplyr::filter(dataframe, violgrpnr == 3)

  } else if (grepl("with injury", subtable_name, ignore.case = TRUE)) {  # Else if the subtable is for "Violence with injury"...

    dataframe = dplyr::filter(dataframe, violentnr1 == 1)

  } else if (grepl("without", subtable_name, ignore.case = TRUE)) {      # Else if the subtable is for "Violence without injury"...

    dataframe = dplyr::filter(dataframe, violentnr1 == 2)

  } else {                                                           # Else...

    dataframe = dplyr::filter(dataframe, violentnr1 == 1)

    if (grepl("wound", subtable_name, ignore.case = TRUE)) {             # If the subtable is for "Wounding"...

       dataframe = dplyr::filter(dataframe, violentnr2 == 1)

    } else if (grepl("assault", subtable_name, ignore.case = TRUE)) {  # Else if the subtable is for "Assault with minor injury"

       dataframe = dplyr::filter(dataframe, violentnr2 == 2)

    }

  }

  return(dataframe)

}