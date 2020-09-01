#' @title emotional_impact_response_experienced
#'
#' @description Builds the "Type of emotional response experienced" part of one subtable of an "Emotional impact of
#' incidents" table for a Nature of Crime dataset.
#'
#' @details This function builds the "Type of emotional response experienced" part of one subtable (specified by
#' \code{subtable}) based predominantly on two configurations file specified by \code{config} and \code{master_config}. See
#' the User Guide for more details about these two config files.
#'
#' @param config A list of lists (and other items) which is the result of applying \code{yaml::read.yaml()} to an appropriate
#' "Nature of Crime" .yaml file, and then \code{autotable::process_config()}.
#'
#' @param master_config A list of lists (and other items) which is the result of applying \code{yaml::read.yaml()} to the master
#' configuration file.
#'
#' @param subtable An integer indicating which subtable to consider; if we denote the value of \code{subtable} by \code{n},
#' then the subtable considered will be that corresponding to the \code{n}th item in \code{config$ALL_PARTS$OFFENCE_CODES}.
#'
#' @return Returns a dataframe with the requisite percentages and unweighted bases for the "Type of emotional response
#' experienced" part of the specified subtable.
#'
#' @examples config = autotable::process_config(yaml::read.yaml("configs/emotional_impact/BURGLARY.yaml"))
#' master_config = yaml::read_yaml(file = "configs/MASTER.yaml")
#' emotional_impact_response_experienced(config = config, master_config = master_config, subtable = 2)
#'
#' @author Eleanor Scott-Allen, Katie Edser, Matthew Morrison, Nicholas O'Neil; ONS Centre for Crime and Justice, Titchfield, PO15 5RR.
#'
#' @export

emotional_impact_response_experienced = function(config, master_config, subtable) {

# 1. Find out which years we are building tables for:
  years_of_data = lapply(config$ALL_PARTS$WEIGHT_VARIABLES, `[[`, 1)

# 2. Get a list of the VF datasets to process:
  list_of_VF_datasets <- base::list.files(path = master_config$VF_DATA_DIRECTORY)

# Below, we will use a for-loop to process the years of data one-by-one.

for (i in c(1:length(years_of_data))) {

  # 3. Read in the VF dataset for this year:
    futile.logger::flog.trace("emotional_impact_response_experienced - Reading in source data from %s",
                              list_of_VF_datasets[i])
    VF_dataset <- base::as.data.frame(data.table::fread(base::paste(master_config$VF_DATA_DIRECTORY, list_of_VF_datasets[i], sep = '/')))

  # 4. Fetch the correct weight variable for this year of data:
    weight_variable = config$ALL_PARTS$WEIGHT_VARIABLES[[i]][[2]]
    futile.logger::flog.trace("emotional_impact_response_experienced - Using %s as the weight variable", weight_variable)

  # 5. Remove zero-weighted cases from the dataset:
    RE_1 = autotable::remove_zeroweighted_rows(df = VF_dataset, weightvar = weight_variable)
    # (Regarding the choice of variable name here: This just stands for "Type of Emotional Response Experienced", which
    #  describes the subtable component that we want this function to build. Each step in the code will build on the previous
    #  one from now onwards; RE_1 will become RE_2, and then RE_2 will become RE_3, and so on, until we have the
    #  dataframe in the form that we want)
    futile.logger::flog.trace("emotional_impact_response_experienced - Removed %s cases out of %s because their weights were 0",
                              nrow(VF_dataset) - nrow(RE_1), nrow(VF_dataset))

  # 6. Keep only cases where victarea = 1 (i.e. the incident occurred within 15 minutes of the interview location - typically
  #    the respondent's home) or where wherhapp = 1 (i.e. the incident occurred in England & Wales):
    RE_2 = dplyr::filter(RE_1, victarea == 1 | wherhapp == 1)
    futile.logger::flog.trace("emotional_impact_response_experienced - Selected %s cases where victarea = 1 or wherapp = 1",
                              nrow(RE_2))

  # 7. Compute a variable which indicates whether each case is a relevant incident or not, based off its associated offence
  #    code; a value of 1 will indicate that the incident is indeed relevant to the subtable we are building:
    offence_codes = config$ALL_PARTS$OFFENCE_CODES[[subtable]][[2]]
    RE_3 = dplyr::mutate(RE_2, relevant_incident = dplyr::if_else(offence %in% offence_codes, 1, NULL))
    futile.logger::flog.trace("emotional_impact_response_experienced - Computed a new variable called relevant_incident; a value of 1 for this variable will indicate that the case in question has an offence code relevant to the subtable being built")

  # 8. Select only the cases where relevant_incident = 1:
    RE_4 = dplyr::filter(RE_3, relevant_incident == 1)
    futile.logger::flog.trace("emotional_impact_response_experienced - Selected %s cases where relevant_incident = 1",
                              nrow(RE_4))

  # 9. Select only the cases where vftype = 1 (i.e. the incident was recorded on the long Victim Form):
    RE_5 = dplyr::filter(RE_4, vftype == 1)
    futile.logger::flog.trace("emotional_impact_response_experienced - Selected %s cases where vftype = 1", nrow(RE_5))

  # 10. Filter out those who responded "Don't know" to "What emotional reactions did you have?" (i.e. whemotk = 1) and those
  #     who refused to answer (whemotl = 1):
    RE_6 = dplyr::filter(RE_5, whemotk != 1 & whemotl != 1)
    futile.logger::flog.trace("emotional_impact_response_experienced - Selected %s cases where whemotk =/= 1 and whemotl =/= 1",
                              nrow(RE_6))

  # 11. If the headline crime type is violence, we may have to apply some extra filters, depending on which subtable is being
  #     built. We'll find out if the headline crime type is violence by searching for "violence" in the Table's title:
    if (grepl("violence", config$ALL_PARTS$TITLE_OF_TABLE, ignore.case = TRUE)) {
      number_of_cases_pre_filter = nrow(RE_6)
      RE_6 = autotable::apply_additional_violence_filters(dataframe = RE_6,
                                                             subtable_name = config$ALL_PARTS$OFFENCE_CODES[[subtable]][[1]])
      futile.logger::flog.trace("emotional_impact_response_experienced - Applied additional violence filters, removing a further %s cases",
                                number_of_cases_pre_filter - nrow(RE_6))
    }

  # 12. Extract the unweighted base:
    unweighted_base = base::sum(RE_6$relevant_incident)
    futile.logger::flog.trace("emotional_impact_response_experienced - Determined the unweighted base as %s",
                              unweighted_base)

  # 13. Weight the data:
    variables_for_rows = unlist(lapply(config$PART_THREE$ROW_LABELS, `[[`, 1))
    RE_7 = autotable::apply_weight(RE_6, base::c("relevant_incident", variables_for_rows), weight = weight_variable)
    futile.logger::flog.trace("emotional_impact_response_experienced - Weighted the data by %s", weight_variable)

  # 14. Generate the unrounded percentages for this year:
    weighted_totals = dplyr::summarise_at(RE_7, variables_for_rows, sum, na.rm = TRUE)
    RE_8            = (weighted_totals / base::sum(RE_7$relevant_incident)) * 100
    futile.logger::flog.trace("emotional_impact_response_experienced - Generated the unrounded percentages")

  # 15. Generate a name for this column, which we will later apply in Step 21:
    start_year              = substr(years_of_data[[i]], 3, 4)
    end_year                = as.numeric(start_year) + 1
    names(years_of_data)[i] = paste0("Apr '", start_year, " to Mar '", end_year)
    futile.logger::flog.trace("emotional_impact_response_experienced - Finished column \"%s\"", names(years_of_data)[i])

  # 16. We now want to add this column into our final dataframe:

    if (i == 1) { # If this is out first iteration through the loop, then we don't have a dataframe to add to, however! So in
                  # that case, we must create one:

      RE_9 = base::as.data.frame(base::t(RE_8))
      row_of_unweighted_bases = unweighted_base

    } else { # But if it's not our first iteration, then we will just add the column to the dataframe that already exists:

      RE_9 = base::cbind(RE_9, base::as.data.frame(base::t(RE_8)))
      row_of_unweighted_bases = base::c(row_of_unweighted_bases, unweighted_base)

    }

}

# 17. Bind the row of unweighted bases to the bottom of the table:
  RE_10 = base::rbind(RE_9, row_of_unweighted_bases)
  futile.logger::flog.trace("emotional_impact_response_experienced - Added the row of unweighted bases")

# 18. Name the rows:
  rownames(RE_10) = lapply(config$PART_THREE$ROW_LABELS, `[[`, 2)
  futile.logger::flog.trace("emotional_impact_response_experienced - Added row labels")

# 19. We now wish to order the rows by the magnitude of their percentages in the latest year of data. However, regardless of
#     magnitudes, we always want the bottom two rows to be the "Other emotional response" and then the unweighted bases:
  index_of_Other_row = grep("othemot", rownames(RE_9))
  Other_row          = RE_10[index_of_Other_row, ]
  bases              = RE_10[nrow(RE_10), ]

  RE_10_without_Other_row_and_bases = RE_10[-c(index_of_Other_row, nrow(RE_10)),]
  RE_11_with_Other_row_and_bases    = RE_10_without_Other_row_and_bases[base::order(-RE_10_without_Other_row_and_bases[, base::ncol(RE_10_without_Other_row_and_bases)]), ]
  RE_11                             = rbind(RE_11_with_Other_row_and_bases, Other_row, bases)

  futile.logger::flog.trace("emotional_impact_response_experienced - Ordered rows by the magnitude of their percentages in the latest year of data")

# 20. Add significance columns, comparing (A) the latest year against the previous year; and (B) the latest year against
#     the earliest year, for each row:
  RE_12 = append_significance_column(dataframe = RE_11, column_1 = ncol(RE_11), column_2 = 1,
                                        row_of_unweighted_bases = nrow(RE_11))
  futile.logger::flog.trace("emotional_impact_response_experienced - Appended a significance column, comparing the latest year of data with the previous year")
  RE_13 = append_significance_column(dataframe = RE_12, column_1 = ncol(RE_12) - 1, column_2 = ncol(RE_12) - 2,
                                        row_of_unweighted_bases = nrow(RE_12))
  futile.logger::flog.trace("emotional_impact_response_experienced - Appended a significance column, comparing the latest year of data with the earliest year")

# 21. Name the columns:

  # For the sig-testing columns, we'll need to attach the correct footnote number:
  name_of_10_year_test_column = paste0(master_config$SIGNIFICANCE_TESTING$COLUMN_LABELS$`10_YEAR_TEST`, "$$Note_",
                                       grep("master_config\\$SIGNIFICANCE_TESTING\\$FOOTNOTE", config$ALL_PARTS$FOOTNOTES), "$$")
  name_of_1_year_test_column  = paste0(master_config$SIGNIFICANCE_TESTING$COLUMN_LABELS$`1_YEAR_TEST`, "$$Note_",
                                       grep("master_config\\$SIGNIFICANCE_TESTING\\$FOOTNOTE", config$ALL_PARTS$FOOTNOTES), "$$")

  colnames(RE_13) = c(names(years_of_data),
                      name_of_10_year_test_column,
                      name_of_1_year_test_column)

# 22. Return the final dataframe:
  return(RE_13)

}
