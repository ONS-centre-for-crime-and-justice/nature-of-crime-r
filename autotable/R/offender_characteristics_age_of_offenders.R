#' @title offender_characteristics_age_of_offenders
#'
#' @description Builds the "Age of offenders" part of one subtable of an "Offender characteristics" table for a Nature
#' of Crime dataset.
#'
#' @details This function builds the "Age of offenders" part of one subtable (specified by \code{subtable}) based
#' predominantly on two configurations file specified by \code{config} and \code{master_config}. See the User Guide for more
#' details about these two config files.
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
#' @return Returns a dataframe with the requisite percentages and unweighted bases for the "Age of offenders" part of the
#' specified subtable.
#'
#' @examples config = autotable::process_config(yaml::read.yaml("configs/offender_characteristics/ROBBERY.yaml"))
#' master_config = yaml::read_yaml(file = "configs/MASTER.yaml")
#' offender_characteristics_age_of_offenders(config = config, master_config = master_config, subtable = 1)
#'
#' @author Eleanor Scott-Allen, Katie Edser, Matthew Morrison, Nicholas O'Neil; ONS Centre for Crime and Justice, Titchfield, PO15 5RR.
#'
#' @export

offender_characteristics_age_of_offenders = function(config, master_config, subtable) {

# 1. Find out which years we are building tables for:
  years_of_data = lapply(config$ALL_PARTS$WEIGHT_VARIABLES, `[[`, 1)

# 2. Get a list of the VF datasets to process:
  list_of_VF_datasets = base::list.files(path = master_config$VF_DATA_DIRECTORY)

# Below, we will use a for-loop to process the years of data one-by-one.

for (i in c(1:length(years_of_data))) {

  # 3. Read in the VF dataset for this year:
    futile.logger::flog.trace("offender_characteristics_age_of_offenders - Reading in source data from %s", list_of_VF_datasets[i])
    VF_dataset = base::as.data.frame(data.table::fread(base::paste(master_config$VF_DATA_DIRECTORY, list_of_VF_datasets[i], sep = '/')))

  # 4. Fetch the correct weight variable for this year of data:
    weight_variable = config$ALL_PARTS$WEIGHT_VARIABLES[[i]][[2]]
    futile.logger::flog.trace("offender_characteristics_age_of_offenders - Using %s as the weight variable", weight_variable)

  # 5. Remove zero-weighted cases from the dataset:
    AoO_1 = autotable::remove_zeroweighted_rows(df = VF_dataset, weightvar = weight_variable)
    # (Regarding the choice of variable name here: This just stands for "Percentage Emotionally Affected", which describes the
    #  subtable component that we want this function to build. Each step in the code will build on the previous one from now
    #  onwards; AoO_1 will become AoO_2, and then AoO_2 will become AoO_3, and so on, until we have the dataframe in the form
    #  that we want)
    futile.logger::flog.trace("offender_characteristics_age_of_offenders - Removed %s cases out of %s because their weights were 0",
                              nrow(VF_dataset) - nrow(AoO_1), nrow(VF_dataset))

  # 6. Keep only cases where victarea = 1 (i.e. the incident occurred within 15 minutes of the interview location - typically
  #    the respondent's home) or where wherhapp = 1 (i.e. the incident occurred in England & Wales):
    AoO_2 = dplyr::filter(AoO_1, victarea == 1 | wherhapp == 1)
    futile.logger::flog.trace("offender_characteristics_age_of_offenders - Selected %s cases where victarea = 1 or wherapp = 1",
                              nrow(AoO_2))

  # 7. Compute a variable which indicates whether each case is a relevant incident or not, based off its associated offence
  #    code; a value of 1 will indicate that the incident is indeed relevant to the subtable we are building:
    Offence_codes = config$ALL_PARTS$OFFENCE_CODES[[subtable]][[2]]
    AoO_3 = dplyr::mutate(AoO_2, relevant_incident = dplyr::if_else(offence %in% Offence_codes, 1, NULL))
    futile.logger::flog.trace("offender_characteristics_age_of_offenders - Computed a new variable called relevant_incident; a value of 1 for this variable will indicate that the case in question has an offence code relevant to the subtable being built")

  # 8. Select only the cases where relevant_incident = 1:
    AoO_4 = dplyr::filter(AoO_3, relevant_incident == 1)
    futile.logger::flog.trace("offender_characteristics_age_of_offenders - Selected %s cases where relevant_incident = 1",
                              nrow(AoO_4))

  # 9. Select only the cases where vftype = 1 (i.e. the incident was recorded on the long Victim Form):
    AoO_5 = dplyr::filter(AoO_4, vftype == 1)
    futile.logger::flog.trace("offender_characteristics_age_of_offenders - Selected %s cases where vftype = 1", nrow(AoO_5))

  # 10. Select only the cases for which seeany = 1 (i.e. victim can say something about the offender(s)) and filter out invalid cases (e.g. Don't know and refusals).
    AoO_6 = dplyr::filter(AoO_5, seeany == 1)
    AoO_6 = dplyr::mutate(AoO_6, valid_incident = ifelse(ageoff2 %in% c(8, 9) | ageoff2f == 1 | ageoff2g == 1, 1, 0))
    AoO_6$valid_incident[is.na(AoO_6$valid_incident)] = 0
    AoO_6 = dplyr::filter(AoO_6, valid_incident != 1 )

  # 11. If the headline crime type is violence, we may have to apply some extra filters, depending on which subtable is being
  #     built. We'll find out if the headline crime type is violence by searching for "violence" in the Table's title:
    if (grepl("violence", config$ALL_PARTS$TITLE_OF_TABLE, ignore.case = TRUE)) {
      number_of_cases_pre_filter = nrow(AoO_6)
      AoO_6 = autotable::apply_additional_violence_filters(dataframe = AoO_6,
                                                           subtable_name = config$ALL_PARTS$OFFENCE_CODES[[subtable]][[1]])
      futile.logger::flog.trace("offender_characteristics_age_of_offenders - Applied additional violence filters, removing a further %s cases",
                                number_of_cases_pre_filter - nrow(AoO_6))
    }

  # 12. [Note to self: You can delete this step later; I only did this so I wouldn't have to re-number comments and dataframes below, because I'm feeling lazy right now]
    AoO_7 = AoO_6

  # 13. Extract the unweighted base:
    unweighted_base = base::sum(AoO_7$relevant_incident)
    futile.logger::flog.trace("Determined the unweighted base as %s", unweighted_base)

  # 14. Weight the data:
    AoO_8 = autotable::apply_weight(AoO_7, c("relevant_incident", "ageof12", "ageof1", "ageof2", "ageof3", "ageof4", "ageof5"), weight = weight_variable)
    futile.logger::flog.trace("offender_characteristics_age_of_offenders - Weighted the data by %s", weight_variable)

  # 15. [Note to self: You can delete this step later; I only did this so I wouldn't have to re-number comments and dataframes below, because I'm feeling lazy right now]
    AoO_10 = AoO_8

  # 16. Generate the unrounded percentages for this year:
    weighted_totals = dplyr::summarise_at(AoO_10, c("ageof12", "ageof1", "ageof2", "ageof3", "ageof4", "ageof5"), sum, na.rm = TRUE)
    AoO_11          = (weighted_totals / base::sum(AoO_10$relevant_incident)) * 100

  # 17. Generate a name for this column, which we will later apply in Step 23:
    start_year              = substr(years_of_data[[i]], 3, 4)
    end_year                = as.numeric(start_year) + 1
    names(years_of_data)[i] = paste0("Apr '", start_year, " to Mar '", end_year)
    futile.logger::flog.trace("offender_characteristics_age_of_offenders - Finished column \"%s\"", names(years_of_data)[i])

  # 18. We now want to add this column into our final dataframe:

    if (i == 1) { # If this is out first iteration through the loop, then we don't have a dataframe to add to, however! So in
                  # that case, we must create one:

      AoO_12 = base::as.data.frame(t(AoO_11))
      row_of_unweighted_bases = unweighted_base

    } else { # But if it's not our first iteration, then we will just add the column to the dataframe that already exists:

      AoO_12 = base::cbind(AoO_12, base::as.data.frame(t(AoO_11)))
      row_of_unweighted_bases = base::c(row_of_unweighted_bases, unweighted_base)

    }

}

# 19. Bind the row of unweighted bases to the bottom of the table:
  AoO_13 = base::rbind(AoO_12, row_of_unweighted_bases)
  futile.logger::flog.trace("offender_characteristics_age_of_offenders - Added the row of unweighted bases")

# 20. Add significance columns, comparing (A) the latest year against the previous year; and (B) the latest year against
#     the earliest year, for each row:
  AoO_14 = append_significance_column(dataframe = AoO_13, column_1 = ncol(AoO_13), column_2 = 1,
                                      row_of_unweighted_bases = nrow(AoO_13))
  futile.logger::flog.trace("offender_characteristics_age_of_offenders - Appended a significance column, comparing the latest year of data with the previous year")
  AoO_15 = append_significance_column(dataframe = AoO_14, column_1 = ncol(AoO_14) - 1, ncol(AoO_14) - 2,
                                      row_of_unweighted_bases = nrow(AoO_14))
  futile.logger::flog.trace("offender_characteristics_age_of_offenders - Appended a significance column, comparing the latest year of data with the earliest year")

# 21. Name the rows:
  rownames(AoO_15) = config$PART_FOUR$ROW_LABELS
  futile.logger::flog.trace("offender_characteristics_age_of_offenders - Added row labels")

# 22. Name the columns:

  # For the sig-testing columns, we'll need to attach the correct footnote number:
  name_of_10_year_test_column = paste0(master_config$SIGNIFICANCE_TESTING$COLUMN_LABELS$`10_YEAR_TEST`, "$$Note_",
                                       grep("master_config\\$SIGNIFICANCE_TESTING\\$FOOTNOTE", config$ALL_PARTS$FOOTNOTES), "$$")
  name_of_1_year_test_column  = paste0(master_config$SIGNIFICANCE_TESTING$COLUMN_LABELS$`1_YEAR_TEST`, "$$Note_",
                                       grep("master_config\\$SIGNIFICANCE_TESTING\\$FOOTNOTE", config$ALL_PARTS$FOOTNOTES), "$$")

  colnames(AoO_15) = c(names(years_of_data),
                       name_of_10_year_test_column,
                       name_of_1_year_test_column)

# 23. Return the final data frame:
  return(AoO_15)

}

