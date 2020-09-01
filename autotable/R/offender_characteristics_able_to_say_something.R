#' @title offender_characteristics_able_to_say_something
#'
#' @description Builds the "Was the respondent able to say something about the offender(s)?" part of one subtable of an
#' "Offender characteristics" table for a Nature of Crime dataset.
#'
#' @details This function builds the "Was the respondent able to say something about the offender(s)?" part of one subtable
#' (specified by \code{subtable}) based predominantly on two configurations file specified by \code{config} and \code{master_config}.
#' See the User Guide for more details about these two config files.
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
#' @return Returns a dataframe with the requisite percentages and unweighted bases for the "Was the respondent able to say
#' something about the offender(s)?" part of the specified subtable.
#'
#' @examples config = autotable::process_config(yaml::read.yaml("configs/offender_characteristics/ROBBERY.yaml"))
#' master_config = yaml::read_yaml(file = "configs/MASTER.yaml")
#' offender_characteristics_able_to_say_something(config = config, master_config = master_config, subtable = 1)
#'
#' @author Eleanor Scott-Allen, Katie Edser, Matthew Morrison, Nicholas O'Neil; ONS Centre for Crime and Justice, Titchfield, PO15 5RR.
#'
#' @export

offender_characteristics_able_to_say_something = function(config, master_config, subtable) {

# 1. Find out which years we are building tables for:
  years_of_data = lapply(config$ALL_PARTS$WEIGHT_VARIABLES, `[[`, 1)

# 2. Get a list of the VF datasets to process:
  list_of_VF_datasets = base::list.files(path = master_config$VF_DATA_DIRECTORY)

# Below, we will use a for-loop to process the years of data one-by-one.

for (i in c(1:length(years_of_data))) {

  # 3. Read in the VF dataset for this year:
    futile.logger::flog.trace("offender_characteristics_able_to_say_something - Reading in source data from %s", list_of_VF_datasets[i])
    VF_dataset = base::as.data.frame(data.table::fread(base::paste(master_config$VF_DATA_DIRECTORY, list_of_VF_datasets[i], sep = '/')))

  # 4. Fetch the correct weight variable for this year of data:
    weight_variable = config$ALL_PARTS$WEIGHT_VARIABLES[[i]][[2]]
    futile.logger::flog.trace("offender_characteristics_able_to_say_something - Using %s as the weight variable", weight_variable)

  # 5. Remove zero-weighted cases from the dataset:
    AtSS_1 = autotable::remove_zeroweighted_rows(df = VF_dataset, weightvar = weight_variable)
    # (Regarding the choice of variable name here: This just stands for "Able to say something", which describes the
    #  subtable component that we want this function to build. Each step in the code will build on the previous one from now
    #  onwards; AtSS_1 will become AtSS_2, and then AtSS_2 will become AtSS_3, and so on, until we have the dataframe in the form
    #  that we want)
    futile.logger::flog.trace("offender_characteristics_able_to_say_something - Removed %s cases out of %s because their weights were 0",
                              nrow(VF_dataset) - nrow(AtSS_1), nrow(VF_dataset))

  # 6. Keep only cases where victarea = 1 (i.e. the incident occurred within 15 minutes of the interview location - typically
  #    the respondent's home) or where wherhapp = 1 (i.e. the incident occurred in England & Wales):
    AtSS_2 = dplyr::filter(AtSS_1, victarea == 1 | wherhapp == 1)
    futile.logger::flog.trace("offender_characteristics_able_to_say_something - Selected %s cases where victarea = 1 or wherapp = 1",
                              nrow(AtSS_2))

  # 7. Compute a variable which indicates whether each case is a relevant incident or not, based off its associated offence
  #    code; a value of 1 will indicate that the incident is indeed relevant to the subtable we are building:
    offence_codes = config$ALL_PARTS$OFFENCE_CODES[[subtable]][[2]]
    AtSS_3 = dplyr::mutate(AtSS_2, relevant_incident = dplyr::if_else(offence %in% offence_codes, 1, NULL))
    futile.logger::flog.trace("offender_characteristics_able_to_say_something - Computed a new variable called relevant_incident; a value of 1 for this variable will indicate that the case in question has an offence code relevant to the subtable being built")

  # 8. Select only the cases where relevant_incident = 1:
    AtSS_4 = dplyr::filter(AtSS_3, relevant_incident == 1)
    futile.logger::flog.trace("offender_characteristics_able_to_say_something - Selected %s cases where relevant_incident = 1",
                              nrow(AtSS_4))

  # 9. [Note to self: You can delete this step later; I only did this so I wouldn't have to re-number comments and dataframes below, because I'm feeling lazy right now]
    AtSS_5 = AtSS_4

  # 10. Exclude invalid cases based on the 'seeany' variable:
    AtSS_6 = dplyr::filter(AtSS_5, seeany == 1 | seeany == 2)
    AtSS_6 = dplyr::filter(AtSS_5, !is.na(seeany))

  # 11. If the headline crime type is violence, we may have to apply some extra filters, depending on which subtable is being
  #     built. We'll find out if the headline crime type is violence by searching for "violence" in the Table's title:
    if (grepl("violence", config$ALL_PARTS$TITLE_OF_TABLE, ignore.case = TRUE)) {
      number_of_cases_pre_filter = nrow(AtSS_6)
      AtSS_6 = autotable::apply_additional_violence_filters(dataframe = AtSS_6,
                                                            subtable_name = config$ALL_PARTS$OFFENCE_CODES[[subtable]][[1]])
      futile.logger::flog.trace("offender_characteristics_able_to_say_something - Applied additional violence filters, removing a further %s cases",
                                number_of_cases_pre_filter - nrow(AtSS_6))
    }

  # 12. [Note to self: You can delete this step later; I only did this so I wouldn't have to re-number comments and dataframes below, because I'm feeling lazy right now]
    AtSS_7 = AtSS_6

  # 13. Extract the unweighted base:
    unweighted_base = base::sum(AtSS_7$relevant_incident)
    futile.logger::flog.trace("Determined the unweighted base as %s", unweighted_base)

  # 14. Weight the data:
    AtSS_8 = autotable::apply_weight(AtSS_7, "relevant_incident", weight = weight_variable)
    futile.logger::flog.trace("offender_characteristics_able_to_say_something - Weighted the data by %s", weight_variable)

  # 15. Group cases by their values for seeany, and then summarise:
    AtSS_8$seeany  = factor(AtSS_8$seeany, levels = c(1:2))
    AtSS_9         = dplyr::group_by(AtSS_8, seeany, .drop = FALSE)
    AtSS_10        = dplyr::summarise_at(AtSS_9, "relevant_incident", sum)
    AtSS_10$seeany = c(1:2)
    futile.logger::flog.trace("offender_characteristics_able_to_say_something - Grouped cases by their values for seeany")

  # 16. Generate the unrounded percentages for this year:
    AtSS_11 = (AtSS_10$relevant_incident / base::sum(AtSS_10$relevant_incident))[1] * 100
    futile.logger::flog.trace("offender_characteristics_able_to_say_something - Generated the unrounded percentages")

  # 17. Generate a name for this column, which we will later apply in Step 23:
    start_year              = substr(years_of_data[[i]], 3, 4)
    end_year                = as.numeric(start_year) + 1
    names(years_of_data)[i] = paste0("Apr '", start_year, " to Mar '", end_year)
    futile.logger::flog.trace("offender_characteristics_able_to_say_something - Finished column \"%s\"", names(years_of_data)[i])

  # 18. We now want to add this column into our final dataframe:

    if (i == 1) { # If this is out first iteration through the loop, then we don't have a dataframe to add to, however! So in
                  # that case, we must create one:

      AtSS_12 = base::as.data.frame(AtSS_11)
      row_of_unweighted_bases = unweighted_base

    } else { # But if it's not our first iteration, then we will just add the column to the dataframe that already exists:

      AtSS_12 = base::cbind(AtSS_12, base::as.data.frame(AtSS_11))
      row_of_unweighted_bases = base::c(row_of_unweighted_bases, unweighted_base)

    }

}


# 19. Bind the row of unweighted bases to the bottom of the table:
  AtSS_13 = base::rbind(AtSS_12, row_of_unweighted_bases)
  futile.logger::flog.trace("offender_characteristics_able_to_say_something - Added the row of unweighted bases")

# 20. Add significance columns, comparing (A) the latest year against the previous year; and (B) the latest year against
#     the earliest year, for each row:
  AtSS_14 = append_significance_column(dataframe = AtSS_13, column_1 = ncol(AtSS_13), column_2 = 1,
                                      row_of_unweighted_bases = nrow(AtSS_13))
  futile.logger::flog.trace("offender_characteristics_able_to_say_something - Appended a significance column, comparing the latest year of data with the previous year")
  AtSS_15 = append_significance_column(dataframe = AtSS_14, column_1 = ncol(AtSS_14) - 1, ncol(AtSS_14) - 2,
                                      row_of_unweighted_bases = nrow(AtSS_14))
  futile.logger::flog.trace("offender_characteristics_able_to_say_something - Appended a significance column, comparing the latest year of data with the earliest year")

# 21. Name the rows:
  rownames(AtSS_15) = config$PART_ONE$ROW_LABELS
  futile.logger::flog.trace("offender_characteristics_able_to_say_something - Added row labels")

# 22. Name the columns:

  # For the sig-testing columns, we'll need to attach the correct footnote number:
  name_of_10_year_test_column = paste0(master_config$SIGNIFICANCE_TESTING$COLUMN_LABELS$`10_YEAR_TEST`, "$$Note_",
                                       grep("master_config\\$SIGNIFICANCE_TESTING\\$FOOTNOTE", config$ALL_PARTS$FOOTNOTES), "$$")
  name_of_1_year_test_column  = paste0(master_config$SIGNIFICANCE_TESTING$COLUMN_LABELS$`1_YEAR_TEST`, "$$Note_",
                                       grep("master_config\\$SIGNIFICANCE_TESTING\\$FOOTNOTE", config$ALL_PARTS$FOOTNOTES), "$$")

  colnames(AtSS_15) = c(names(years_of_data),
                        name_of_10_year_test_column,
                        name_of_1_year_test_column)

# 23. Return the final data frame:
  return(AtSS_15)

}

