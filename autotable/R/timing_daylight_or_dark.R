#' @title timing_daylight_or_dark
#'
#' @description Builds the "Daylight or dark?" part of one subtable of a "Timing of when incidents occurred" table for a
#' Nature of Crime dataset.
#'
#' @details This function builds the "Daylight or dark?" part of one subtable (specified by \code{subtable}) based
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
#' @return Returns a dataframe with the requisite percentages and unweighted bases for the "Daylight or dark?" part of
#' the specified subtable.
#'
#' @examples config = autotable::process_config(yaml::read.yaml("configs/timing/BURGLARY.yaml"))
#' master_config = yaml::read_yaml(file = "configs/MASTER.yaml")
#' timing_daylight_or_dark(config = config, master_config = master_config, subtable = 1)
#'
#' @author Eleanor Scott-Allen, Katie Edser, Matthew Morrison, Nicholas O'Neil; ONS Centre for Crime and Justice, Titchfield, PO15 5RR.
#'
#' @export

timing_daylight_or_dark = function(config, master_config, subtable) {

# 1. Find out which years we are building tables for:
  years_of_data = lapply(config$BOTH_PARTS_OF_THE_TABLE$WEIGHT_VARIABLES, `[[`, 1)

# 2. Get a list of the VF datasets to process:
  list_of_VF_datasets = base::list.files(path = master_config$VF_DATA_DIRECTORY)

# Below, we will use a for-loop to process the years of data one-by-one.

for (i in c(1:length(years_of_data))) {

  # 3. Read in the VF dataset for this year:
    futile.logger::flog.trace("timing_daylight_or_dark - Reading in source data from %s", list_of_VF_datasets[i])
    VF_dataset = base::as.data.frame(data.table::fread(base::paste(master_config$VF_DATA_DIRECTORY, list_of_VF_datasets[i], sep = '/')))

  # 4. Fetch the correct weight variable for this year of data:
    weight_variable = config$BOTH_PARTS_OF_THE_TABLE$WEIGHT_VARIABLES[[i]][[2]]
    futile.logger::flog.trace("timing_daylight_or_dark - Using %s as the weight variable", weight_variable)

  # 5. Remove zero-weighted cases from the dataset:
    DoD_1 = autotable::remove_zeroweighted_rows(df = VF_dataset, weightvar = weight_variable)
    # (Regarding the choice of variable name here: This just stands for "Daylight or Dark", which describes the subtable
    #  component that we want this function to build. Each step in the code will build on the previous one from now
    #  onwards; DoD_1 will become DoD_2, and then DoD_2 will become DoD_3, and so on, until we have the dataframe in
    #  the form that we want)
    futile.logger::flog.trace("timing_daylight_or_dark - Removed %s cases out of %s because their weights were 0",
                             nrow(VF_dataset) - nrow(DoD_1), nrow(VF_dataset))

  # 6. Keep only cases where victarea = 1 (i.e. the incident occurred within 15 minutes of the interview location - typically
  #    the respondent's home) or where wherhapp = 1 (i.e. the incident occurred in England & Wales):
    DoD_2 = dplyr::filter(DoD_1, victarea == 1 | wherhapp == 1)
    futile.logger::flog.trace("timing_daylight_or_dark - Selected %s cases where victarea = 1 or wherapp = 1", nrow(DoD_2))

  # 7. Compute a variable which indicates whether each case is a relevant incident or not, based off its associated offence
  #    code; a value of 1 will indicate that the incident is indeed relevant to the subtable we are building:
    offence_codes = config$BOTH_PARTS_OF_THE_TABLE$OFFENCE_CODES[[subtable]][[2]]
    DoD_3 = dplyr::mutate(DoD_2, relevant_incident = dplyr::if_else(offence %in% offence_codes, 1, NULL))
    futile.logger::flog.trace("timing_daylight_or_dark - Computed a new variable called relevant_incident; a value of 1 for this variable will indicate that the case in question has an offence code relevant to the subtable being built")

  # 8. Select only the cases where relevant_incident = 1:
    DoD_4 = dplyr::filter(DoD_3, relevant_incident == 1)
    futile.logger::flog.trace("timing_daylight_or_dark - Selected %s cases where relevant_incident = 1", nrow(DoD_4))

  # 9. Select only the cases where vftype = 1 (i.e. the incident was recorded on the long Victim Form):
    DoD_5 = dplyr::filter(DoD_4, vftype == 1)
    futile.logger::flog.trace("timing_daylight_or_dark - Selected %s cases where vftype = 1", nrow(DoD_5))

  # 10. If the headline crime type is violence, we may have to apply some extra filters, depending on which subtable is being
  #     built. We'll find out if the headline crime type is violence by searching for "violence" in the Table's title:
    if (grepl("violence", config$BOTH_PARTS_OF_THE_TABLE$TITLE_OF_TABLE, ignore.case = TRUE)) {
      number_of_cases_pre_filter = nrow(DoD_5)
      DoD_5 = autotable::apply_additional_violence_filters(dataframe = DoD_5,
                                                           subtable_name = config$BOTH_PARTS_OF_THE_TABLE$OFFENCE_CODES[[subtable]][[1]])
      futile.logger::flog.trace("timing_daylight_or_dark - Applied additional violence filters, removing a further %s cases",
                                number_of_cases_pre_filter - nrow(DoD_5))
    }

  # 11. We now want to exclude the missing cases:
    DoD_6 = dplyr::filter(DoD_5, !is.na(daylight) & daylight <= 3)

  # 12. Extract the unweighted base:
    unweighted_base = base::sum(nrow(DoD_6))
    futile.logger::flog.trace("timing_daylight_or_dark - Determined the unweighted base as %s", unweighted_base)

  # 13. Weight the data:
    DoD_7 = autotable::apply_weight(DoD_6, "relevant_incident", weight = weight_variable)
    futile.logger::flog.trace("timing_daylight_or_dark - Weighted the data by %s", weight_variable)

  # 14. Group cases by their values for daylight, and then summarise:
    DoD_7$daylight = factor(DoD_7$daylight, levels = c(1:3))
    DoD_8          = dplyr::group_by(DoD_7, daylight, .drop = FALSE)
    DoD_9          = dplyr::summarise_at(DoD_8, "relevant_incident", sum)
    DoD_9$daylight = c(1:3)

    futile.logger::flog.trace("timing_daylight_or_dark - Grouped cases by their values for time1, time2 and time3")

  # 15. Generate the unrounded percentages for this year:
    DoD_10 = (DoD_9$relevant_incident / base::sum(DoD_9$relevant_incident)) * 100
    futile.logger::flog.trace("timing_daylight_or_dark - Generated the unrounded percentages")

  # 16. Generate a name for this column, which we will later apply in Step 21:
    start_year              = substr(years_of_data[[i]], 3, 4)
    end_year                = as.numeric(start_year) + 1
    names(years_of_data)[i] = paste0("Apr '", start_year, " to Mar '", end_year)
    futile.logger::flog.trace("timing_daylight_or_dark - Finished column \"%s\"", names(years_of_data)[i])

  # 17. We now want to add this column into our final dataframe:

    if (i == 1) { # If this is out first iteration through the loop, then we don't have a dataframe to add to, however! So in
                  # that case, we must create one:

      DoD_11 = base::as.data.frame(DoD_10)
      row_of_unweighted_bases = unweighted_base

    } else { # But if it's not our first iteration, then we will just add the column to the dataframe that already exists:

      DoD_11 = base::cbind(DoD_11, base::as.data.frame(DoD_10))
      row_of_unweighted_bases = base::c(row_of_unweighted_bases, unweighted_base)

    }

}

# 17. Bind the row of unweighted bases to the bottom of the table:
  DoD_12 = base::rbind(DoD_11, row_of_unweighted_bases)
  futile.logger::flog.trace("timing_daylight_or_dark - Added the row of unweighted bases")

# 18. Name the rows:
  rownames(DoD_12) = config$PART_THREE$ROW_LABELS
  futile.logger::flog.trace("timing_daylight_or_dark - Added row labels")

# 19. Add significance columns, comparing (A) the latest year against the previous year; and (B) the latest year against
#     the earliest year, for each row:
  DoD_13 = append_significance_column(dataframe = DoD_12, column_1 = ncol(DoD_12), column_2 = ncol(DoD_12) - 1,
                                      row_of_unweighted_bases = nrow(DoD_12))
  futile.logger::flog.trace("timing_daylight_or_dark - Appended a significance column, comparing the latest year of data with the previous year")
  DoD_14 = append_significance_column(dataframe = DoD_13, column_1 = ncol(DoD_13) - 1, column_2 = 1,
                                      row_of_unweighted_bases = nrow(DoD_13))
  futile.logger::flog.trace("timing_daylight_or_dark - Appended a significance column, comparing the latest year of data with the earliest year")

# 21. Name the columns:

  # For the sig-testing columns, we'll need to attach the correct footnote number:
  name_of_10_year_test_column = paste0(master_config$SIGNIFICANCE_TESTING$COLUMN_LABELS$`10_YEAR_TEST`, "$$Note_",
                                       grep("master_config\\$SIGNIFICANCE_TESTING\\$FOOTNOTE", config$BOTH_PARTS_OF_THE_TABLE$FOOTNOTES), "$$")
  name_of_1_year_test_column  = paste0(master_config$SIGNIFICANCE_TESTING$COLUMN_LABELS$`1_YEAR_TEST`, "$$Note_",
                                       grep("master_config\\$SIGNIFICANCE_TESTING\\$FOOTNOTE", config$BOTH_PARTS_OF_THE_TABLE$FOOTNOTES), "$$")

  colnames(DoD_14) = c(names(years_of_data),
                       name_of_10_year_test_column,
                       name_of_1_year_test_column)

  futile.logger::flog.trace("timing_daylight_or_dark - Added column labels")

# 22. Return the final dataframe:
  return(DoD_14)

}
